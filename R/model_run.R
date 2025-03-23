#' Run Predictive Model on Prepared Data
#'
#' @description
#' Master function to build, train, and evaluate predictive models.
#'
#' @param data_list A list containing training and test datasets from prepare_model_data()
#' @param outcome_var The name of the outcome variable (default: "DEELNEMER_BC_uitval")
#' @param model_type The type of model to build (default: "random_forest")
#' @param grid_size Number of hyperparameter combinations to try (default: 25)
#' @param n_folds Number of cross-validation folds (default: 5)
#' @param trees Number of trees for ensemble models (default: 1000)
#' @param save Logical indicating whether to save the model (default: TRUE)
#' @param path Optional custom path to save the model
#'
#' @return A list containing the fitted model, metrics, and other results
#'
#' @importFrom parsnip rand_forest boost_tree set_engine set_mode
#' @importFrom workflows workflow add_model add_recipe
#' @importFrom tune tune_grid collect_metrics select_best last_fit
#' @importFrom rsample vfold_cv make_splits
#' @importFrom recipes recipe step_rm step_normalize step_unknown step_zv all_nominal_predictors all_numeric_predictors
#' @importFrom dplyr filter pull bind_rows mutate
#' @importFrom yardstick metric_set roc_auc accuracy
#' @importFrom rlang sym !!
#' @importFrom stats as.formula
#' @importFrom utils head
#'
#' @export
run_model <- function(data_list,
                      outcome_var = "DEELNEMER_BC_uitval",
                      model_type = "random_forest",
                      grid_size = 25,
                      n_folds = 5,
                      trees = 1000,
                      save = TRUE,
                      path = NULL) {

  # Extract data from list
  data_train <- data_list$train
  data_test <- data_list$test
  filter_info <- data_list$filter_info

  # Check if we have both classes in the training data
  if (outcome_var %in% colnames(data_train)) {
    class_counts <- table(data_train[[outcome_var]])
    if (length(class_counts) < 2) {
      stop(paste0("Cannot train a model: training data only contains one class (",
                 paste(names(class_counts), collapse = ", "),
                 "). A predictive model requires examples of both classes."))
    } else {
      # Check if the minimum class count is sufficient
      min_class_count <- min(class_counts)
      if (min_class_count < 5) {  # Using 5 as a minimum threshold
        warning(paste0("Training data contains very few examples of class '",
                      names(which.min(class_counts)), "' (only ", min_class_count,
                      " instances). Model performance may be unreliable."))
      }
    }
  }

  # Create cross-validation folds
  set.seed(0821)
  cv_folds <- create_cv_folds(data_train, outcome_var, n_folds)

  # Create model recipe
  model_recipe <- create_model_recipe(data_train, outcome_var)

  # Create model specification
  model_spec <- create_model_spec(model_type, trees)

  # Create workflow
  model_workflow <- create_model_workflow(model_spec, model_recipe)

  # Tune the model
  tuned_results <- tune_model(model_workflow, cv_folds, grid_size)

  # Get best hyperparameters (optimized for ROC AUC for better ranking)
  best_params <- select_best_params(tuned_results, metric = "roc_auc")

  # Finalize workflow with best parameters
  final_workflow <- finalize_model_workflow(model_workflow, best_params, model_type)

  # Create splits for final evaluation
  combined_data <- bind_rows(data_train, data_test)
  train_indices <- seq_len(nrow(data_train))
  test_indices <- seq_len(nrow(data_test)) + nrow(data_train)

  split_obj <- rsample::initial_split(
      combined_data,
      prop = nrow(data_train) / nrow(combined_data),
      strata = !!sym(outcome_var)
  )

  # Final fit and evaluate with explicit argument names
  final_results <- last_fit(object = final_workflow, split = split_obj)

  # Extract metrics with explicit argument name
  metrics <- collect_metrics(x = final_results)

  # Create results object
  model_results <- list(
    final_model = final_results,
    best_params = best_params,
    tuning_results = tuned_results,
    metrics = metrics,
    workflow = final_workflow,
    filter_info = filter_info
  )

  # Save results if requested
  if (save) {
    save_model_results(model_results, filter_info$program, filter_info$level, path)
  }

  return(model_results)
}

#' Create Cross-Validation Folds for Model Training
#'
#' @description
#' Create stratified cross-validation folds for model tuning.
#'
#' @param data Training data
#' @param outcome_var Name of the outcome variable
#' @param n_folds Number of folds for cross-validation
#'
#' @return A vfold_cv object for use in tuning
#'
#' @importFrom rsample vfold_cv
#'
#' @export
create_cv_folds <- function(data, outcome_var = "DEELNEMER_BC_uitval", n_folds = 5) {
  # Check if we have enough data for stratification
  min_class_size <- min(table(data[[outcome_var]]))

  # Create folds with stratification if possible
  if (min_class_size >= n_folds) {
    # Use explicit argument names to avoid ... issues
    cv_folds <- vfold_cv(data = data, v = n_folds, strata = outcome_var)
    cat("Using", n_folds, "fold stratified cross-validation\n")
  } else {
    # Use explicit argument names to avoid ... issues
    cv_folds <- vfold_cv(data = data, v = n_folds)
    cat("WARNING: Limited data per class, using unstratified", n_folds, "fold cross-validation\n")
  }

  return(cv_folds)
}

#' Create Model Recipe for Predictive Modeling
#'
#' @description
#' Create a recipe for data preprocessing in the modeling pipeline.
#'
#' @param data Training data
#' @param outcome_var Name of the outcome variable
#'
#' @return A recipe object for preprocessing data
#'
#' @importFrom recipes recipe step_rm step_normalize step_unknown step_zv all_nominal_predictors all_numeric_predictors all_predictors
#'
#' @export
create_model_recipe <- function(data, outcome_var = "DEELNEMER_BC_uitval") {
  # Create formula based on outcome variable
  formula_obj <- as.formula(paste(outcome_var, "~ ."))

  # Create and return recipe with explicit argument names
  recipe(formula = formula_obj, data = data) |>
    # Remove identifying variables and dates
    step_rm(matches("_ID$"),
            matches("datum"),
            matches("naam"),
            matches("duur"),
            matches("jaar")) |>
    # Handle unknown levels in categorical variables
    step_unknown(all_nominal_predictors()) |>
    # Remove zero variance predictors before normalization
    # This prevents errors when normalizing variables with no variance
    step_zv(all_predictors()) |>
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors())
}

#' Create Model Specification
#'
#' @description
#' Create a model specification based on the selected model type.
#'
#' @param model_type Type of model to create (e.g., "random_forest", "boost_tree")
#' @param trees Number of trees for ensemble models
#'
#' @return A model specification object
#'
#' @importFrom parsnip rand_forest boost_tree set_engine set_mode decision_tree
#'
#' @export
create_model_spec <- function(model_type = "random_forest", trees = 1000) {
  # Determine number of cores for parallel processing
  cores <- parallel::detectCores()

  # Create model specification based on type
  if (model_type == "random_forest") {
    model_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = trees) |>
      set_engine("ranger", num.threads = cores) |>
      set_mode("classification")
  } else if (model_type == "boost_tree") {
    model_spec <- boost_tree(trees = trees, learn_rate = tune(), min_n = tune()) |>
      set_engine("xgboost") |>
      set_mode("classification")
  } else if (model_type == "decision_tree") {
    model_spec <- decision_tree(cost_complexity = tune(), min_n = tune()) |>
      set_engine("rpart") |>
      set_mode("classification")
  } else {
    stop("Unsupported model type: ", model_type)
  }

  return(model_spec)
}

#' Create Model Workflow
#'
#' @description
#' Create a workflow combining model specification and recipe.
#'
#' @param model_spec Model specification from create_model_spec()
#' @param model_recipe Recipe from create_model_recipe()
#'
#' @return A workflow object
#'
#' @importFrom workflows workflow add_model add_recipe
#'
#' @export
create_model_workflow <- function(model_spec, model_recipe) {
  workflow() |>
    add_model(spec = model_spec) |>
    add_recipe(recipe = model_recipe)
}

#' Tune Model Hyperparameters
#'
#' @description
#' Tune model hyperparameters using cross-validation.
#'
#' @param model_workflow Workflow from create_model_workflow()
#' @param cv_folds Cross-validation folds from create_cv_folds()
#' @param grid_size Number of hyperparameter combinations to try
#'
#' @return A tuning results object
#'
#' @importFrom tune tune_grid control_grid
#' @importFrom yardstick metric_set roc_auc
#'
#' @export
tune_model <- function(model_workflow, cv_folds, grid_size = 25) {
  set.seed(2904)

  # Create the grid control object separately
  ctrl <- control_grid(save_pred = TRUE)

  # Define metrics separately
  metrics <- metric_set(roc_auc, accuracy)

  # Call tune_grid with named arguments only
  tune_grid(
    object = model_workflow,
    resamples = cv_folds,
    grid = grid_size,
    control = ctrl,
    metrics = metrics
  )
}

#' Select Best Parameters from Tuning Results
#'
#' @description
#' Select the best hyperparameters from tuning results.
#'
#' @param tuned_results Tuning results from tune_model()
#' @param metric Metric to optimize, defaults to "roc_auc" which is best for ranking students
#'
#' @return The best hyperparameter values
#'
#' @importFrom tune select_best
#'
#' @export
select_best_params <- function(tuned_results, metric = "roc_auc") {
  select_best(x = tuned_results, metric = metric)
}

#' Finalize Model Workflow with Best Parameters
#'
#' @description
#' Finalize a workflow with the best hyperparameters.
#'
#' @param model_workflow Workflow from create_model_workflow()
#' @param best_params Best parameters from select_best_params()
#' @param model_type Type of model
#'
#' @return A finalized workflow
#'
#' @importFrom workflows update_model
#' @importFrom parsnip rand_forest boost_tree set_engine set_mode
#'
#' @export
finalize_model_workflow <- function(model_workflow, best_params, model_type = "random_forest") {
  # Determine number of cores for parallel processing
  cores <- parallel::detectCores()

  # Create finalized model specification based on type
  if (model_type == "random_forest") {
    final_spec <- rand_forest(
      mtry = best_params$mtry,
      min_n = best_params$min_n,
      trees = 1000
    ) |>
      set_engine("ranger", num.threads = cores, importance = "impurity") |>
      set_mode("classification")
  } else if (model_type == "boost_tree") {
    final_spec <- boost_tree(
      trees = 1000,
      learn_rate = best_params$learn_rate,
      min_n = best_params$min_n
    ) |>
      set_engine("xgboost", importance = TRUE) |>
      set_mode("classification")
  } else if (model_type == "decision_tree") {
    final_spec <- decision_tree(
      cost_complexity = best_params$cost_complexity,
      min_n = best_params$min_n
    ) |>
      set_engine("rpart") |>
      set_mode("classification")
  } else {
    stop("Unsupported model type: ", model_type)
  }

  # Update the workflow with finalized model
  model_workflow |> update_model(final_spec)
}

#' Save Model Results
#'
#' @description
#' Save model results to the appropriate location.
#'
#' @param model_results Model results from run_model()
#' @param program_filter The program filter used (or NULL)
#' @param level_filter The level filter used (or NULL)
#' @param custom_path Optional custom path to save the results
#'
#' @return Invisibly returns the paths where results were saved
#'
#' @importFrom config get
#'
#' @export
save_model_results <- function(model_results, program_filter = NULL, level_filter = NULL, custom_path = NULL) {
  # Create descriptive filename components
  program_str <- if (is.null(program_filter)) "all_programs" else paste0(program_filter, collapse = "_")
  level_str <- if (is.null(level_filter)) "all_levels" else paste0("level", paste0(level_filter, collapse = "_"))

  # Determine base path for saving
  if (is.null(custom_path)) {
    # Check if config exists, otherwise use default path
    if (requireNamespace("config", quietly = TRUE)) {
      modelled_dir <- try(config::get("modelled_dir"), silent = TRUE)
      if (inherits(modelled_dir, "try-error")) {
        modelled_dir <- file.path("data", "modelled")
      }
    } else {
      modelled_dir <- file.path("data", "modelled")
    }

    models_dir <- file.path(modelled_dir, "run", "models")
    metrics_dir <- file.path(modelled_dir, "run", "metrics")
  } else {
    models_dir <- file.path(custom_path, "models")
    metrics_dir <- file.path(custom_path, "metrics")
  }

  # Create directories if they don't exist
  if (!dir.exists(models_dir)) {
    dir.create(models_dir, recursive = TRUE)
  }
  if (!dir.exists(metrics_dir)) {
    dir.create(metrics_dir, recursive = TRUE)
  }

  # Create filenames
  model_filename <- paste0(program_str, "_", level_str, "_model.rds")
  metrics_filename <- paste0(program_str, "_", level_str, "_metrics.rds")

  # Full paths
  model_path <- file.path(models_dir, model_filename)
  metrics_path <- file.path(metrics_dir, metrics_filename)

  # Save model and metrics separately
  saveRDS(model_results$final_model, model_path)
  saveRDS(list(
    metrics = model_results$metrics,
    best_params = model_results$best_params,
    tuning_results = model_results$tuning_results
  ), metrics_path)

  # Inform user
  message("Model saved to: ", model_path)
  message("Metrics saved to: ", metrics_path)

  # Return paths invisibly
  invisible(list(model_path = model_path, metrics_path = metrics_path))
}
