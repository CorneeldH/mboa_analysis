#' Interpret Model Results
#'
#' @description
#' Master function for interpreting predictive model results.
#'
#' @param model_results Model results from run_model()
#' @param n_vars Number of top variables to extract (default: 10)
#' @param save Logical indicating whether to save the interpretation (default: TRUE)
#' @param path Optional custom path to save the interpretation
#'
#' @return A list containing interpretation results
#'
#' @importFrom workflows extract_fit_parsnip
#' @importFrom vip vi
#' @importFrom dplyr arrange desc slice
#'
#' @export
interpret_model <- function(model_results,
                          n_vars = 10,
                          save = TRUE,
                          path = NULL) {

  # Extract filter info
  filter_info <- model_results$filter_info

  # Extract variable importance
  var_importance <- extract_variable_importance(model_results, n_vars)

  # Extract performance metrics
  performance <- model_results$metrics

  # Create interpretation object
  interpretation <- list(
    variable_importance = var_importance,
    performance_metrics = performance,
    filter_info = filter_info,
    interpretation_date = Sys.time()
  )

  # Save interpretation if requested
  if (save) {
    save_model_interpretation(interpretation, filter_info$program, filter_info$level, path)
  }

  return(interpretation)
}

#' Extract Variable Importance from Model
#'
#' @description
#' Extract and format variable importance from a fitted model.
#'
#' @param model_results Model results from run_model()
#' @param n_vars Number of top variables to extract
#'
#' @return A data frame with variable importance scores
#'
#' @importFrom tune extract_fit_parsnip
#' @importFrom vip vi
#' @importFrom dplyr arrange desc slice
#'
#' @export
extract_variable_importance <- function(model_results, n_vars = 10) {
  # Extract the fitted model from the results
  tryCatch({
    # Check if model_results contains a valid model
    if (is.null(model_results$final_model) || !inherits(model_results$final_model, "workflow")) {
      warning("Invalid model object: model_results does not contain a valid fitted model")
      return(data.frame(Variable = character(0), Importance = numeric(0)))
    }

    # Check if the model was actually fit
    fit_results <- try(model_results$final_model[[1]], silent = TRUE)
    if (inherits(fit_results, "try-error") || is.null(fit_results)) {
      warning("Model was not successfully fit")
      return(data.frame(Variable = character(0), Importance = numeric(0)))
    }

    # Extract the fitted model
    model_fit <- extract_fit_parsnip(model_results$final_model)

    # Check if variable importance can be calculated
    if (is.null(model_fit$fit) || !inherits(model_fit$fit, "ranger")) {
      warning("Model does not support variable importance extraction")
      return(data.frame(Variable = character(0), Importance = numeric(0)))
    }

    # Extract variable importance
    var_importance <- vi(model_fit)

    # Check if we got any results
    if (is.null(var_importance) || nrow(var_importance) == 0) {
      warning("No variable importance data available")
      return(data.frame(Variable = character(0), Importance = numeric(0)))
    }

    # Sort and limit results
    var_importance <- var_importance |>
      arrange(desc(Importance))

    if (nrow(var_importance) > 0) {
      var_importance <- var_importance |>
        slice(1:min(n_vars, nrow(var_importance)))
    }

    return(var_importance)
  }, error = function(e) {
    warning("Could not extract variable importance: ", e$message)
    return(data.frame(Variable = character(0), Importance = numeric(0)))
  })
}

#' Calculate Baseline Model Accuracy
#'
#' @description
#' Calculate the accuracy of predicting the most common class (baseline model).
#'
#' @param data Test data used for evaluation
#' @param outcome_var Name of the outcome variable
#'
#' @return A list with baseline accuracy and most common class
#'
#' @importFrom dplyr count mutate filter pull
#' @importFrom rlang sym !!
#'
#' @export
calculate_baseline_accuracy <- function(data, outcome_var = "DEELNEMER_BC_uitval") {
  # Calculate class distribution
  class_distribution <- data |>
    count(!!sym(outcome_var)) |>
    mutate(pct = n/sum(n))

  # Find most common class
  most_common_class <- class_distribution |>
    filter(pct == max(pct)) |>
    pull(!!sym(outcome_var))

  # Get baseline accuracy
  baseline_accuracy <- class_distribution |>
    filter(!!sym(outcome_var) == most_common_class) |>
    pull(pct)

  return(list(
    baseline_accuracy = baseline_accuracy,
    most_common_class = most_common_class
  ))
}

#' Calculate Model Lift Over Baseline
#'
#' @description
#' Calculate how much better the model performs compared to the baseline.
#'
#' @param model_accuracy Model accuracy value
#' @param baseline_accuracy Baseline accuracy value
#'
#' @return A list with lift values and descriptive text
#'
#' @export
calculate_model_lift <- function(model_accuracy, baseline_accuracy) {
  # Calculate absolute and relative lift
  accuracy_lift <- model_accuracy - baseline_accuracy
  accuracy_lift_pct <- (model_accuracy / baseline_accuracy - 1) * 100

  # Generate descriptive text
  if (accuracy_lift <= 0) {
    comparison_text <- "not better than"
    level_text <- "insufficient"
  } else if (accuracy_lift < 0.05) {
    comparison_text <- "slightly better than"
    level_text <- "minimal"
  } else if (accuracy_lift < 0.1) {
    comparison_text <- "better than"
    level_text <- "moderate"
  } else if (accuracy_lift < 0.2) {
    comparison_text <- "substantially better than"
    level_text <- "substantial"
  } else {
    comparison_text <- "far better than"
    level_text <- "exceptional"
  }

  return(list(
    lift = accuracy_lift,
    lift_pct = accuracy_lift_pct,
    comparison = comparison_text,
    level = level_text
  ))
}

#' Save Model Interpretation
#'
#' @description
#' Save model interpretation results to the appropriate location.
#'
#' @param interpretation Interpretation results from interpret_model()
#' @param program_filter The program filter used (or NULL)
#' @param level_filter The level filter used (or NULL)
#' @param custom_path Optional custom path to save the interpretation
#'
#' @return Invisibly returns the path where the interpretation was saved
#'
#' @importFrom config get
#'
#' @export
save_model_interpretation <- function(interpretation, program_filter = NULL, level_filter = NULL, custom_path = NULL) {
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

    importance_dir <- file.path(modelled_dir, "interpreted", "importance")
  } else {
    importance_dir <- file.path(custom_path, "importance")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(importance_dir)) {
    dir.create(importance_dir, recursive = TRUE)
  }

  # Create filename
  importance_filename <- paste0(program_str, "_", level_str, "_importance.rds")

  # Full path
  importance_path <- file.path(importance_dir, importance_filename)

  # Save interpretation
  saveRDS(interpretation, importance_path)

  # Inform user
  message("Interpretation saved to: ", importance_path)

  # Return path invisibly
  invisible(importance_path)
}

#' Create Visualization of Variable Importance
#'
#' @description
#' Create a standardized ggplot of variable importance.
#'
#' @param var_importance Variable importance data frame from extract_variable_importance()
#' @param title Optional title for the plot
#' @param color Optional color for the bars
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal geom_vline ylim
#' @importFrom stats reorder
#'
#' @export
create_importance_plot <- function(var_importance, title = NULL, color = "steelblue") {
  # Set default title if not provided
  if (is.null(title)) {
    title <- "Variable Importance"
  }

  # Create the plot
  ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = color) +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = "Importance"
    ) +
    theme_minimal()
}

#' Compare Variable Importance Across Groups
#'
#' @description
#' Compare variable importance across different program/level groups.
#'
#' @param interpretation_list A list of interpretation results from interpret_model()
#' @param n_vars Number of top variables to include (default: 10)
#' @param save Logical indicating whether to save the comparison (default: TRUE)
#' @param path Optional custom path to save the comparison
#'
#' @return A data frame with importance comparisons across groups
#'
#' @importFrom dplyr bind_rows mutate group_by summarize arrange desc
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @export
compare_group_importance <- function(interpretation_list,
                                  n_vars = 10,
                                  save = TRUE,
                                  path = NULL) {
  # Extract variable importance for each group
  group_importance <- list()

  for (group_name in names(interpretation_list)) {
    interp <- interpretation_list[[group_name]]

    if (!is.null(interp$variable_importance) && nrow(interp$variable_importance) > 0) {
      group_importance[[group_name]] <- interp$variable_importance |>
        mutate(group = group_name)
    }
  }

  # Combine all groups
  combined_importance <- bind_rows(group_importance)

  if (nrow(combined_importance) == 0) {
    warning("No variable importance data available for comparison")
    return(data.frame())
  }

  # Find overall top variables across groups
  top_vars <- combined_importance |>
    group_by(Variable) |>
    summarize(avg_importance = mean(Importance, na.rm = TRUE)) |>
    arrange(desc(avg_importance)) |>
    head(n_vars) |>
    pull(Variable)

  # Filter to only include top variables
  comparison <- combined_importance |>
    filter(Variable %in% top_vars)

  # Create wide format for easier comparison
  comparison_wide <- comparison |>
    select(group, Variable, Importance) |>
    pivot_wider(
      names_from = group,
      values_from = Importance
    )

  # Save comparison if requested
  if (save) {
    # Determine base path for saving
    if (is.null(path)) {
      # Check if config exists, otherwise use default path
      if (requireNamespace("config", quietly = TRUE)) {
        modelled_dir <- try(config::get("modelled_dir"), silent = TRUE)
        if (inherits(modelled_dir, "try-error")) {
          modelled_dir <- file.path("data", "modelled")
        }
      } else {
        modelled_dir <- file.path("data", "modelled")
      }

      vis_dir <- file.path(modelled_dir, "interpreted", "visualizations")
    } else {
      vis_dir <- file.path(path, "visualizations")
    }

    # Create directory if it doesn't exist
    if (!dir.exists(vis_dir)) {
      dir.create(vis_dir, recursive = TRUE)
    }

    # Save comparison
    saveRDS(comparison_wide, file.path(vis_dir, "importance_comparison.rds"))
    message("Importance comparison saved to: ", file.path(vis_dir, "importance_comparison.rds"))
  }

  return(comparison_wide)
}

#' Compare Model Performance Across Groups
#'
#' @description
#' Compare model performance metrics across different program/level groups.
#'
#' @param interpretation_list A list of interpretation results from interpret_model()
#' @param metric Performance metric to compare (default: "accuracy")
#' @param save Logical indicating whether to save the comparison (default: TRUE)
#' @param path Optional custom path to save the comparison
#'
#' @return A data frame with performance comparisons across groups
#'
#' @importFrom dplyr bind_rows mutate filter arrange desc
#'
#' @export
compare_group_performance <- function(interpretation_list,
                                   metric = "accuracy",
                                   save = TRUE,
                                   path = NULL) {
  # Extract performance metrics for each group
  group_performance <- list()

  for (group_name in names(interpretation_list)) {
    interp <- interpretation_list[[group_name]]

    if (!is.null(interp$performance_metrics) && nrow(interp$performance_metrics) > 0) {
      metrics <- interp$performance_metrics |>
        filter(.metric == metric) |>
        mutate(group = group_name)

      group_performance[[group_name]] <- metrics
    }
  }

  # Combine all groups
  comparison <- bind_rows(group_performance) |>
    arrange(desc(.estimate))

  if (nrow(comparison) == 0) {
    warning("No performance data available for comparison")
    return(data.frame())
  }

  # Save comparison if requested
  if (save) {
    # Determine base path for saving
    if (is.null(path)) {
      # Check if config exists, otherwise use default path
      if (requireNamespace("config", quietly = TRUE)) {
        modelled_dir <- try(config::get("modelled_dir"), silent = TRUE)
        if (inherits(modelled_dir, "try-error")) {
          modelled_dir <- file.path("data", "modelled")
        }
      } else {
        modelled_dir <- file.path("data", "modelled")
      }

      vis_dir <- file.path(modelled_dir, "interpreted", "visualizations")
    } else {
      vis_dir <- file.path(path, "visualizations")
    }

    # Create directory if it doesn't exist
    if (!dir.exists(vis_dir)) {
      dir.create(vis_dir, recursive = TRUE)
    }

    # Save comparison
    saveRDS(comparison, file.path(vis_dir, "performance_comparison.rds"))
    message("Performance comparison saved to: ", file.path(vis_dir, "performance_comparison.rds"))
  }

  return(comparison)
}

#' Create Performance Comparison Plot
#'
#' @description
#' Create a standardized ggplot of performance comparison across groups.
#'
#' @param performance_comparison Performance comparison data frame
#' @param metric Performance metric being compared (for labeling)
#' @param title Optional title for the plot
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text position_stack labs theme_minimal
#' @importFrom scales percent
#'
#' @export
create_performance_plot <- function(performance_comparison, metric = "accuracy", title = NULL) {
  # Set default title if not provided
  if (is.null(title)) {
    title <- paste("Model", toupper(substr(metric, 1, 1)), substr(metric, 2, nchar(metric)), "Comparison")
  }

  # Calculate baseline performance if available
  has_baseline <- "baseline" %in% colnames(performance_comparison)

  # Create the plot
  p <- ggplot(performance_comparison, aes(x = reorder(group, .estimate), y = .estimate)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = scales::percent(.estimate, accuracy = 0.1)),
              hjust = -0.2) +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = toupper(substr(metric, 1, 1)) %+% substr(metric, 2, nchar(metric))
    ) +
    theme_minimal() +
    ylim(0, max(1, max(performance_comparison$.estimate) * 1.2))

  # Add baseline if available
  if (has_baseline) {
    p <- p +
      geom_vline(aes(xintercept = baseline), linetype = "dashed", color = "darkred")
  }

  return(p)
}

#' Interpret Results for Multiple Groups
#'
#' @description
#' Run interpretation for multiple model results.
#'
#' @param model_results_list A list of model results from run_model() or run_group_models()
#' @param n_vars Number of top variables to extract (default: 10)
#' @param save Logical indicating whether to save the interpretations (default: TRUE)
#'
#' @return A list of interpretation results for each group
#'
#' @export
interpret_group_models <- function(model_results_list, n_vars = 10, save = TRUE) {
  # Initialize results list
  interpretation_list <- list()

  # Interpret each group's model
  for (group_name in names(model_results_list)) {
    message("Interpreting model for group: ", group_name)

    # Run interpretation for this group
    interpretation_list[[group_name]] <- interpret_model(
      model_results = model_results_list[[group_name]],
      n_vars = n_vars,
      save = save
    )
  }

  # Run comparisons across groups
  if (length(interpretation_list) > 1) {
    message("Comparing models across groups...")

    # Compare variable importance
    importance_comparison <- compare_group_importance(
      interpretation_list = interpretation_list,
      n_vars = n_vars,
      save = save
    )

    # Compare model performance
    performance_comparison <- compare_group_performance(
      interpretation_list = interpretation_list,
      save = save
    )

    # Add comparisons to the results
    interpretation_list$comparisons <- list(
      importance = importance_comparison,
      performance = performance_comparison
    )
  }

  return(interpretation_list)
}
