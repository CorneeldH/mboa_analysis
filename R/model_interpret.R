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
    if (is.null(model_results$final_model) || !inherits(model_results$final_model, "last_fit")) {
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
#' For ROC AUC, the baseline is always 0.5 (random classifier).
#'
#' @param model_metric Model performance metric value (ROC AUC or accuracy)
#' @param baseline_metric Baseline metric value (0.5 for ROC AUC)
#' @param metric_type Type of metric being used (default: "roc_auc")
#'
#' @return A list with lift values and descriptive text
#'
#' @export
calculate_model_lift <- function(model_metric, baseline_metric, metric_type = "roc_auc") {
  # Calculate absolute and relative lift
  metric_lift <- model_metric - baseline_metric
  metric_lift_pct <- (model_metric / baseline_metric - 1) * 100

  # Different thresholds based on metric type
  if (metric_type == "roc_auc") {
    # For ROC AUC
    if (metric_lift <= 0) {
      comparison_text <- "not better than"
      level_text <- "insufficient"
    } else if (metric_lift < 0.1) {
      comparison_text <- "slightly better than"
      level_text <- "minimal"
    } else if (metric_lift < 0.2) {
      comparison_text <- "better than"
      level_text <- "moderate"
    } else if (metric_lift < 0.3) {
      comparison_text <- "substantially better than"
      level_text <- "substantial"
    } else {
      comparison_text <- "far better than"
      level_text <- "exceptional"
    }
  } else {
    # For accuracy or other metrics
    if (metric_lift <= 0) {
      comparison_text <- "not better than"
      level_text <- "insufficient"
    } else if (metric_lift < 0.05) {
      comparison_text <- "slightly better than"
      level_text <- "minimal"
    } else if (metric_lift < 0.1) {
      comparison_text <- "better than"
      level_text <- "moderate"
    } else if (metric_lift < 0.2) {
      comparison_text <- "substantially better than"
      level_text <- "substantial"
    } else {
      comparison_text <- "far better than"
      level_text <- "exceptional"
    }
  }

  return(list(
    lift = metric_lift,
    lift_pct = metric_lift_pct,
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
#' @param use_friendly_names Logical indicating whether to use user-friendly variable names (default: TRUE)
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal geom_vline ylim
#' @importFrom stats reorder
#'
#' @export
create_importance_plot <- function(var_importance, title = NULL, color = "steelblue", use_friendly_names = TRUE) {
  # Set default title if not provided
  if (is.null(title)) {
    title <- "Variable Importance"
  }

  # Convert Variable column to data frame format for replacement if necessary
  if (use_friendly_names) {
    # Create temporary data frame with Variable column
    temp_df <- data.frame(Variable = var_importance$Variable)
    # Replace with friendly names
    temp_df <- replace_with_friendly_names(temp_df)
    # Update original data frame
    var_importance$Variable <- temp_df$Variable
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
#' @param use_friendly_names Logical indicating whether to use user-friendly variable names (default: TRUE)
#'
#' @return A data frame with importance comparisons across groups
#'
#' @importFrom dplyr bind_rows mutate group_by summarize arrange desc
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats setNames
#'
#' @export
compare_group_importance <- function(interpretation_list,
                                  n_vars = 10,
                                  save = TRUE,
                                  path = NULL,
                                  use_friendly_names = TRUE) {
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

  # Apply friendly variable names if requested
  if (use_friendly_names) {
    # Get unique variable names
    unique_vars <- unique(comparison$Variable)

    # Create mapping table
    var_mapping <- data.frame(
      original = unique_vars,
      stringsAsFactors = FALSE
    )

    # Apply replacement
    var_mapping_friendly <- replace_with_friendly_names(var_mapping)

    # Create a mapping vector
    name_map <- setNames(var_mapping_friendly$original, var_mapping$original)

    # Replace variable names
    comparison$Variable <- name_map[comparison$Variable]
  }

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

#' Compare Performance by Week Strategy
#'
#' @description
#' Compare model performance between different week variable strategies for the same program-level combinations.
#' Focuses on ROC AUC for better student ranking capability.
#'
#' @param performance_comparison Performance comparison data frame from compare_group_performance()
#' @param save Logical indicating whether to save the comparison (default: TRUE)
#' @param path Optional custom path to save the comparison
#'
#' @return A data frame with performance comparisons by week strategy
#'
#' @importFrom dplyr group_by summarize arrange desc mutate
#' @importFrom tidyr pivot_wider
#'
#' @export
compare_week_strategies <- function(performance_comparison, save = TRUE, path = NULL) {
  # Group by program_level and compute performance for each week strategy
  # Using ROC AUC for better ranking assessment
  week_comparison <- performance_comparison |>
    filter(.metric == "roc_auc") |>  # Using ROC AUC instead of accuracy
    group_by(program_level, week_strategy) |>
    summarize(
      roc_auc = mean(.estimate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Pivot to wide format for easy comparison
    pivot_wider(
      names_from = week_strategy,
      values_from = roc_auc
    ) |>
    # Calculate differences between strategies
    mutate(
      # Positive values indicate second strategy is better
      early_vs_none = weeks_early - weeks_none,  # Higher = better student ranking
      all_vs_none = weeks_all - weeks_none,
      all_vs_early = weeks_all - weeks_early
    ) |>
    arrange(desc(weeks_all))

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
    saveRDS(week_comparison, file.path(vis_dir, "week_strategy_comparison.rds"))
    message("Week strategy comparison saved to: ", file.path(vis_dir, "week_strategy_comparison.rds"))
  }

  return(week_comparison)
}

#' Compare Model Performance Across Groups
#'
#' @description
#' Compare model performance metrics across different program/level groups.
#'
#' @param interpretation_list A list of interpretation results from interpret_model()
#' @param metric Performance metric to compare (default: "roc_auc")
#' @param save Logical indicating whether to save the comparison (default: TRUE)
#' @param path Optional custom path to save the comparison
#'
#' @return A data frame with performance comparisons across groups
#'
#' @importFrom dplyr bind_rows mutate filter arrange desc
#' @importFrom stringr str_extract
#'
#' @export
compare_group_performance <- function(interpretation_list,
                                   metric = "roc_auc",
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

  # Combine all groups and extract metadata
  comparison <- bind_rows(group_performance) |>
    mutate(
      # Extract program-level and week strategy information from group name
      program_level = str_extract(group, "^[^_weeks]+"),
      week_strategy = str_extract(group, "weeks_[^_]+$")
    ) |>
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
#' @importFrom ggplot2 ggplot aes geom_col geom_text geom_hline position_stack labs theme_minimal
#' @importFrom scales percent
#'
#' @export
create_performance_plot <- function(performance_comparison, metric = "roc_auc", title = NULL) {
  # Set default title if not provided
  if (is.null(title)) {
    # Special case for ROC AUC
    if (metric == "roc_auc") {
      title <- "Model ROC AUC Comparison"
    } else {
      title <- paste("Model", toupper(substr(metric, 1, 1)), substr(metric, 2, nchar(metric)), "Comparison")
    }
  }

  # Calculate baseline performance if available
  has_baseline <- "baseline" %in% colnames(performance_comparison)

  # If there's no baseline but we're using ROC AUC, add a reference line at 0.5
  add_roc_baseline <- (metric == "roc_auc" && !has_baseline)

  # Create the plot
  p <- ggplot(performance_comparison, aes(x = reorder(group, .estimate), y = .estimate)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = scales::percent(.estimate, accuracy = 0.1)),
              hjust = -0.2) +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = if(metric == "roc_auc") "ROC AUC" else toupper(substr(metric, 1, 1)) %+% substr(metric, 2, nchar(metric)),
      subtitle = if(metric == "roc_auc") "Higher values indicate better student ranking capability" else NULL
    ) +
    theme_minimal() +
    ylim(0, max(1, max(performance_comparison$.estimate) * 1.2))

  # Add baseline if available
  if (has_baseline) {
    p <- p +
      geom_vline(aes(xintercept = baseline), linetype = "dashed", color = "darkred")
  } else if (add_roc_baseline) {
    # For ROC AUC, add reference line at 0.5 (random model)
    p <- p +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkred")
  }

  return(p)
}

#' Create Week Strategy Comparison Plot
#'
#' @description
#' Create a standardized ggplot comparing performance of different week strategies.
#'
#' @param week_comparison Week strategy comparison data frame from compare_week_strategies()
#' @param title Optional title for the plot
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point position_dodge theme_minimal labs scale_y_continuous
#' @importFrom scales percent
#'
#' @export
create_week_strategy_plot <- function(week_comparison, title = NULL) {
  # Set default title if not provided
  if (is.null(title)) {
    title <- "Comparison of Week Variable Strategies"
  }

  # Reshape data for plotting
  plot_data <- week_comparison |>
    pivot_longer(
      cols = c(weeks_none, weeks_early, weeks_all),
      names_to = "strategy",
      values_to = "roc_auc"
    ) |>
    mutate(
      strategy = factor(strategy,
                        levels = c("weeks_none", "weeks_early", "weeks_all"),
                        labels = c("No Weeks", "Early Weeks (1-5)", "All Weeks"))
    )

  # Create plot
  p <- ggplot(plot_data, aes(x = reorder(program_level, roc_auc), y = roc_auc, color = strategy, group = strategy)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_segment(aes(y = 0, yend = roc_auc, xend = reorder(program_level, roc_auc)),
                position = position_dodge(width = 0.5), alpha = 0.3, linewidth = 0.5) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = title,
      x = NULL,
      y = "ROC AUC",
      color = "Week Strategy"
    ) +
    theme_minimal()

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
#' @param use_friendly_names Logical indicating whether to use user-friendly variable names (default: TRUE)
#'
#' @return A list of interpretation results for each group
#'
#' @export
interpret_group_models <- function(model_results_list, n_vars = 10, save = TRUE, use_friendly_names = TRUE) {
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

    # Apply friendly variable names if requested
    if (use_friendly_names && !is.null(interpretation_list[[group_name]]$variable_importance)) {
      # Create temporary data frame with Variable column
      temp_df <- data.frame(Variable = interpretation_list[[group_name]]$variable_importance$Variable)
      # Replace with friendly names
      temp_df <- replace_with_friendly_names(temp_df)
      # Update original data frame
      interpretation_list[[group_name]]$variable_importance$Variable <- temp_df$Variable
    }
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

    # Apply friendly variable names to the importance comparison if requested
    if (use_friendly_names && !is.null(importance_comparison) && nrow(importance_comparison) > 0) {
      # Create temporary data frame with Variable column
      temp_df <- data.frame(Variable = importance_comparison$Variable)
      # Replace with friendly names
      temp_df <- replace_with_friendly_names(temp_df)
      # Update original data frame
      importance_comparison$Variable <- temp_df$Variable
    }

    # Compare model performance
    performance_comparison <- compare_group_performance(
      interpretation_list = interpretation_list,
      save = save
    )

    # Compare week strategies if we have different week strategies
    if (any(grepl("weeks_", names(interpretation_list)))) {
      message("Comparing different week variable strategies...")

      # Compare week strategies
      week_comparison <- compare_week_strategies(
        performance_comparison = performance_comparison,
        save = save
      )

      # Add week comparison to results
      interpretation_list$comparisons <- list(
        importance = importance_comparison,
        performance = performance_comparison,
        week_strategies = week_comparison
      )
    } else {
      # Add regular comparisons to the results
      interpretation_list$comparisons <- list(
        importance = importance_comparison,
        performance = performance_comparison
      )
    }
  }

  return(interpretation_list)
}


#' Create Intervention Lift Chart
#'
#' @description
#' Create a chart showing the percentage of dropouts captured at different intervention thresholds.
#' This helps determine how many students need intervention to catch a given percentage of dropouts.
#'
#' @param model_results Model results from run_model()
#' @param title Optional title for the plot
#' @param save Logical indicating whether to save the plot (default: TRUE)
#' @param path Optional custom path to save the plot
#' @param filename Optional custom filename for the saved plot (without extension)
#' @param program_id Optional program identifier for unique filename generation
#' @param week_strategy Optional week strategy for unique filename generation
#'
#' @return A ggplot object showing the lift chart or NULL if unable to extract predictions
#'
#' @importFrom tune collect_predictions
#' @importFrom dplyr arrange mutate select bind_rows bind_cols filter left_join group_by summarize
#' @importFrom ggplot2 ggplot aes geom_line geom_area geom_abline labs theme_minimal
#' @importFrom scales percent
#' @importFrom stats predict
#'
#' @export
create_lift_chart <- function(model_results, title = NULL, save = TRUE, path = NULL,
                             filename = NULL, program_id = NULL, week_strategy = NULL) {
  # Set default title
  if (is.null(title)) {
    title <- "Intervention Efficiency: Dropout Capture Rate vs. Students Targeted"
  }

  # Extract predictions
  predictions <- tryCatch({
    # Try collect_predictions first
    preds <- collect_predictions(model_results$final_model)

    # If empty, try alternative locations
    if (nrow(preds) == 0) {
      if ("predictions" %in% names(model_results)) {
        preds <- model_results$predictions
      } else if (!is.null(model_results$final_model) && ".predictions" %in% names(model_results$final_model)) {
        preds <- model_results$final_model$.predictions[[1]]
      } else if (!is.null(model_results$workflow) && !is.null(model_results$test_data)) {
        preds <- predict(model_results$workflow, model_results$test_data, type = "prob") |>
          bind_cols(model_results$test_data |> select(matches("DEELNEMER_BC_uitval")))
      }
    }
    preds
  }, error = function(e) {
    return(NULL)
  })

  # Check if we have predictions
  if (is.null(predictions) || nrow(predictions) == 0) {
    return(NULL)
  }

  # Find probability column
  dropout_prob_col <- grep("^.pred_", colnames(predictions), value = TRUE)[1]
  if (is.na(dropout_prob_col)) {
    return(NULL)
  }

  # Find outcome column
  outcome_col <- intersect(
    colnames(predictions),
    c(".outcome", "DEELNEMER_BC_uitval", "truth", "actual", "y")
  )[1]
  if (is.na(outcome_col)) {
    return(NULL)
  }

  # Determine positive class values
  unique_outcomes <- unique(predictions[[outcome_col]])
  pos_values <- c("Uitval", "1", 1, TRUE)
  positive_class <- pos_values[which(pos_values %in% unique_outcomes)[1]]

  # If can't identify positive class, use least frequent class
  if (is.na(positive_class)) {
    outcome_counts <- table(predictions[[outcome_col]])
    positive_class <- names(outcome_counts)[which.min(outcome_counts)]
  }

  # Prepare lift chart data
  lift_data <- predictions |>
    arrange(desc(!!sym(dropout_prob_col))) |>
    mutate(
      actual_dropout = as.numeric(as.character(!!sym(outcome_col)) == as.character(positive_class)),
      rank = row_number(),
      rank_pct = rank / n()
    )

  # Calculate cumulative metrics
  if (sum(lift_data$actual_dropout) > 0) {
    lift_data <- lift_data |>
      mutate(
        cum_dropouts = cumsum(actual_dropout),
        cum_dropout_pct = cum_dropouts / sum(actual_dropout)
      ) |>
      select(rank_pct, cum_dropout_pct)
  } else {
    return(NULL)
  }

  # Add reference points
  reference_points <- data.frame(
    rank_pct = seq(0, 1, by = 0.1),
    model = "reference"
  ) |>
    left_join(
      lift_data |>
        filter(rank_pct >= 0) |>
        group_by(rank_pct = floor(rank_pct * 10) / 10) |>
        summarize(cum_dropout_pct = max(cum_dropout_pct), .groups = "drop")
    )

  # Create plot
  p <- ggplot(lift_data, aes(x = rank_pct, y = cum_dropout_pct)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
    geom_area(fill = "steelblue", alpha = 0.3) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(data = reference_points, size = 3, color = "darkred") +
    labs(
      title = title,
      subtitle = "Shows % of dropouts captured when intervening with % of highest-risk students",
      x = "Percentage of Students Targeted (Highest Risk First)",
      y = "Percentage of Dropouts Captured"
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()

  # Save if requested
  if (save) {
    # Get save directory
    if (is.null(path)) {
      modelled_dir <- tryCatch(
        config::get("modelled_dir"),
        error = function(e) file.path("data", "modelled")
      )
      vis_dir <- file.path(modelled_dir, "interpreted", "visualizations")
    } else {
      vis_dir <- path
    }

    # Create directory if needed
    if (!dir.exists(vis_dir)) {
      dir.create(vis_dir, recursive = TRUE)
    }

    # Generate filename
    if (is.null(filename)) {
      # Auto-extract program and strategy if not provided
      if (is.null(program_id) && !is.null(model_results$filter_info)) {
        program_id <- model_results$filter_info$program
      }
      if (is.null(week_strategy) && !is.null(model_results$filter_info)) {
        week_strategy <- model_results$filter_info$week_vars
      }

      # Build filename with available info
      parts <- c("lift_chart")
      if (!is.null(program_id)) {
        parts <- c(parts, gsub("[^a-zA-Z0-9]", "_", program_id))
      }
      if (!is.null(week_strategy)) {
        parts <- c(parts, week_strategy)
      }
      if (length(parts) == 1) {
        parts <- c(parts, format(Sys.time(), "%Y%m%d_%H%M%S"))
      }

      output_filename <- paste0(paste(parts, collapse = "_"), ".png")
    } else {
      output_filename <- paste0(filename, ".png")
    }

    # Save plot
    ggsave(
      file.path(vis_dir, output_filename),
      p,
      width = 10,
      height = 7
    )
  }

  return(p)
}

#' Create Lift Charts for All Week Strategies
#'
#' @description
#' Create lift charts for each week strategy to compare intervention efficiency.
#' This function expects a named list of model results, with week strategy names as keys.
#'
#' @param models_by_strategy A named list of model results, with strategy names ("none", "early", "all") as keys
#' @param save Logical indicating whether to save the plots (default: TRUE)
#' @param path Optional custom path to save the plots
#' @param program_filter Optional program filter for filename generation
#'
#' @return A list of ggplot objects
#'
#' @importFrom ggplot2 ggsave
#'
#' @export
create_all_strategy_lift_charts <- function(models_by_strategy, save = TRUE, path = NULL, program_filter = NULL) {
  # Initialize results and create individual charts
  lift_charts <- list()

  # Process each strategy
  for (strategy in names(models_by_strategy)) {
    title <- paste0("Intervention Efficiency: ", toupper(substr(strategy, 1, 1)),
                   substr(strategy, 2, nchar(strategy)), " Week Strategy")

    # Create individual chart
    chart <- tryCatch({
      create_lift_chart(
        model_results = models_by_strategy[[strategy]],
        title = title,
        save = save,
        path = path,
        filename = paste0("lift_chart_strategy_", strategy),
        program_id = program_filter,
        week_strategy = strategy
      )
    }, error = function(e) NULL)

    if (!is.null(chart)) {
      lift_charts[[strategy]] <- chart
    }
  }

  # Create comparison chart if we have multiple strategies
  if (length(lift_charts) >= 2) {
    # Extract and combine data
    comparison_data <- data.frame()

    for (strategy in names(lift_charts)) {
      if (strategy != "comparison" && !is.null(lift_charts[[strategy]]$data)) {
        strategy_data <- lift_charts[[strategy]]$data |>
          mutate(strategy = factor(strategy,
                                 levels = c("none", "early", "all"),
                                 labels = c("Bij start", "Na 5 weken", "Na 10 weken")))
        comparison_data <- bind_rows(comparison_data, strategy_data)
      }
    }

    # Create comparison plot if we have data
    if (nrow(comparison_data) > 0) {
      combined_plot <- ggplot(comparison_data, aes(x = rank_pct, y = cum_dropout_pct, color = strategy)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
        geom_line(linewidth = 1.2) +
        geom_point(aes(shape = strategy), size = 3) +
        labs(
          title = "Intervention Efficiency Comparison Across Week Strategies",
          subtitle = "Shows % of dropouts captured when intervening with % of highest-risk students",
          x = "Percentage of Students Targeted (Highest Risk First)",
          y = "Percentage of Dropouts Captured",
          color = "Week Strategy",
          shape = "Week Strategy"
        ) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(legend.position = "bottom")

      # Save comparison plot
      if (save) {
        # Get path and create directory if needed
        vis_dir <- if (is.null(path)) {
          modelled_dir <- tryCatch(
            config::get("modelled_dir"),
            error = function(e) file.path("data", "modelled")
          )
          file.path(modelled_dir, "interpreted", "visualizations")
        } else {
          path
        }

        if (!dir.exists(vis_dir)) {
          dir.create(vis_dir, recursive = TRUE)
        }

        # Create filename
        filename <- "lift_chart_strategy_comparison"
        if (!is.null(program_filter)) {
          filename <- paste0(filename, "_", gsub("[^a-zA-Z0-9]", "_", program_filter))
        }

        # Save plot
        ggsave(
          file.path(vis_dir, paste0(filename, ".png")),
          combined_plot,
          width = 10,
          height = 7
        )
      }

      # Add to results
      lift_charts$comparison <- combined_plot
    }
  }

  return(lift_charts)
}

#' Create Lift Charts for Top Programs
#'
#' @description
#' Create lift charts for the top-performing program-level combinations to show
#' intervention efficiency for specific programs.
#'
#' @param top_model_results List of model results for top-performing programs
#' @param performance_display Performance metrics data frame to extract program information
#' @param save Logical indicating whether to save the plots (default: TRUE)
#' @param path Optional custom path to save the plots
#'
#' @return A list of ggplot objects
#'
#' @importFrom ggplot2 ggsave element_text
#'
#' @export
create_top_programs_lift_charts <- function(top_model_results, performance_display,
                                          save = TRUE, path = NULL) {
  # Initialize results list
  lift_charts <- list()

  # Get default programs directory
  programs_dir <- if(is.null(path)) {
    modelled_dir <- tryCatch(
      config::get("modelled_dir"),
      error = function(e) file.path("data", "modelled")
    )
    file.path(modelled_dir, "interpreted", "visualizations", "programs")
  } else {
    file.path(path, "programs")
  }

  # Create directory if needed
  if(save && !dir.exists(programs_dir)) {
    dir.create(programs_dir, recursive = TRUE)
  }

  # Process each model
  for (model_id in names(top_model_results)) {
    # Extract program and strategy info
    program_level <- NA
    week_strategy <- NA

    # Try to get from performance display first
    if(!is.null(performance_display)) {
      program_info <- performance_display |> filter(dataset_id == model_id)
      if(nrow(program_info) > 0) {
        program_level <- program_info$program_level[1]
        week_strategy <- program_info$week_strategy[1]
      }
    }

    # Fall back to extracting from model_id
    if(is.na(program_level) || is.na(week_strategy)) {
      parts <- strsplit(model_id, "_weeks_")[[1]]
      if(length(parts) >= 2) {
        program_level <- parts[1]
        week_strategy <- parts[2]
      }
    }

    # Skip if missing key info
    if(is.na(program_level) || is.na(week_strategy)) {
      next
    }

    # Create enhanced chart
    chart <- tryCatch({
      create_lift_chart(
        model_results = top_model_results[[model_id]],
        title = paste0("Program: ", program_level, " - Intervention Efficiency"),
        save = save,
        path = programs_dir,
        filename = paste0("lift_chart_program_", gsub("[^a-zA-Z0-9]", "_", program_level), "_", week_strategy),
        program_id = program_level,
        week_strategy = week_strategy
      ) +
      labs(
        subtitle = paste0("Week Strategy: ", toupper(substr(week_strategy, 1, 1)),
                        substr(week_strategy, 2, nchar(week_strategy)), " Weeks"),
        caption = paste("Program:", program_level)
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12)
      )
    }, error = function(e) NULL)

    # Store chart if successful
    if(!is.null(chart)) {
      lift_charts[[model_id]] <- chart
    }
  }

  return(lift_charts)
}
