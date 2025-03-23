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

#' Process Program Charts for Model Comparisons
#'
#' @description
#' Generate and save lift charts comparing model performance across different
#' timing strategies for all educational programs
#'
#' @returns
#' A list of ggplot objects, one for each program. Charts are also saved to disk
#' as PNG files and the complete list is saved as an RDS file.
#'
#' @importFrom dplyr arrange mutate row_number n select bind_rows
#' @importFrom ggplot2 ggplot aes geom_abline geom_line labs scale_x_continuous scale_y_continuous theme_minimal theme ggsave
#' @importFrom stringr str_replace
#' @importFrom config get
#' @importFrom rlang sym
#'
#' @export
process_all_program_charts <- function() {
    # Get all unique program levels from models
    all_programs <- unique(sapply(strsplit(names(all_model_results), "_weeks_"), function(x) x[1]))
    cat("Found", length(all_programs), "unique programs\n")

    # Directory for visualizations
    vis_dir <- file.path(config::get("modelled_dir"), "interpreted", "visualizations", "programs")
    if(!dir.exists(vis_dir)) {
        dir.create(vis_dir, recursive = TRUE)
    }

    # Process each program
    all_charts <- list()
    for(program_name in all_programs) {
        cat("Processing:", program_name, "\n")

        # Find models matching this program
        model_ids <- names(all_model_results)[startsWith(names(all_model_results), paste0(program_name, "_weeks_"))]

        if(length(model_ids) == 0) next

        # Collect data from all week strategies
        chart_data <- data.frame()
        for(model_id in model_ids) {
            week_strategy <- sub(".*weeks_", "", model_id)
            predictions <- collect_predictions(all_model_results[[model_id]]$final_model)

            # Find columns
            outcome_col <- grep("DEELNEMER_BC_uitval|.outcome|truth", colnames(predictions), value = TRUE)[1]
            prob_col <- grep("^.pred_Uitval|^.pred_TRUE", colnames(predictions), value = TRUE)[1]

            # Calculate lift data
            lift_data <- predictions |>
                arrange(desc(!!sym(prob_col))) |>
                mutate(
                    actual_dropout = as.numeric(!!sym(outcome_col) %in% c("Uitval", "TRUE", "1", 1, TRUE)),
                    rank = row_number(),
                    rank_pct = rank / n(),
                    cum_dropouts = cumsum(actual_dropout),
                    cum_dropout_pct = cum_dropouts / sum(actual_dropout)
                ) |>
                select(rank_pct, cum_dropout_pct) |>
                mutate(strategy = week_strategy)

            chart_data <- bind_rows(chart_data, lift_data)
        }

        # Format strategy names
        chart_data <- chart_data |>
            mutate(strategy = factor(strategy,
                                     levels = c("none", "early", "all"),
                                     labels = c("Bij start", "Na 5 weken", "Na 10 weken")))

        # Clean program name for display
        clean_program <- str_replace(program_name, "_level", " niveau ")

        # Create and save chart
        comparison_chart <- ggplot(chart_data, aes(x = rank_pct, y = cum_dropout_pct, color = strategy)) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
            geom_line(linewidth = 1.2) +
            labs(
                title = clean_program,
                subtitle = "Toont % uitvallers bereikt bij interventie met % studenten",
                x = "Percentage benaderde studenten",
                y = "Percentage bereikte uitvallers",
                color = "Tijdstip"
            ) +
            scale_x_continuous(labels = scales::percent) +
            scale_y_continuous(labels = scales::percent) +
            theme_minimal() +
            theme(legend.position = "bottom")

        # Save chart
        clean_filename <- gsub("[^a-zA-Z0-9]", "_", clean_program)
        ggsave(
            file.path(vis_dir, paste0("lift_chart_", clean_filename, "_comparison.png")),
            comparison_chart,
            width = 10,
            height = 7
        )

        all_charts[[program_name]] <- comparison_chart
    }

    # Save all charts
    saveRDS(all_charts, file.path(config::get("modelled_dir"), "interpreted", "all_program_charts.rds"))

    return(all_charts)
}
