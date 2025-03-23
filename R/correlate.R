
#' Calculate and Filter Variable Correlations
#'
#' @description
#' Calculate correlations between variables and filter based on correlation strength
#'
#' @param df A data frame containing numeric variables to correlate
#' @param target_var A single string specifying the target variable to correlate against
#' @param cols Optional. Column selection using tidy-select syntax
#' @param filter_type One of `"none"`, `"between"`, or `"outside"`
#' @param correlation_limits Optional. A numeric vector of length 2 specifying correlation boundaries
#' @param exclude_terms Optional. A character vector of variable names to exclude
#'
#' @returns
#' A data frame of correlations between the target variable and other variables,
#' filtered according to the specified criteria
#'
#' @importFrom dplyr select filter arrange mutate sym
#' @importFrom stringr str_replace_all
#' @importFrom tidyselect matches
#' @importFrom stats setNames
#'
#' @export
correlate_teams_and_filter <- function(df,
                            target_var,
                            cols = !matches("JOB|MWO"),
                            filter_type = c("none", "between", "outside"),
                            correlation_limits = c(-0.3, 0.3),
                            exclude_terms = c(
                                "MEDEWERKER_verzuim_kort",
                                "MEDEWERKER_verzuim_kort_opgevuld",
                                "VERBINTENIS_is_passend_onderwijs_gevuld_opgevuld"
                            )) {

    filter_type <- match.arg(filter_type)

    result <- df |>
        select(target_var, {{cols}}) |>
        correlate() |>
        select(all_of(c("term", target_var))) |>
        filter(
            term != !!sym(target_var),
            !term %in% exclude_terms
        )

    result <- if (filter_type == "between") {
        result |>
            filter(
                !!sym(target_var) >= correlation_limits[1],
                !!sym(target_var) <= correlation_limits[2]
            )
    } else if (filter_type == "outside") {
        result |>
            filter(
                !!sym(target_var) <= correlation_limits[1] |
                    !!sym(target_var) >= correlation_limits[2]
            )
    } else {
        result
    }

    # First arrange results
    result_arranged <- result |>
        arrange(!!sym(target_var))

    # Apply user-friendly variable names using existing function
    # Convert term column to user-friendly names
    mapping_file <- file.path(config::get("data_reference_dir", "data/reference"), "variable_descriptions.csv")
    mapping <- readr::read_delim(mapping_file, delim = ";", show_col_types = FALSE)
    name_map <- setNames(mapping$user_friendly_name, mapping$variable_name)

    # Replace names and maintain ordering
    result_arranged |>
        mutate(
            term = ifelse(term %in% names(name_map), name_map[term], term),
            term = factor(term, levels = term)
        )
}



#' Plot Team Correlations
#'
#' @description
#' Create a horizontal bar chart showing correlations between teams and a target variable
#'
#' @param df A data frame containing correlation data.
#' @param target_var A string specifying the correlation variable column.
#' @param title_text A string for the plot title.
#' @param low_color Optional. A string specifying the color for negative correlations. Default is "indianred".
#' @param high_color Optional. A string specifying the color for positive correlations. Default is "skyblue1".
#'
#' @returns
#' A ggplot2 object showing correlation values as colored segments with text labels.
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_text scale_color_gradient2 scale_x_continuous theme_minimal theme element_text element_blank ggtitle
#' @importFrom rlang sym
#'
#' @export
plot_teams_correlations <- function(df,
                                    target_var,
                                    title_text,
                                    low_color = "indianred",
                                    high_color = "skyblue1") {

    # The input data already has user-friendly names at this point
    ggplot(df,
           aes(y = term)) +
        geom_segment(aes(
            x = 0,
            xend = !!sym(target_var),
            yend = term,
            color = !!sym(target_var)
        ),
        linewidth = 2) +
        # Fixed position for numbers on the right
        geom_text(aes(
            x = 0.55,  # Fixed x position for all numbers
            label = sprintf("%.2f", !!sym(target_var))
        ),
        hjust = 0,  # Align text to the left at the fixed position
        size = 3,
        color = "black") +
        scale_color_gradient2(
            low = low_color,
            mid = "white",
            high = high_color,
            limits = c(-0.6, 0.6),
            midpoint = 0
        ) +
        scale_x_continuous(limits = c(-0.5, 0.6)) +
        theme_minimal() +
        theme(
            axis.text.y = element_text(size = 8),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            legend.position = "none"
        ) +
        ggtitle(title_text)
}

