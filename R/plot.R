

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
