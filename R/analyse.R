
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

    result |>
        arrange(!!sym(target_var)) |>
        mutate(
            term = str_replace_all(term, "_", " "),
            term = factor(term, levels = term)
        )
}
