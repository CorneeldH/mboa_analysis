#' Safe CSV Reader with Flexible Arguments
#'
#' @description
#' Safely reads a delimited file with error handling, path validation, and
#' configurable reading parameters. Uses sane defaults for European CSV format.
#'
#' @param filename Required filename
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing the CSV data
#' @importFrom readr read_delim cols col_guess locale
#'
#' @export
safe_read_csv <- function(filename, ...) {

    # Check if file exists
    if (!file.exists(filename)) {
        stop(sprintf("File not found: %s", filename))
    }

    # Default arguments for European CSV format
    default_args <- list(
        file = filename,
        delim = ";",
        name_repair = "minimal",
        escape_double = FALSE,
        locale = locale(decimal_mark = ",", grouping_mark = "."),
        trim_ws = TRUE,
        col_types = cols(.default = col_guess()),
        na = c("", "NA", "NULL")
    )

    # Override defaults with any provided arguments
    function_args <- utils::modifyList(default_args, list(...))

    # Attempt to read file
    tryCatch({
        df <- do.call(read_delim, function_args)
        if (nrow(df) == 0) warning(sprintf("File is empty: %s", filename))
        df
    }, error = function(e) {
        stop(sprintf("Error reading file %s: %s", filename, e$message))
    })
}

#' Get Filename from Config
#'
#' @description
#' Retrieves a filename from config settings. If the config key contains a list,
#' looks for the specified argument within that list.
#'
#' @param config_key Character string specifying the config key to look up
#' @param argument Character string specifying which argument to extract from list,
#'   defaults to "filename"
#'
#' @return Character string containing the filename
#'
#' @examples
#' \dontrun{
#' # For simple config entry
#' # config.yml: default: input_file: "data.csv"
#' get_filename_from_config("input_file")
#'
#' # For list config entry
#' # config.yml: default: files: {filename: "data.csv", path: "data/"}
#' get_filename_from_config("files", "filename")
#' }
#'
#' @export
get_filename_from_config <- function(config_key, argument = "filename") {
    if (!requireNamespace("config", quietly = TRUE)) {
        stop("The 'config' package is not available")
    }

    config_value <- try(config::get(config_key), silent = TRUE)

    if (inherits(config_value, "try-error")) {
        stop(sprintf("The config key '%s' was'nt found in the config file", config_key))
    }

    # If config_value is a list, look for the specified argument
    if (is.list(config_value)) {
        if (argument %in% names(config_value)) {
            return(config_value[[argument]])
        } else {
            available_args <- paste(names(config_value), collapse = ", ")
            stop(sprintf(
                "Argument '%s' not found in config key '%s'. Available arguments are: %s",
                argument, config_key, available_args
            ))
        }
    }

    # If not a list, return the value directly
    return(config_value)
}
