#' Extract exported function definitions from R files
#'
#' @param folder_path Path to the R folder
#' @return A list of exported functions by file
get_exported_r_functions <- function(folder_path, exclude_files = "R/utils.R") {
    # List all R files
    r_files <- fs::dir_ls(folder_path, glob = "*.R")
    r_files <- r_files %>% discard(~ str_detect(., exclude_files))

    # Process each file
    all_functions <- r_files %>%
        map(function(file) {
            # Read the file content
            lines <- read_lines(file)

            # Process lines to find exported functions
            exported_funs <- tibble(
                line = lines,
                line_num = seq_along(lines)
            ) %>%
                # Find @export tags and get the next non-roxygen line
                mutate(
                    is_export = str_detect(line, "@export"),
                    is_roxygen = str_detect(line, "^#'")
                ) %>%
                filter(is_export) %>%
                mutate(
                    # For each @export, find the next non-roxygen line
                    next_line = map_chr(line_num, ~{
                        following_lines <- lines[(.x+1):length(lines)]
                        first_non_roxygen <- which(!str_detect(following_lines, "^#'"))[1]
                        if (is.na(first_non_roxygen)) return(NA_character_)
                        following_lines[first_non_roxygen]
                    })
                ) %>%
                # Find function definitions
                filter(
                    !is.na(next_line),
                    !str_detect(next_line, "^#"),
                    str_detect(next_line, "<-[[:space:]]*function")
                ) %>%
                # Extract function names
                mutate(
                    fun_name = str_extract(next_line, "^[[:alnum:]._]+(?=[[:space:]]*<-)") %>%
                        str_trim()
                ) %>%
                pull(fun_name) %>%
                compact()

            if (length(exported_funs) > 0) exported_funs else NULL
        }) %>%
        compact()


    return(all_functions)
}

#' Extract function calls from Quarto document
#'
#' @param qmd_path Path to the Quarto file
#' @return A character vector of function calls
get_qmd_function_calls <- function(qmd_path) {
    # Read and process the Quarto file
    qmd_content <- read_lines(qmd_path)

    # Extract R code chunks
    r_code <- tibble(
        line = qmd_content,
        chunk_start = str_detect(line, "^```\\{r"),
        chunk_end = str_detect(line, "^```$")
    ) %>%
        mutate(
            in_chunk = cumsum(chunk_start) > cumsum(chunk_end),
            is_code = in_chunk & !chunk_start & !chunk_end
        ) %>%
        filter(is_code) %>%
        pull(line)

    # Parse R code and extract function calls
    if (length(r_code) == 0) {
        return(character())
    }

    tryCatch({
        r_code_text <- paste(r_code, collapse = "\n")
        parsed <- parse(text = r_code_text)

        # Extract all function calls recursively
        get_calls <- function(expr) {
            if (is.call(expr)) {
                calls <- as.character(expr[[1]])
                rest_calls <- map(as.list(as.list(expr)[-1]), get_calls) %>%
                    reduce(c, .init = character())
                return(c(calls, rest_calls))
            }
            return(character())
        }

        unique(map(as.list(parsed), get_calls) %>% reduce(c, .init = character()))
    }, error = function(e) {
        warning("Error parsing R code in Quarto document: ", e$message)
        character()
    })
}

#' Check usage of exported functions in a Quarto document
#'
#' @param qmd_path Path to the Quarto document
#' @param r_folder_path Path to the R folder (defaults to "R")
#' @return Invisibly returns a list of results
#' @examples
#' check_exported_function_usage("analysis.qmd")
#' check_exported_function_usage("analysis.qmd", "R/")
check_exported_function_usage <- function(qmd_path, r_folder_path = "R", exclude_files = "R/utils.R") {
    if (!file.exists(qmd_path)) {
        stop("Quarto file not found: ", qmd_path)
    }
    if (!dir.exists(r_folder_path)) {
        stop("R folder not found: ", r_folder_path)
    }

    # Get exported functions and Quarto calls
    r_functions <- get_exported_r_functions(r_folder_path, exclude_files = exclude_files)
    qmd_calls <- get_qmd_function_calls(qmd_path)

    # Analyze usage
    results <- r_functions %>%
        map(~ list(
            used = intersect(., qmd_calls),
            unused = setdiff(., qmd_calls)
        )) %>%
        # Remove files with no results
        compact()

    # Print results
    cat("\nExported Function Usage Analysis Results:\n")

    # Count total exported functions
    total_exported <- sum(map_int(results, ~ length(c(.$used, .$unused))))

    total_used <- sum(map_int(results, ~ length(c(.$used))))

    if (total_exported == 0) {
        cat("\nNo exported functions found in R files.\n")
        return(invisible(results))
    }

    # Show used functions
    used_any <- any(map_lgl(results, ~ length(.$used) > 0))
    if (used_any) {
        cat("\n1. Used exported functions by file:", total_used, "\n")
        # walk2(names(results), results, function(file, res) {
        #     if (length(res$used) > 0) {
        #         cat(sprintf("\n%s:\n", file))
        #         walk(res$used, ~ cat("-", ., "\n"))
        #     }
        # })
    } else {
        cat("\n1. No exported functions are being used in the Quarto document.\n")
    }

    # Show unused functions
    unused_any <- any(map_lgl(results, ~ length(.$unused) > 0))
    if (unused_any) {
        cat("\n2. Unused exported functions by file:\n")
        file_basic = names(results)[[1]]
        cat(file_basic, "\n")
        walk2(names(results), results, function(file, res) {
            if (length(res$unused) > 0) {
                if (file != file_basic) {
                    cat(sprintf("\n%s:\n", file))
                    file_basic <- file
                }
                #cat(sprintf("%s:", file))
                walk(res$unused, ~ cat("- ", ., "\n"))
            }
        })
    }

    # Summary
    cat(sprintf("\nSummary: Found %d exported function(s).\n", total_exported))

    invisible(results)
}
