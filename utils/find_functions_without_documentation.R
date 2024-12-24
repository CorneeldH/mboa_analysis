get_r_files <- function(path) {
    list.files(path, pattern = "\\.R$", full.names = TRUE)
}

parse_file_content <- function(file) {
    content <- read_lines(file)
    if (length(content) == 0) {
        return(tibble(file = basename(file), function_name = character(), line = integer()))
    }
    tibble(file = basename(file), line_num = seq_along(content), text = content)
}

find_functions <- function(content, fun_pattern, name_pattern) {
    result <- content |>
        mutate(
            is_fun_start = str_detect(text, fun_pattern),
            fun_name = str_extract(text, "^\\s*([[:alnum:]_]+)(?=\\s*<-\\s*function)")
        )

    # Debug lines with functions
    if (any(result$is_fun_start)) {
        print("Found function lines:")
        print(result |> filter(is_fun_start) |> select(text, fun_name))
    }

    result

}

check_documentation <- function(content_with_funs, roxy_pattern, window_size) {
    content_with_funs |>
        mutate(
            has_roxygen = slider::slide_dbl(
                str_detect(text, roxy_pattern),
                sum,
                .before = window_size - 1,
                .complete = TRUE
            ) > 0
        )
}

extract_undocumented <- function(parsed_content) {
    parsed_content |>
        filter(is_fun_start, !has_roxygen) |>
        select(file, fun_name, line = line_num)
}

find_undocumented <- function(path = "R/") {
    fun_pattern <- "^\\s*[[:alnum:]_]+\\s*<-\\s*function\\s*\\("
    roxy_pattern <- "^\\s*#'"
    window_size <- 10

    get_r_files(path) |>
        map_df(parse_file_content) |>
        find_functions(fun_pattern) |>
        check_documentation(roxy_pattern, window_size) |>
        extract_undocumented() |>
        arrange(file, line)
}

undocumented <- find_undocumented()
