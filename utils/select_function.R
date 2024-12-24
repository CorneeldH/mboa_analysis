select_function <- function(file_path, fun_name, start_line) {
    file_uri <- rstudioapi::navigateToFile(file_path)
    doc <- rstudioapi::getSourceEditorContext()

    # Find function end by matching braces
    lines <- doc$contents
    brace_count <- 0
    end_line <- start_line

    for (i in start_line:length(lines)) {
        brace_count <- brace_count +
            sum(stringr::str_count(lines[i], "\\{")) -
            sum(stringr::str_count(lines[i], "\\}"))

        if (brace_count == 0) {
            end_line <- i +1
            break
        }
    }

    rstudioapi::setSelectionRanges(
        rstudioapi::document_range(
            start = rstudioapi::document_position(start_line, 1),
            end = rstudioapi::document_position(end_line, nchar(lines[end_line]))
        )
    )
}


select_and_run_pal <- function(file_path, fun_name, start_line) {
    file_path <- file.path("R", file_path)
    select_function(file_path, fun_name, start_line)
    #pal::.init_addin()
    #Sys.sleep(1)
    #keypress <- rstudioapi::sendToConsole("\n", execute = TRUE)
    #rstudioapi::sendToConsole("\n", execute = TRUE)
    #rstudioapi::insertText(text = "roxygen2")
    #rstudioapi::executeCommand("historySendToSource")
    #shinyjs::runjs("document.getElementById('elementId').dispatchEvent(new KeyboardEvent('keydown', {'key': 'Enter'}))")
    #rstudioapi::executeCommand("insertText", args = list(text = "\n"))
    #rstudioapi::executeCommand("triggerKeyboardShortcut", args = list(shortcut = "\n"))
    #rstudioapi::triggerKeyboardShortcut("Enter")
    #.init_pal(role = "roxygen2")
    # pal::pal_roxygen()
#        pal_fn <- env_get(pal_env(), "roxygen2")
#        do.call(pal_fn, args = list())

}
