



# test_print <- function(function_content) {
#     print("The current function is:")
#     # Using width.cutoff = 500 prevents line breaks at inconvenient places
#     # paste0 with collapse="\n" ensures each line is printed separately
#     formatted_content <- paste0(deparse(function_content, width.cutoff = 500), collapse = "\n")
#     # Add proper indentation
#     cat(formatted_content, "\n")
# }
#
# my_function <- function() {
#     x <- 1
#     if(x > 0) {
#         print("test")
#     }
#     test_print(sys.function())
#     current_function_name <- as.character(match.call()[[1]])
#     print(current_function_name)
# }
#
# my_function()
#
# function_B <- function() {
#     # Get the call one level up in the call stack
#     parent_call <- sys.call(sys.parent())
#     # Extract the name of the calling function
#     caller_name <- as.character(parent_call[[1]])
#     print(paste("I was called by:", caller_name))
# }
#
# function_A <- function() {
#     print("I am function A")
#     function_B()
# }
#
# # Test the calls
# function_A()

# #' @importFrom dplyr left_join join_by
# combine_enrollments <- function(enrollments_basics, enrollments_flex, enrollments_level, enrollments_application, cohorts, teams) {
#     data <- enrollments_basics |>
#         left_join(enrollments_level, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
#         left_join(enrollments_flex, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
#         left_join(enrollments_application, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
#         left_join(programmes_basics, by = join_by(OPLEIDING_ID), relationship = "many-to-one") |>
#         left_join(cohorts, by = join_by(COHORT_ID), relationship = "many-to-one") |>
#         left_join(teams, by = join_by(TEAM_ID), relationship = "many-to-one")
# }


# extract_globals_to_wordlist <- function() {
#     # Read globals
#     globals_content <- readLines("R/globals.R")
#     vars <- globals_content[grep("^[[:space:]]*\"[^\"]+\"", globals_content)]
#     vars <- gsub("[[:space:]\"#,]+", "", vars)
#
#     # Add both cases
#     vars_both_cases <- unique(c(vars, tolower(vars), toupper(vars)))
#
#     dir.create("inst", showWarnings = FALSE)
#     writeLines(sort(vars_both_cases), "inst/WORDLIST")
# }




