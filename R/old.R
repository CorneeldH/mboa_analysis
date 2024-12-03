



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
