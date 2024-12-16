library(stringr)

#' Extract code chunks from a Quarto file
#' @param file_path Path to the Quarto file
#' @return Character vector of R code chunks
extract_r_chunks <- function(file_path) {
    # Read the file
    qmd_content <- readLines(file_path)

    # Initialize variables
    chunks <- character(0)
    in_chunk <- FALSE
    current_chunk <- character(0)

    for (line in qmd_content) {
        if (grepl("^```\\{r.*\\}", line)) {
            in_chunk <- TRUE
            current_chunk <- character(0)
        } else if (grepl("^```$", line) && in_chunk) {
            in_chunk <- FALSE
            chunks <- c(chunks, paste(current_chunk, collapse = "\n"))
        } else if (in_chunk) {
            current_chunk <- c(current_chunk, line)
        }
    }

    return(chunks)
}

#' Extract assignments and their usage from R code
#' @param code_chunks Character vector of R code chunks
#' @return List containing final assignments (unused after assignment) and non-final assignments (used after assignment), and warnings
analyze_assignments <- function(code_chunks) {
    # Combine all chunks into one string for analysis
    full_code <- paste(code_chunks, collapse = "\n")

    # Find all assignments using <-
    # Match: start of line or space/tab before variable name, followed by the name and <-
    assignments <- str_extract_all(full_code, "(^|\\s+)([[:alnum:]_\\.]+)\\s*<-")[[1]]
    assignments <- str_trim(str_replace(assignments, "<-$", ""))
    assignments <- str_trim(assignments) # Remove leading whitespace

    # Check for duplicate assignments
    duplicates <- assignments[duplicated(assignments)]
    warnings <- if(length(duplicates) > 0) {
        paste("Warning: Duplicate assignments found for:",
              paste(duplicates, collapse = ", "))
    } else {
        character(0)
    }

    # Initialize results
    final_assignments <- character(0)
    non_final_assignments <- character(0)

    # Create a data frame of all assignments with their positions
    assignment_positions <- data.frame(
        var = character(),
        position = numeric(),
        stringsAsFactors = FALSE
    )

    # Find all assignment positions
    for (var in unique(assignments)) {
        positions <- str_locate_all(full_code, paste0("(^|\\s+)", var, "\\s*<-"))[[1]][,1]
        assignment_positions <- rbind(
            assignment_positions,
            data.frame(
                var = var,
                position = positions,
                stringsAsFactors = FALSE
            )
        )
    }

    # Sort by position to process assignments in order
    assignment_positions <- assignment_positions[order(assignment_positions$position),]

    # Track the final state of each variable
    final_assignments <- character(0)
    non_final_assignments <- character(0)

    # Get the last position for each variable
    last_positions <- tapply(assignment_positions$position,
                             assignment_positions$var,
                             max)

    # For each variable, check if it's used after its last assignment
    for (var in names(last_positions)) {
        last_pos <- last_positions[var]

        remaining_code <- substr(full_code, last_pos + 10, nchar(full_code))

        # Pattern matches:
        # 1. Variable followed by pipe operators
        # 2. Variable inside join functions (by looking for the variable after "join(")
        # 3. Variable after "by = join_by(" or similar join specifications
        is_used <- grepl(paste0("(",
                                "\\b", var, "\\s*(\\|>|%>%)|",  # pipe operators
                                "join\\([^)]*\\b", var, "\\b|",  # inside join()
                                "join_by[^)]*\\b", var, "\\b",   # inside join_by()
                                ")"),
                         remaining_code, perl = TRUE)

        #is_used <- grepl(var, remaining_code, perl = TRUE)


        if (is_used) {
            non_final_assignments <- c(non_final_assignments, var)
        } else {
            final_assignments <- c(final_assignments, var)
        }
    }

    return(list(
        final_assignments = sort(unique(final_assignments)),
        non_final_assignments = sort(unique(non_final_assignments)),
        warnings = warnings
    ))
}

#' Main function to analyze a Quarto file
#' @param file_path Path to the Quarto file
#' @return List containing analysis results
analyze_quarto_assignments <- function(file_path) {
    chunks <- extract_r_chunks(file_path)
    results <- analyze_assignments(chunks)

    # Print warnings if any
    if (length(results$warnings) > 0) {
        warning(results$warnings)
    }

    return(results)
}

# Example usage:
results <- analyze_quarto_assignments("01_ingest.qmd")
print(results$final_assignments)
print(results$non_final_assignments)
