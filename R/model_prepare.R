#' Prepare Data for Predictive Modeling
#'
#' @description
#' Master function to prepare data for modeling by filtering, encoding, and splitting.
#' This function handles all preprocessing steps required before model training.
#'
#' @param data A data frame containing the raw data
#' @param outcome_var The name of the outcome variable (default: "DEELNEMER_BC_uitval")
#' @param program_var The name of the program variable (default: "OPLEIDING_naam_kort")
#' @param level_var The name of the education level variable (default: "VERBINTENIS_niveau")
#' @param cohort_var The name of the cohort year variable (default: "COHORT_startjaar")
#' @param program_filter Optional character vector of programs to include
#' @param level_filter Optional numeric vector of education levels to include
#' @param test_cohort The cohort year to use as test data (default: 2023)
#' @param remove_patterns Character vector of variable patterns to exclude (default: c("week"))
#' @param save Logical indicating whether to save the prepared data (default: TRUE)
#' @param path Optional custom path to save the data
#'
#' @return A list containing the prepared training and test datasets
#'
#' @importFrom dplyr filter select mutate across where arrange matches everything
#' @importFrom tidyr drop_na
#' @importFrom forcats fct_explicit_na
#' @importFrom stringr str_detect
#'
#' @export
prepare_model_data <- function(data,
                                outcome_var = "DEELNEMER_BC_uitval",
                                program_var = "OPLEIDING_naam_kort",
                                level_var = "VERBINTENIS_niveau",
                                cohort_var = "COHORT_startjaar",
                                program_filter = NULL,
                                level_filter = NULL,
                                test_cohort = 2023,
                                remove_patterns = c("week"),
                                save = TRUE,
                                path = NULL) {

  # Record the specific filters used
  filter_info <- list(
    program = program_filter,
    level = level_filter,
    test_cohort = test_cohort
  )

  # Step 1: Apply program and level filters if provided
  filtered_data <- data

  if (!is.null(program_filter)) {
    filtered_data <- filtered_data |>
      filter(.data[[program_var]] %in% program_filter)
  }

  if (!is.null(level_filter)) {
    filtered_data <- filtered_data |>
      filter(.data[[level_var]] %in% level_filter)
  }

  # Step 2: Apply variable filtering and preprocessing
  prepared_data <- filter_model_variables(filtered_data, remove_patterns)
  prepared_data <- encode_model_factors(prepared_data, outcome_var)

  # Step 3: Split the data into training and test sets
  split_data <- split_model_data(prepared_data, cohort_var, test_cohort)

  # Add filter information to results
  result <- c(split_data, list(filter_info = filter_info))

  # Step 4: Save the data if requested
  if (save) {
    save_model_data(result, program_filter, level_filter, path)
  }

  return(result)
}

#' Filter Variables for Modeling
#'
#' @description
#' Remove unnecessary variables based on patterns and data characteristics.
#'
#' @param data A data frame to filter
#' @param remove_patterns Character vector of patterns to match for removal
#'
#' @return A filtered data frame
#'
#' @importFrom dplyr select where matches n_distinct
#'
#' @export
filter_model_variables <- function(data, remove_patterns = c("week")) {
  # Create a pattern for matches() function
  pattern_expr <- paste(remove_patterns, collapse = "|")

  filtered_data <- data |>
    # Remove specified patterns
    select(-matches(pattern_expr)) |>
    # Remove variables with only one unique value (non-informative)
    select(where(~ n_distinct(.) > 1))

  return(filtered_data)
}

#' Encode Factors for Modeling
#'
#' @description
#' Encode categorical variables as factors and handle missing values.
#'
#' @param data A data frame to process
#' @param outcome_var The name of the outcome variable
#'
#' @return A data frame with properly encoded factors and handled missing values
#'
#' @importFrom dplyr mutate across where everything
#' @importFrom forcats fct_explicit_na
#' @importFrom rlang sym !!
#'
#' @export
encode_model_factors <- function(data, outcome_var = "DEELNEMER_BC_uitval") {
  # Ensure the outcome is a factor for classification
  encoded_data <- data |>
    # Convert logical variables to 0/1 integers
    mutate(across(where(is.logical), as.integer)) |>
    # Convert character variables to factors
    mutate(across(where(is.character), as.factor)) |>
    # Fill missing factor values with "Unknown"
    mutate(across(where(is.factor), ~ suppressWarnings(
      fct_explicit_na(.x, na_level = "Onbekend")
    ))) |>
    # Handle missing numeric values with mean imputation
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

  # Special handling for outcome variable if it exists
  if (outcome_var %in% names(encoded_data)) {
    encoded_data <- encoded_data |>
      mutate(
        # Ensure outcome is integer first (0/1)
        !!outcome_var := as.integer(!!sym(outcome_var)),
        # Then convert to factor with descriptive labels
        !!outcome_var := factor(!!sym(outcome_var),
                              levels = c(0, 1),
                              labels = c("Geen uitval", "Uitval"))
      ) |>
      # Rearrange to put outcome first
      select(!!sym(outcome_var), everything())
  }

  return(encoded_data)
}

#' Split Data into Training and Test Sets
#'
#' @description
#' Split data into training and test sets based on cohort year.
#'
#' @param data A data frame to split
#' @param cohort_var The name of the cohort year variable
#' @param test_cohort The cohort year to use as test data
#'
#' @return A list containing the training and test datasets
#'
#' @importFrom dplyr filter
#'
#' @export
split_model_data <- function(data, cohort_var = "COHORT_startjaar", test_cohort = 2023) {
  # Split into training (earlier cohorts) and test (test_cohort)
  train_data <- data |>
    filter(.data[[cohort_var]] < test_cohort)

  test_data <- data |>
    filter(.data[[cohort_var]] == test_cohort)

  # Print information about the splits
  cat("Training data size (pre-", test_cohort, "):", nrow(train_data), "rows\n")
  cat("Test data size (", test_cohort, "):", nrow(test_data), "rows\n")

  # Check if both sets have data
  if (nrow(train_data) == 0) {
    warning("Training dataset is empty. Check your cohort filter criteria.")
  }

  if (nrow(test_data) == 0) {
    warning("Test dataset is empty. Check your cohort filter criteria.")
  }

  return(list(
    train = train_data,
    test = test_data
  ))
}

#' Save Prepared Model Data
#'
#' @description
#' Save prepared data to the appropriate location in the modelled/prepared directory.
#'
#' @param data_list A list containing the training and test datasets and filter info
#' @param program_filter The program filter used (or NULL)
#' @param level_filter The level filter used (or NULL)
#' @param custom_path Optional custom path to save the data
#'
#' @return Invisibly returns the paths where the data was saved
#'
#' @importFrom config get
#'
#' @export
save_model_data <- function(data_list, program_filter = NULL, level_filter = NULL, custom_path = NULL) {
  # Create a descriptive filename
  program_str <- if (is.null(program_filter)) "all_programs" else paste0(program_filter, collapse = "_")
  level_str <- if (is.null(level_filter)) "all_levels" else paste0("level", paste0(level_filter, collapse = "_"))
  cohort_str <- paste0("cohort", data_list$filter_info$test_cohort)

  filename <- paste0(program_str, "_", level_str, "_", cohort_str, ".rds")

  # Determine the path for saving
  if (is.null(custom_path)) {
    # Check if config exists, otherwise use default path
    if (requireNamespace("config", quietly = TRUE)) {
      modelled_dir <- try(config::get("modelled_dir"), silent = TRUE)
      if (inherits(modelled_dir, "try-error")) {
        modelled_dir <- file.path("data", "modelled")
      }
    } else {
      modelled_dir <- file.path("data", "modelled")
    }

    model_data_dir <- file.path(modelled_dir, "prepared")
  } else {
    model_data_dir <- custom_path
  }

  # Create directory if it doesn't exist
  if (!dir.exists(model_data_dir)) {
    dir.create(model_data_dir, recursive = TRUE)
  }

  # Full path for saving
  full_path <- file.path(model_data_dir, filename)

  # Save the data
  saveRDS(data_list, full_path)

  # Inform user
  message("Model data saved to: ", full_path)

  # Return the path invisibly
  invisible(full_path)
}

#' Create Metadata for Model Files
#'
#' @description
#' Generate consistent metadata for model files to track data provenance.
#'
#' @param data The data being used
#' @param program_info Program filter information
#' @param level_info Level filter information
#' @param cohort_info Cohort year information
#' @param notes Optional additional notes
#'
#' @return A list containing metadata
#'
#' @export
create_model_metadata <- function(data, program_info, level_info, cohort_info, notes = NULL) {
  metadata <- list(
    creation_date = Sys.time(),
    n_observations = nrow(data),
    n_variables = ncol(data),
    program_filter = program_info,
    level_filter = level_info,
    cohort_info = cohort_info,
    notes = notes
  )

  return(metadata)
}
