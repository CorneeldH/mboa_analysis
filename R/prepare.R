#' Calculate the Duration of Application Process
#'
#' @description
#' Takes a data frame containing application data and calculates the number of days
#' between the application start date and last modification date.
#'
#' @param data A data frame containing application data with columns
#'             AANMELDING_begin_datum and AANMELDING_laatst_gewijzigd_datum
#'
#' @return A data frame with an additional column VERBINTENIS_aanmelding_in_proces
#'         containing the rounded number of days between start and last modification
#'
#' @details
#' The function calculates the difference in days between AANMELDING_laatst_gewijzigd_datum
#' and AANMELDING_begin_datum, rounds the result, and adds it as a new column.
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' enrollments_application <- calculate_application_duration(raw_enrollments_application)
#' }
#' @export
calculate_application_duration <- function(data) {
    data_prepared <- data |>
        mutate(VERBINTENIS_aanmelding_dagen_in_proces = round(as.numeric(
            difftime(
                VERBINTENIS_aanmelding_laatst_gewijzigd_datum,
                VERBINTENIS_aanmelding_begin_datum,
                units = "days"
            )
        )))

    save_prepared(data_prepared)

    return(data_prepared)

}

#' Calculate Combined Enrollment Numbers
#'
#' @description
#' Creates a combined identifier from enrollment sequence number and page number
#' by multiplying the sequence number by 1000 and adding the page number.
#'
#' @param data A data frame containing enrollment data with columns
#'             VERBINTENIS_volgnummer and VERBINTENIS_bladnummer
#'
#' @return A data frame with an additional column VERBINTENIS_volg_en_blad_nummers
#'         containing the combined identifier
#'
#' @details
#' The function creates a unique identifier by multiplying VERBINTENIS_volgnummer
#' by 1000 and adding VERBINTENIS_bladnummer to ensure unique combinations.
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' enrollments_basics <- combine_enrollment_numbers_for_order(raw_enrollments_basics)
#' }
#' @export
combine_enrollment_numbers_for_order <- function(data) {

    data_prepared <- data |>
        mutate(VERBINTENIS_volg_en_blad_nummers = VERBINTENIS_volgnummer * 1000 + VERBINTENIS_bladnummer)

    save_prepared(data_prepared)

    return(data_prepared)
}


#' Create Boolean Flex Status
#'
#' @description
#' Converts flex status from text ("Ja") to boolean (TRUE/FALSE).
#'
#' @param data A tibble containing a VERBINTENIS_is_flex_omschrijving column
#'
#' @return A tibble with additional VERBINTENIS_is_flex column:
#'   \itemize{
#'     \item VERBINTENIS_is_flex: Boolean indicating flex status
#'   }
#'
#' @importFrom dplyr mutate
#'
#' @export
create_flex_boolean <- function(data) {

    data_prepared <- data |>
        mutate(VERBINTENIS_is_flex = ifelse(VERBINTENIS_flex_omschrijving == "Ja", TRUE, FALSE))

    save_prepared(data_prepared)

    return(data_prepared)

}




#' Add Cohort Start Year
#'
#' @description
#' Extract and add the start year from cohort names
#'
#' @param data A data frame containing a COHORT_naam column
#'
#' @returns
#' The input data frame with an additional COHORT_startjaar column containing
#' numeric start years
#'
#' @importFrom dplyr mutate
#'
#' @export
add_cohort_start_year <- function(data) {

    data_prepared <- data |>
        mutate(COHORT_startjaar = as.numeric(substr(COHORT_naam, 1, 4)))

    save_prepared(data_prepared)

    return(data_prepared)
}

add_cohort_start_date <- function(data) {
    data_prepared <- data |>
        mutate(
            COHORT_start_datum = as.Date(paste0(COHORT_startjaar, "-08-01"))
        )

    save_prepared(data_prepared)

    return(data_prepared)
}


#' Convert Absence Types
#'
#' @description
#' Convert data types of absence-related columns to their appropriate formats
#'
#' @param data A data frame containing employee absence information.
#'
#' @returns
#' A data frame with converted column types:
#' - MEDEWERKER_ID as character
#' - MEDEWERKER_contract_kostenplaats_code as character
#' - MEDEWERKER_eerste_verzuimdag as Date
#' - MEDEWERKER_laatste_verzuimdag as Date
#' - MEDEWERKER_percentage_verzuim as numeric (proportion)
#'
#' @importFrom dplyr mutate
#'
#' @export
convert_absence_types <- function(data) {
    data_prepared <- data |>
        mutate(
            MEDEWERKER_eerste_verzuimdag = as.Date(MEDEWERKER_eerste_verzuimdag, format = "%Y-%m-%d"),
            MEDEWERKER_laatste_verzuimdag = as.Date(MEDEWERKER_laatste_verzuimdag, format = "%Y-%m-%d"),
            MEDEWERKER_percentage_verzuim = as.numeric(MEDEWERKER_percentage_verzuim_omschrijving) / 100
        ) |>
        select(-MEDEWERKER_percentage_verzuim_omschrijving)

    save_prepared(data_prepared)

    return(data_prepared)

}

#' Split Data into School Years
#'
#' @description
#' Split a dataset into multiple groups based on school years, where each school
#' year runs from August 1st to July 31st
#'
#' @param data A data frame containing columns MEDEWERKER_eerste_verzuimdag and
#'   MEDEWERKER_laatste_verzuimdag (as Date objects)
#' @param first_year Optional. Starting year for the split. If NULL, retrieved from config
#' @param last_year Optional. Ending year for the split. If NULL, retrieved from config
#'   or defaults to current year
#'
#' @returns
#' A list of data frames, one for each school year. Each data frame includes the
#' original data plus SCHOOLJAAR_startdatum, SCHOOLJAAR_einddatum, and
#' SCHOOLJAAR_naam columns
#'
#' @importFrom dplyr mutate filter cross_join group_by group_split
#' @importFrom lubridate years days
#'
#' @export
split_absences_into_school_years <- function(data, first_year = NULL, last_year = NULL) {

    if (is.null(first_year)) {

        requireNamespace("config", quietly = TRUE)
        first_year <- try(config::get("first_year"), silent = TRUE)

        if (inherits(first_year, "try-error")) {
            stop("No first year found in argument or config")
        }
    }

    if (is.null(last_year)) {

        requireNamespace("config", quietly = TRUE)
        last_year <- try(config::get("last_year"), silent = TRUE)

        if (inherits(last_year, "try-error")) {
            last_year <- format(Sys.Date(), "%Y")
            message("No last year found in config, using current year")
        }
    }

    school_years <- data.frame(
        SCHOOLJAAR_startdatum = seq.Date(
            from = as.Date(paste0(first_year, "-08-01")),
            to = as.Date(paste0(last_year, "-08-01")),
            by = "1 year"
        )) |>
        mutate(
            SCHOOLJAAR_einddatum = SCHOOLJAAR_startdatum + years(1) - days(1),
            SCHOOLJAAR_naam = paste0(
                format(SCHOOLJAAR_startdatum, "%Y"),
                "/",
                format(SCHOOLJAAR_startdatum + years(1), "%Y")
            )
        )

    data_in_years <- data |>
        # Cross join with school years
        cross_join(school_years) |>
        # Filter for relevant periods
        filter(
            MEDEWERKER_eerste_verzuimdag <= SCHOOLJAAR_einddatum,
            MEDEWERKER_laatste_verzuimdag >= SCHOOLJAAR_startdatum | is.na(MEDEWERKER_laatste_verzuimdag)
        )

    data_in_years_split <- data_in_years |>
        group_by(SCHOOLJAAR_naam) |>
        group_split()

    save_prepared(data_in_years_split)

    return(data_in_years_split)
}


#' Convert BPV Status Date Types
#'
#' @description
#' Convert date columns in BPV (professional practice) status data to proper Date types
#'
#' @param data A data frame containing date columns (with suffix "datum")
#'
#' @returns
#' A data frame with date columns converted to Date class
#'
#' @importFrom dplyr mutate across
#'
#' @export
convert_bpv_status_types <- function(data) {
    data_prepared <- data |>
        mutate(across(contains("datum"), as.Date, format = "%Y-%m-%d"))

    save_prepared(data_prepared)

    return(data_prepared)

}

#' Add School Year Column
#'
#' @description
#' Adds a school year column based on a reference date
#'
#' @param data A data frame containing the column MEDEWERKER_contract_fte_peildatum
#'
#' @returns
#' The input data frame with an additional SCHOOLJAAR_naam column in the format "YYYY/YYYY+1"
#'
#' @importFrom lubridate year
#' @importFrom dplyr mutate
#'
#' @export
add_school_year <- function(data) {
    data_prepared <- data |>
        mutate(
            SCHOOLJAAR_naam = get_school_year(MEDEWERKER_contract_fte_peildatum)
        )

    save_prepared(data_prepared)

    return(data_prepared)
}

#' Parse Enrollment Level
#'
#' @description
#' Extract numeric level from enrollment level description
#'
#' @param data A data frame containing a column `VERBINTENIS_niveau_omschrijving`
#'
#' @returns
#' A data frame with an additional column `VERBINTENIS_niveau` containing the numeric level
#'
#' @importFrom readr parse_number
#'
#' @export
parse_enrollment_level <- function(data) {

    data_prepared <- data |>
        mutate(VERBINTENIS_niveau = parse_number(VERBINTENIS_niveau_omschrijving))

    save_prepared(data_prepared)

    return(data_prepared)
}


#' Parse Team Result Percentages
#'
#' @description
#' Convert text-based result descriptions to numeric percentages
#'
#' @param teams_results A data frame containing team results with a column
#'   TEAM_startersresultaat_1_jaars_omschrijving
#'
#' @returns
#' A data frame with an additional numeric column TEAM_startersresultaat_1_jaars
#' containing percentages multiplied by 100
#'
#' @importFrom dplyr mutate
#' @importFrom readr parse_number
#'
#' @export
parse_result_pct <- function(teams_results) {
    team_results_prepared <- teams_results |>
        mutate(
            TEAM_startersresultaat_1_jaars = parse_number(TEAM_startersresultaat_1_jaars_omschrijving) * 100
        )

    save_prepared(team_results_prepared)

    return(team_results_prepared)

}

#' Format School Year Names
#'
#' @description
#' Convert school year names from a dash format to a slash format
#'
#' @param teams_results A data frame containing a column SCHOOLJAAR_naam_met_streep
#'
#' @returns
#' A data frame with formatted school year names in new column SCHOOLJAAR_naam
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#'
#' @export
format_school_year_name <- function(teams_results) {

    team_results_prepared <- teams_results |>
        mutate(SCHOOLJAAR_naam = str_replace(SCHOOLJAAR_naam_met_streep, " - ", "/"))

    save_prepared(team_results_prepared)

    return(team_results_prepared)

}

#' Add Helper Variables to Employee Satisfaction Data
#'
#' @description
#' Adds grouping and appearance variables to employee satisfaction survey data
#'
#' @param employee_answers_satisfaction A data frame containing employee satisfaction survey responses.
#'
#' @returns
#' A data frame with additional columns:
#' \itemize{
#'   \item group_number: A unique identifier for each group
#'   \item appearance_number: Sequential numbering within each group
#' }
#'
#' @importFrom dplyr filter group_by mutate ungroup row_number cur_group_id
#' @importFrom stringr str_detect
#'
#' @export
add_helper_variables <- function(employee_answers_satisfaction) {

    employee_answers_satisfaction_with_helper_vars <- employee_answers_satisfaction |>
    filter(!str_detect(QuestionId, "_")) |>
        group_by(SCHOOLJAAR_startjaar, Organisatie, `Characteristic 1`, `Characteristic 2`) |>
        mutate(group_number = cur_group_id()) |>
        ungroup() |>
        group_by(QuestionId, group_number) |>
        mutate(appearance_number = row_number()) |>
        ungroup()

    return(employee_answers_satisfaction_with_helper_vars)
}


