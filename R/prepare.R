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



#' Filter Cohorts by Year Range
#'
#' @description
#' Filters cohort data based on a year range, either provided directly or
#' retrieved from configuration.
#'
#' @param data A tibble containing cohort data with column:
#'   \itemize{
#'     \item COHORT_naam: Cohort name in format "YYYY/YYYY+1"
#'   }
#' @param first_year Character or numeric specifying start year. If NULL, retrieved from config
#' @param last_year Character or numeric specifying end year. If NULL, defaults to current year
#'
#' @return A filtered tibble containing only cohorts within the specified year range
#'
#' @details
#' The function extracts the start year from COHORT_naam and filters based on the
#' specified range. If years aren't provided, attempts to read from config.
#' If last_year isn't found in config, uses current year.
#'
#' @importFrom dplyr mutate filter
#'
#' @export
filter_cohorts <- function(data, first_year = NULL, last_year = NULL) {

    ## TODO Avoid dry
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

    data_prepared <- data |>
        mutate(COHORT_startjaar = as.numeric(substr(COHORT_naam, 1, 4))) |>
        filter(COHORT_startjaar >= first_year,
               COHORT_startjaar <= last_year)

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


parse_result_pct <- function(teams_results) {
    team_results_prepared <- teams_results |>
        mutate(
            TEAM_startersresultaat_1_jaars = parse_number(TEAM_startersresultaat_1_jaars_omschrijving) * 100
        )

    save_prepared(team_results_prepared)

    return(team_results_prepared)

}

format_school_year_name <- function(teams_results) {

    team_results_prepared <- teams_results |>
        mutate(SCHOOLJAAR_naam = str_replace(SCHOOLJAAR_naam_met_streep, " - ", "/"))

    save_prepared(team_results_prepared)

    return(team_results_prepared)

}

add_cohort_start_date <- function(data) {
    data_prepared <- data |>
        mutate(
            COHORT_start_datum = as.Date(paste0(COHORT_startjaar, "-08-01"))
        )

    save_prepared(data_prepared)

    return(data_prepared)
}

calculate_exam_plan_to_start <- function(data) {
    data_prepared <- data |>
        mutate(
            OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_verplicht_begindatum),
            OPLEIDING_examen_plan_keuze_begin_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_keuze_begindatum),
            OPLEIDING_examen_plan_verplicht_eind_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_verplicht_einddatum),
            OPLEIDING_examen_plan_keuze_eind_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_keuze_einddatum),

            across(matches(".*examen_plan.*dagen_tot_start"),
                   ~if_else(. == Inf, NA_integer_, .))
        )

    save_prepared(data_prepared)

    return(data_prepared)

}

calculate_application_to_start <- function(data) {

    data_prepared <- data |>
        mutate(
            VERBINTENIS_aanmelding_begin_datum = as.Date(VERBINTENIS_aanmelding_begin_datum, format = "%Y-%m-%d"),
            VERBINTENIS_aanmelding_laatst_gewijzigd_datum = as.Date(VERBINTENIS_aanmelding_laatst_gewijzigd_datum, format = "%Y-%m-%d"),
            VERBINTENIS_begindatum = as.Date(VERBINTENIS_begindatum, format = "%Y-%m-%d"),
            VERBINTENIS_aanmelding_begin_dagen_tot_start = as.numeric(VERBINTENIS_begindatum - VERBINTENIS_aanmelding_begin_datum),
            VERBINTENIS_aanmelding_afgerond_dagen_tot_start = as.numeric(VERBINTENIS_begindatum - VERBINTENIS_aanmelding_laatst_gewijzigd_datum)
        )

    save_prepared(data_prepared)

    return(data_prepared)

}

calculate_bpv_status_to_start <- function(data) {

    data_prepared <- data |>
        mutate(
            BPV_definitief_dagen_tot_start = as.numeric(BPV_status_definitief_datum - VERBINTENIS_begindatum),
            BPV_volledig_dagen_tot_start = as.numeric(BPV_status_volledig_datum - VERBINTENIS_begindatum),
            BPV_begin_dagen_tot_start = as.numeric(BPV_status_begin_datum - VERBINTENIS_begindatum)
        )

    save_prepared(data_prepared)

    return(data_prepared)

}

#' Calculate BPV Status Against Specific Dates
#'
#' @description
#' Calculate whether BPV (professional practice) statuses were achieved before key dates
#'
#' @param data A data frame containing BPV status dates and cohort information.
#'   Must include columns COHORT_startjaar, BPV_status_definitief_datum, and
#'   BPV_status_volledig_datum.
#'
#' @returns
#' A data frame with additional columns:
#'   \itemize{
#'     \item datum_definitief: October 1st of start year
#'     \item BPV_is_definitief_voor_1_okt: Logical indicating if definitive status was before Oct 1
#'     \item datum_volledig: January 1st of year after start
#'     \item BPV_is_volledig_voor_1_jan: Logical indicating if complete status was before Jan 1
#'   }
#'
#' @importFrom dplyr mutate
#'
#' @export
calculate_bpv_status_to_specific_dates <- function(data) {

    data_prepared <- data |>
        mutate(
            datum_definitief = as.Date(paste0(COHORT_startjaar, "-10-01")),
            BPV_is_definitief_voor_1_okt = BPV_status_definitief_datum < datum_definitief,
            datum_volledig = as.Date(paste0(COHORT_startjaar + 1, "-01-01")),
            BPV_is_volledig_voor_1_jan = BPV_status_volledig_datum < datum_volledig

        )

    save_prepared(data_prepared)

    return(data_prepared)

}

