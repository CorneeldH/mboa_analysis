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

    return(data_prepared)

}


#' Transform Advice Data to Enrollment Format
#'
#' @description
#' Transforms advice data into an enrollment format by processing preliminary and
#' definitive advice records. The function groups data by enrollment ID and advice type,
#' then pivots the data to create separate columns for different advice types with their
#' respective creation dates.
#'
#' @param data A data frame containing advice records with the following required columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Enrollment identifier
#'     \item VERBINTENIS_advies_voorlopig: Binary indicator for preliminary advice (1 = yes)
#'     \item VERBINTENIS_advies_definitief: Binary indicator for definitive advice (1 = yes)
#'     \item VERBINTENIS_advies_aanmaakdatum: Creation date of the advice
#'   }
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Original enrollment identifier
#'     \item verbintenis_advies_voorlopig_eerste_datum: Date of first preliminary advice
#'     \item verbintenis_advies_definitief_eerste_datum: Date of first definitive advice
#'   }
#'
#' @details
#' The function performs the following transformations:
#' \enumerate{
#'   \item Determines advice type (preliminary or definitive) based on binary indicators
#'   \item Groups data by enrollment ID and advice type
#'   \item Takes the earliest date for each advice type
#'   \item Pivots the data to create separate columns for each advice type
#' }
#'
#' @examples
#' \dontrun{
#' advice_data <- data.frame(
#'   VERBINTENIS_ID = c(1, 1, 2),
#'   VERBINTENIS_advies_voorlopig = c(1, 0, 1),
#'   VERBINTENIS_advies_definitief = c(0, 1, 0),
#'   VERBINTENIS_advies_aanmaakdatum = as.Date(c("2024-01-01", "2024-02-01", "2024-01-15"))
#' )
#'
#' result <- transform_advices_to_enrollments(advice_data)
#' }
#'
#' @importFrom dplyr mutate case_when group_by summarise
#' @importFrom tidyr pivot_wider
#'
#' @export
transform_advices_to_enrollments <- function(data) {

    data_prepared <- data |>
        mutate(
            type = case_when(
                VERBINTENIS_advies_voorlopig == 1 ~ "voorlopig",
                VERBINTENIS_advies_definitief == 1 ~ "definitief"
            )
        ) |>
        group_by(VERBINTENIS_ID, type) |>
        # Take either first() or last() or min() or max() depending on your needs
        summarise(
            VERBINTENIS_advies_aanmaakdatum = min(VERBINTENIS_advies_aanmaakdatum),
            .groups = "drop"
        ) |>
        pivot_wider(
            names_from = type,
            values_from = VERBINTENIS_advies_aanmaakdatum,
            names_prefix = "verbintenis_advies_",
            names_glue = "{.value}_{type}_eerste_datum"
        )

    return(data_prepared)
}


#' Transform row per student-change to student-year
#'
#' @description
#' Transforms the rows that represent a change in student status to a row per student-year. The
#' years are determined by given variables (or from the config or defaults)
#' @param data A tibble or data frame containing student records with columns:
#'   \itemize{
#'     \item DEELNEMER_ID: Student identifier
#'     \item DEELNEMER_begindatum: Start date of student enrollment
#'     \item DEELNEMER_einddatum: End date of student enrollment (optional)
#'   }
#' @param first_year Character string specifying the start year for analysis (format: "YYYY").
#'   If NULL, attempts to read from config, defaults to an error if not found.
#' @param last_year Character string specifying the end year for analysis (format: "YYYY").
#'   If NULL, attempts to read from config, defaults to current year if not found.
#'
#' @return A tibble containing expanded student year records with columns:
#'   \itemize{
#'     \item All original columns from input data
#'     \item peildatum: Reference date (October 1st of each academic year)
#'     \item COHORT_naam: Academic year identifier (e.g., "2019/2020")
#'   }
#'
#' @importFrom dplyr filter cross_join distinct mutate
#' @importFrom lubridate years
#'
#' @export
transform_students_to_student_year <- function(data, first_year = NULL, last_year = NULL) {

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

    SCHOOLJAAR_startdatum <- as.Date(paste0(first_year, "-08-01"))
    first_reference_date <- as.Date(paste0(first_year, "-10-01"))
    last_reference_date <- as.Date(paste0(last_year, "-10-01"))


    # TODO Number of moves is ignored
    data_filtered <- data |>
        filter(DEELNEMER_begindatum >= SCHOOLJAAR_startdatum)

    peildatums <- data.frame(
        peildatum = seq.Date(
            from = first_reference_date,
            to = last_reference_date,
            by = "1 year"
        )
    ) |>
        mutate(COHORT_naam = paste0(format(peildatum, "%Y"),
                                    "/",
                                    format(peildatum + years(1), "%Y")))

    data_prepared <- data_filtered |>
        cross_join(peildatums) |>
        filter(
            peildatum >= DEELNEMER_begindatum,
            peildatum <= DEELNEMER_einddatum | is.na(DEELNEMER_einddatum)
        ) |>
        # To remove duplicates due to use of is.na einddatum and cross_join
        distinct(DEELNEMER_ID, peildatum, .keep_all = TRUE)

    return(data_prepared)
}

#
# transform_statusses_to_enrollments <- function(data) {
#
#     return(data_prepared)
# }

#' Summarize Special Educational Needs Data
#'
#' @description
#' Aggregates special educational needs data by enrollment ID, combining multiple
#' types into a comma-separated string.
#'
#' @param data A tibble containing columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Enrollment identifier
#'     \item VERBINTENIS_passend_onderwijs_type: Special needs type
#'   }
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Enrollment identifier
#'     \item VERBINTENIS_passend_onderwijs_type: Comma-separated list of unique special needs types
#'   }
#'
#' @importFrom dplyr group_by summarize
#'
#' @export
summarise_special_needs <- function(data) {

    data_prepared <- data |>
        group_by(VERBINTENIS_ID) |>
        summarise(
            VERBINTENIS_passend_onderwijs_kenmerk = paste(unique(VERBINTENIS_passend_onderwijs_kenmerk), collapse = ","),
            .groups = "drop"
        )

    return(data_prepared)
}

#' Summarise Attendance Observations to weekly percentages
#'
#' @description
#' Aggregates student attendance observations into enrollment-level summary statistics,
#' including attendance percentages, lateness indicators, and duration metrics.
#'
#' @param data A tibble containing attendance observations with columns:
#'   \itemize{
#'     \item SK_GroepInschrijving: Group enrollment identifier
#'     \item Presentietekst: Attendance status text (e.g., "Aanwezig", "Te laat", "Uitgestuurd")
#'     \item Waarnemingsduur: Duration of observation
#'     \item Datum: Date of observation
#'   }
#'
#' @return A tibble with summarized enrollment data containing:
#'   \itemize{
#'     \item SK_GroepInschrijving: Group enrollment identifier
#'     \item is_laat: Binary indicator (0/1) for any late arrivals
#'     \item is_uitgestuurd: Binary indicator (0/1) for any removals from class
#'     \item Waarnemingsduur: Total valid observation duration (excluding certain statuses)
#'     \item pct_aanwezig: Percentage of time present
#'     \item pct_geoorloofd: Percentage of time with authorized absence
#'     \item pct_ongeoorloofd: Percentage of time with unauthorized absence
#'     \item begin_datum: First observation date
#'     \item eind_datum: Last observation date
#'   }
#'
#' @details
#' The function calculates various attendance metrics:
#' - Binary indicators for lateness and removal from class
#' - Total valid duration excluding "Te laat", "Kort verzuim", and "Uitgestuurd"
#' - Percentage calculations for different attendance statuses
#' - All percentages are rounded to 2 decimal places
#'
#' Attendance types considered:
#' - Present ("Aanwezig")
#' - Authorized absence ("Ziek", "Geoorloofd verzuim")
#' - Unauthorized absence ("Ongeoorloofd verzuim")
#' - Late ("Te laat")
#' - Removed from class ("Uitgestuurd")
#'
#' @importFrom dplyr group_by summarize rename mutate across starts_with
#'
#' @export
summarise_observations_to_weekly_attendance <- function(data) {

    data_summarised <- data |>
        mutate(
            SCHOOLJAAR_naam = get_school_year(Datum),
            VERBINTENIS_verzuim_week_nummer = paste0("week_", sprintf("%02d", isoweek(Datum)))
        ) |>
        group_by(VERBINTENIS_ID, SCHOOLJAAR_naam, VERBINTENIS_verzuim_week_nummer) |>
        summarise(
            # Binary indicators for Te laat and Uitgestuurd
            VERBINTENIS_waarneming_is_laat = as.integer(any(Presentietekst == "Te laat")),
            VERBINTENIS_waarneming_is_uitgestuurd = as.integer(any(Presentietekst == "Uitgestuurd")),
            # Calculate total duration excluding Te laat, Kort verzuim, and Uitgestuurd
            VERBINTENIS_waarneming_totale_duur = sum(Waarnemingsduur[!Presentietekst %in%
                                                           c("Te laat", "Kort verzuim", "Uitgestuurd")]),
            # TODO: "Te laat", "Kort verzuim", "Uitgestuurd" are enclosed in total sum, but
            # maybe they shouldn't be? It was around 0.7 %
            # Calculate total duration excluding Te laat, Kort verzuim, and Uitgestuurd
            VERBINTENIS_waarneming_pct_aanwezig = sum(Waarnemingsduur[Presentietekst == "Aanwezig"]) / VERBINTENIS_waarneming_totale_duur,
            # Calculate weighted percentage "Ziek/Geoorloofd verzuim"
            VERBINTENIS_waarneming_pct_geoorloofd = sum(Waarnemingsduur[Presentietekst %in% c("Ziek", "Geoorloofd verzuim")]) /
                VERBINTENIS_waarneming_totale_duur,
            # Calculate weighted percentage "Ongeoorloofd verzuim"
            VERBINTENIS_waarneming_pct_ongeoorloofd = sum(Waarnemingsduur[Presentietekst == "Ongeoorloofd verzuim"]) /
                VERBINTENIS_waarneming_totale_duur,
            # # basics grouped
            # begin_datum = min(Datum),
            # eind_datum = max(Datum),
            VERBINTENIS_groep_code = paste0(unique(GROEP_groepcode), collapse = ","),
            VERBINTENIS_groep_organisatie_eenheid = paste0(unique(GROEP_organisatie_eenheid), collapse = ","),
            VERBINTENIS_groep_naam = paste0(unique(GROEP_groepnaam), collapse = ","),
            VERBINTENIS_groep_type_omschrijving = paste0(unique(GROEP_type_omschrijving), collapse = ","),
            VERBINTENIS_groepdeelname_begindatum = min(GROEP_groepdeelname_begindatum),
            VERBINTENIS_groepdeelname_einddatum = max(GROEP_groepdeelname_einddatum),
            .groups = "drop"
        ) |>
        mutate(across(contains("_pct_"), round, 2)) |>
        arrange(VERBINTENIS_ID, SCHOOLJAAR_naam, VERBINTENIS_verzuim_week_nummer)

    return(data_summarised)

}

summarise_observations_to_yearly_attendance <- function(data) {

    data_summarised <- data |>
        mutate(
            SCHOOLJAAR_naam = get_school_year(Datum)
            #VERBINTENIS_verzuim_week_nummer = paste0("week_", sprintf("%02d", isoweek(Datum)))
        ) |>
        group_by(VERBINTENIS_ID, SCHOOLJAAR_naam) |>
        summarise(
            # Binary indicators for Te laat and Uitgestuurd
            VERBINTENIS_waarneming_is_laat = as.integer(any(Presentietekst == "Te laat")),
            VERBINTENIS_waarneming_is_uitgestuurd = as.integer(any(Presentietekst == "Uitgestuurd")),
            # Calculate total duration excluding Te laat, Kort verzuim, and Uitgestuurd
            VERBINTENIS_waarneming_totale_duur = sum(Waarnemingsduur[!Presentietekst %in%
                                                                         c("Te laat", "Kort verzuim", "Uitgestuurd")]),
            # TODO: "Te laat", "Kort verzuim", "Uitgestuurd" are enclosed in total sum, but
            # maybe they shouldn't be? It was around 0.7 %
            # Calculate total duration excluding Te laat, Kort verzuim, and Uitgestuurd
            VERBINTENIS_waarneming_pct_aanwezig = sum(Waarnemingsduur[Presentietekst == "Aanwezig"]) / VERBINTENIS_waarneming_totale_duur,
            # Calculate weighted percentage "Ziek/Geoorloofd verzuim"
            VERBINTENIS_waarneming_pct_geoorloofd = sum(Waarnemingsduur[Presentietekst %in% c("Ziek", "Geoorloofd verzuim")]) /
                VERBINTENIS_waarneming_totale_duur,
            # Calculate weighted percentage "Ongeoorloofd verzuim"
            VERBINTENIS_waarneming_pct_ongeoorloofd = sum(Waarnemingsduur[Presentietekst == "Ongeoorloofd verzuim"]) /
                VERBINTENIS_waarneming_totale_duur,
            # # basics grouped
            # begin_datum = min(Datum),
            # eind_datum = max(Datum),
            VERBINTENIS_groep_code = paste0(unique(GROEP_groepcode), collapse = ","),
            VERBINTENIS_groep_organisatie_eenheid = paste0(unique(GROEP_organisatie_eenheid), collapse = ","),
            VERBINTENIS_groep_naam = paste0(unique(GROEP_groepnaam), collapse = ","),
            VERBINTENIS_groep_type_omschrijving = paste0(unique(GROEP_type_omschrijving), collapse = ","),
            VERBINTENIS_groepdeelname_begindatum = min(GROEP_groepdeelname_begindatum),
            VERBINTENIS_groepdeelname_einddatum = max(GROEP_groepdeelname_einddatum),
            .groups = "drop"
        ) |>
        mutate(across(contains("_pct_"), round, 2)) |>
        arrange(VERBINTENIS_ID, SCHOOLJAAR_naam)

    return(data_summarised)

}



#' Transform Weekly Attendance Data to Enrollment Level
#'
#' @description
#' Aggregates weekly attendance data to enrollment level, combining group information
#' and pivoting weekly observations
#'
#' @param data A tibble containing attendance data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Enrollment identifier
#'     \item SCHOOLJAAR_naam: School year
#'     \item VERBINTENIS_groep_* columns: Group information
#'     \item VERBINTENIS_verzuim_week_nummer: Week number
#'     \item *_waarneming_* columns: Observation values
#'   }
#'
#' @returns
#' A tibble with:
#'   \itemize{
#'     \item Summarized group information (concatenated with commas for multiple values)
#'     \item Group participation dates (min begin date, max end date)
#'     \item Pivoted weekly observations
#'   }
#'
#' @importFrom dplyr group_by summarise left_join
#' @importFrom tidyr pivot_wider
#'
#' @export
transform_attendance_weekly_to_enrollments <- function(data) {

    data_summarised <- data |>
        group_by(VERBINTENIS_ID, SCHOOLJAAR_naam) |>
        summarise(
            VERBINTENIS_groep_code = paste0(unique(VERBINTENIS_groep_code), collapse = ","),
            VERBINTENIS_groep_organisatie_eenheid = paste0(unique(VERBINTENIS_groep_organisatie_eenheid), collapse = ","),
            VERBINTENIS_groep_naam = paste0(unique(VERBINTENIS_groep_naam), collapse = ","),
            VERBINTENIS_groep_type_omschrijving = paste0(unique(VERBINTENIS_groep_type_omschrijving), collapse = ","),
            VERBINTENIS_groepdeelname_begindatum = min(VERBINTENIS_groepdeelname_begindatum),
            VERBINTENIS_groepdeelname_einddatum = max(VERBINTENIS_groepdeelname_einddatum),
        )


    data_pivoted <- data |>
        # First pivot the week numbers and their corresponding values
        pivot_wider(
            id_cols = c(VERBINTENIS_ID,
                        SCHOOLJAAR_naam),
            names_from = VERBINTENIS_verzuim_week_nummer,
            values_from = contains("_waarneming_"),
            names_glue = "{.value}_{VERBINTENIS_verzuim_week_nummer}"
        )

    data_prepared <- data_summarised |>
        left_join(data_pivoted, by = c("VERBINTENIS_ID", "SCHOOLJAAR_naam"))

    return(data_prepared)
}


#' Summarize Exam Plan Dates by Programme
#'
#' @description
#' Aggregates exam plan dates for each programme and cohort combination,
#' separating mandatory and optional exam components.
#'
#' @param data A tibble containing exam plan data with columns:
#'   \itemize{
#'     \item OPLEIDING_ID: Programme identifier
#'     \item COHORT_ID: Cohort identifier
#'     \item EXAMENPLAN_begindatum_omschrijving: Start date description (format: "dd-mm-yyyy")
#'     \item EXAMENPLAN_einddatum_omschrijving: End date description (format: "dd-mm-yyyy")
#'     \item EXAMENPLAN_verplicht: Binary indicator for mandatory exams (1) vs optional exams (0)
#'   }
#'
#' @return A tibble with summarized exam plan dates:
#'   \itemize{
#'     \item OPLEIDING_ID: Programme identifier
#'     \item COHORT_ID: Cohort identifier
#'     \item OPLEIDING_examen_plan_verplicht_begindatum: Start date of mandatory exams
#'     \item OPLEIDING_examen_plan_keuze_begindatum: Start date of optional exams
#'     \item OPLEIDING_examen_plan_verplicht_einddatum: End date of mandatory exams
#'     \item OPLEIDING_examen_plan_keuze_einddatum: End date of optional exams
#'   }
#'
#' @details
#' The function converts string dates to Date objects, calculates min/max dates
#' for mandatory and optional exams separately, and handles missing values by
#' converting infinite values to NA.
#'
#' @importFrom dplyr group_by mutate summarise across if_else
#'
#' @export
summarise_plan_dates_to_programmes <- function(data) {
    data_prepared <- data |>
        group_by(OPLEIDING_ID, COHORT_ID) |>
        mutate(EXAMENPLAN_begindatum = as.Date(EXAMENPLAN_begindatum_omschrijving, format = "%d-%m-%Y"),
               EXAMENPLAN_einddatum = as.Date(EXAMENPLAN_einddatum_omschrijving, format = "%d-%m-%Y")) |>
        # This creates warnings and Inf values due to missing "verplicht" or "keuze" in group
        summarise(
            OPLEIDING_examen_plan_verplicht_begindatum = suppressWarnings(as.Date(min(EXAMENPLAN_begindatum[EXAMENPLAN_verplicht == 1], na.rm = TRUE))),
            OPLEIDING_examen_plan_keuze_begindatum = suppressWarnings(as.Date(min(EXAMENPLAN_begindatum[EXAMENPLAN_verplicht == 0], na.rm = TRUE))),
            OPLEIDING_examen_plan_verplicht_einddatum = suppressWarnings(as.Date(max(EXAMENPLAN_einddatum[EXAMENPLAN_verplicht == 1], na.rm = TRUE))),
            OPLEIDING_examen_plan_keuze_einddatum = suppressWarnings(as.Date(max(EXAMENPLAN_einddatum[EXAMENPLAN_verplicht == 0], na.rm = TRUE))),
            .groups = 'drop'
        ) |>
        mutate(across(contains("datum"),
                      ~if_else(. == Inf, NA_Date_, .)))

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

    return(data_prepared)
}




#' Transform Prior Education Data to Yearly Records
#'
#' @description
#' Expands prior education records across academic years between specified dates
#'
#' @param data A dataframe containing prior education data with columns DEELNEMER_ID
#'   and DEELNEMER_vooropleiding_einddatum
#' @param first_year Optional. Starting year for the analysis. If NULL, retrieved from config
#' @param last_year Optional. Ending year for the analysis. If NULL, retrieved from config
#'   or defaults to current year
#'
#' @returns A dataframe with expanded yearly records, including:
#'   \itemize{
#'     \item Original columns from input data
#'     \item COHORT_start_datum: Reference start date for each academic year
#'     \item COHORT_naam: Academic year in format "YYYY/YYYY"
#'   }
#'
#' @importFrom dplyr mutate cross_join filter group_by slice_max ungroup
#' @importFrom lubridate years
#'
#' @export
transform_prior_education_to_student_year <- function(data, first_year = NULL, last_year = NULL) {

    # Check config for years if not provided
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


    datums_start <- data.frame(
        COHORT_startdatum = seq.Date(
            from = as.Date(paste0(first_year, "-08-01")),
            to = as.Date(paste0(last_year, "-08-01")),
            by = "1 year"
        )
    ) |>
        mutate(
            COHORT_naam = paste0(
                format(COHORT_startdatum, "%Y"),
                "/",
                format(COHORT_startdatum + years(1), "%Y")
            )
        )

    if (!inherits(data$DEELNEMER_vooropleiding_einddatum, "Date")) {
        data <- data |>
            mutate(
                DEELNEMER_vooropleiding_einddatum = as.Date(
                    DEELNEMER_vooropleiding_einddatum,
                    format = "%d-%m-%Y"
                )
            )
    }

    # Combine reference dates with prior education data
    data_prepared <- data |>
        cross_join(datums_start) |>
        filter(
            !is.na(DEELNEMER_vooropleiding_einddatum),
            # TODO Old prior educations have often an end date in the future, but for the last
            # years this works
            DEELNEMER_vooropleiding_einddatum < COHORT_startdatum
        ) |>
        # Keep only the most recent end date
        ungroup()

    return(data_prepared)
}





#' Transform Prior Education and Highest Degree Data
#'
#' @description
#' Processes student educational history data to extract information about prior
#' secondary education (VO) and highest obtained degree.
#'
#' @param data A data frame containing student educational history with columns:
#'   \itemize{
#'     \item DEELNEMER_ID: Student identifier
#'     \item DEELNEMER_vooropleiding_hoogst_vo: Indicator for highest secondary education
#'     \item DEELNEMER_vooropleiding_hoogst_gediplomeerde: Indicator for highest diploma
#'     \item Various columns containing education details and dates
#'   }
#'
#' @returns A data frame containing:
#'   \itemize{
#'     \item DEELNEMER_ID: Student identifier
#'     \item start_kwalificatie: Starting qualification status ("Ja"/"Nee"/NA)
#'     \item Various columns for VO (secondary) and highest diploma information
#'   }
#'
#' @importFrom dplyr filter select group_by ungroup slice_max slice_tail full_join summarise case_when join_by
#'
#' @export
transform_prior_education_vo_and_highest_degree <- function(data) {
    # First get the records for highest VO
    vo_records <- data |>
        filter(DEELNEMER_vooropleiding_hoogst_vo == 1) |>
        select(
            DEELNEMER_ID,
            COHORT_naam,
            DEELNEMER_vooropleiding_vo_opleiding = DEELNEMER_vooropleiding_vooropleiding,
            DEELNEMER_vooropleiding_vo_categorie = DEELNEMER_vooropleiding_vooropleidings_categorie,
            DEELNEMER_vooropleiding_vo_aanbieder_categorie = DEELNEMER_vooropleiding_soort,
            DEELNEMER_vooropleiding_vo_soort_naam = DEELNEMER_vooropleiding_soort_naam,
            DEELNEMER_vooropleiding_vo_soort_code = DEELNEMER_vooropleiding_soort_code,
            DEELNEMER_vooropleiding_vo_begin_datum = DEELNEMER_vooropleiding_begin_datum,
            DEELNEMER_vooropleiding_vo_einddatum = DEELNEMER_vooropleiding_einddatum
        ) |>
        group_by(DEELNEMER_ID, COHORT_naam) |>
        slice_max(DEELNEMER_vooropleiding_vo_begin_datum, n = 1) |>
        slice_tail(n = 1) |>
        ungroup()

    # Then get the records for highest gediplomeerde
    gediplomeerde_records <- data |>
        filter(DEELNEMER_vooropleiding_hoogst_gediplomeerde == 1) |>
        select(
            DEELNEMER_ID,
            COHORT_naam,
            DEELNEMER_vooropleiding_hoogste_diploma = DEELNEMER_vooropleiding_vooropleiding,
            DEELNEMER_vooropleiding_hoogste_diploma_categorie = DEELNEMER_vooropleiding_vooropleidings_categorie,
            DEELNEMER_vooropleiding_hoogste_diploma_aanbieder_categorie = DEELNEMER_vooropleiding_soort,
            DEELNEMER_vooropleiding_hoogste_diploma_soort_naam = DEELNEMER_vooropleiding_soort_naam,
            DEELNEMER_vooropleiding_hoogste_diploma_soort_code = DEELNEMER_vooropleiding_soort_code,
            DEELNEMER_vooropleiding_hoogste_diploma_begin_datum = DEELNEMER_vooropleiding_begin_datum,
            DEELNEMER_vooropleiding_hoogste_diploma_einddatum = DEELNEMER_vooropleiding_einddatum
        ) |>
        group_by(DEELNEMER_ID, COHORT_naam) |>
        slice_max(DEELNEMER_vooropleiding_hoogste_diploma_begin_datum, n = 1) |>
        slice_tail(n = 1) |>
        ungroup()

    start_kwalificatie <- data |>
        group_by(DEELNEMER_ID, COHORT_naam) |>
        summarise(
            start_kwalificatie = case_when(
                any(DEELNEMER_vooropleiding_startkwalificatie_behaald == "Ja") ~ "Ja",
                any(DEELNEMER_vooropleiding_startkwalificatie_behaald == "Nee") ~ "Nee",
                TRUE ~ NA_character_
            )
        ) |>
        ungroup()

    # Join the two datasets
    data_prepared <- start_kwalificatie |>
        full_join(vo_records, by = join_by(DEELNEMER_ID, COHORT_naam)) |>
        full_join(gediplomeerde_records, by = join_by(DEELNEMER_ID, COHORT_naam))

    return(data_prepared)
}



#' Summarise Component Data to Contract Level
#'
#' @description
#' Aggregates employee component data to contract level, summing FTE adjustments
#'
#' @param data A data frame containing employee contract components with columns:
#'   MEDEWERKER_ID, MEDEWERKER_contract_kostenplaats_code,
#'   MEDEWERKER_contract_fte_peildatum, and MEDEWERKER_contract_fte_aanpassing
#'
#' @returns
#' A grouped tibble summarizing FTE adjustments by employee, cost center, and
#' reference date FTE
#'
#' @importFrom dplyr group_by summarise
#'
#' @export
summarise_components_to_employees <- function(data) {

    data_prepared <- data |>
        group_by(MEDEWERKER_ID,
                 MEDEWERKER_contract_kostenplaats_code,
                 MEDEWERKER_contract_fte_peildatum
        ) |>
        summarise(MEDEWERKER_contract_fte_aanpassing = sum(MEDEWERKER_contract_fte_aanpassing))

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

    return(data_in_years_split)
}



split_observations_in_school_years <- function(data) {

    data_with_school_year <- data |>
        mutate(
            SCHOOLJAAR_naam = get_school_year(Datum)
        )

    data_in_years_split <- data_with_school_year |>
        group_by(SCHOOLJAAR_naam) |>
        group_split()

    return(data_in_years_split)

}

#' Expand Absence Records to Daily Level
#'
#' @description
#' Transform employee absence records into daily observations, calculating absence
#' percentages and duration categories for each workday.
#'
#' @param data A data frame containing employee absence records with columns:
#'   MEDEWERKER_ID, MEDEWERKER_eerste_verzuimdag, MEDEWERKER_laatste_verzuimdag,
#'   MEDEWERKER_percentage_verzuim, MEDEWERKER_contract_kostenplaats_code, SCHOOLJAAR_naam
#'
#' @returns A data frame with daily absence records containing:
#'   \itemize{
#'     \item MEDEWERKER_ID: Employee identifier
#'     \item MEDEWERKER_contract_kostenplaats_code: Cost center code
#'     \item datum: Date
#'     \item SCHOOLJAAR_naam: School year
#'     \item verzuim_percentage: Absence percentage (0-1)
#'     \item verzuim_duur: Duration category ("kort", "middellang", or "lang")
#'   }
#'
#' @importFrom dplyr cross_join mutate case_when group_by summarise filter
#' @importFrom lubridate wday
#'
#' @export
expand_to_daily <- function(data) {

    # Create daily sequence
    days <- data.frame(
        datum = seq.Date(
            from = min(data$SCHOOLJAAR_startdatum),
            to = max(data$SCHOOLJAAR_einddatum),
            by = "day"
        )
    ) |>
        filter(!wday(datum, week_start = 1) %in% c(6, 7))  # Filter out Saturday (6) and Sunday (7)

    # Cross join full data with days
    data_in_days <- data |>
        cross_join(days) |>
        mutate(
            verzuim_percentage = case_when(
                datum >= MEDEWERKER_eerste_verzuimdag &
                    datum <= MEDEWERKER_laatste_verzuimdag ~ as.numeric(MEDEWERKER_percentage_verzuim),
                TRUE ~ 0
            ),
            verzuim_duur = case_when(
                verzuim_percentage > 0 ~ case_when(
                    datum - MEDEWERKER_eerste_verzuimdag < 7 ~ "kort",
                    datum - MEDEWERKER_eerste_verzuimdag < 42 ~ "middellang",
                    TRUE ~ "lang"
                ),
                TRUE ~ NA_character_
            )
        ) |>
        # Sometimes persons have multiple absences at the same time, we fix this by summarizing
        group_by(
            MEDEWERKER_ID,
            # MEDEWERKER_contract_kostenplaats_code,
            datum,
            SCHOOLJAAR_naam
        ) |>
        summarise(
            verzuim_percentage = ifelse(sum(verzuim_percentage) > 1, 1, sum(verzuim_percentage)),
            verzuim_duur = case_when(
                any(verzuim_duur == "lang") ~ "lang",
                any(verzuim_duur == "middellang") ~ "middellang",
                any(verzuim_duur == "kort") ~ "kort",
                TRUE ~ NA_character_),
            .groups = "drop",
        )

    return(data_in_days)
}


#' Aggregate Daily Absence Data to Weekly Summaries
#'
#' @description
#' Convert daily absence records into weekly summaries, calculating average absence
#' percentages and determining absence duration categories
#'
#' @param data A data frame containing daily absence records with columns:
#'   - datum: Date of absence
#'   - MEDEWERKER_ID: Employee identifier
#'   - MEDEWERKER_contract_kostenplaats_code: Cost center code
#'   - SCHOOLJAAR_naam: School year
#'   - verzuim_percentage: Daily absence percentage
#'   - verzuim_duur: Absence duration category ("kort", "middellang", "lang")
#'
#' @returns A data frame with weekly absence summaries per employee and cost center:
#'   - MEDEWERKER_ID: Employee identifier
#'   - MEDEWERKER_contract_kostenplaats_code: Cost center code
#'   - SCHOOLJAAR_naam: School year
#'   - verzuim_percentage: Weekly average absence percentage
#'   - verzuim_duur: Most severe absence duration in the week
#'
#' @importFrom dplyr mutate group_by summarise select case_when n
#' @importFrom lubridate floor_date days isoweek
#'
#' @export
summarise_employee_absence_to_weeks <- function(data) {

    data_in_weeks <- data |>
        mutate(
            MEDEWERKER_verzuim_week_nummer = paste0("week_", sprintf("%02d", isoweek(datum)))
        ) |>
        group_by(
            MEDEWERKER_ID,
            MEDEWERKER_verzuim_week_nummer,
            SCHOOLJAAR_naam
        ) |>
        summarise(
            days_in_week = n(),
            MEDEWERKER_verzuim_totaal = sum(verzuim_percentage),
            MEDEWERKER_verzuim_percentage = MEDEWERKER_verzuim_totaal / days_in_week,
            MEDEWERKER_verzuim_duur = case_when(
                any(verzuim_duur == "lang") ~ "lang",
                any(verzuim_duur == "middellang") ~ "middellang",
                any(verzuim_duur == "kort") ~ "kort",
                TRUE ~ NA_character_
            ),
            .groups = "drop"
        ) |>
        select(-days_in_week)

    return(data_in_weeks)
}

summarise_employee_absence_to_years <- function(data) {

    data_summarised <- data |>
        group_by(
            MEDEWERKER_ID,
            SCHOOLJAAR_naam
        ) |>
        summarise(
            days_in_year = n(),
            MEDEWERKER_verzuim_lang = sum(verzuim_percentage[verzuim_duur == "lang"], na.rm = TRUE),
            MEDEWERKER_verzuim_pct_lang = MEDEWERKER_verzuim_lang / days_in_year,
            MEDEWERKER_verzuim_middellang = sum(verzuim_percentage[verzuim_duur == "middellang"], na.rm = TRUE),
            MEDEWERKER_verzuim_pct_middellang = MEDEWERKER_verzuim_middellang / days_in_year,
            MEDEWERKER_verzuim_kort = sum(verzuim_percentage[verzuim_duur == "kort"], na.rm = TRUE),
            MEDEWERKER_verzuim_pct_kort = MEDEWERKER_verzuim_kort / days_in_year,

            #MEDEWERKER_verzuim_percentage_totaal = sum(verzuim_percentage) / days_in_year,
            #MEDEWERKER_verzuim_lang = sum(if_else(verzuim_duur == "lang", verzuim_percentage, 0)),
            #MEDEWERKER_verzuim_percentage_lang = sum(if_else(verzuim_duur == "lang", verzuim_percentage, 0)) / days_in_year,
            #MEDEWERKER_verzuim_percentage_middellang = sum(if_else(verzuim_duur == "middellang", verzuim_percentage, 0)) / days_in_year,
            #MEDEWERKER_verzuim_percentage_kort = sum(if_else(verzuim_duur == "kort", verzuim_percentage, 0)) / days_in_year,

            # MEDEWERKER_verzuim_percentage = MEDEWERKER_verzuim_totaal / days_in_week,
            # MEDEWERKER_verzuim_duur = case_when(
            #     any(verzuim_duur == "lang") ~ "lang",
            #     any(verzuim_duur == "middellang") ~ "middellang",
            #     any(verzuim_duur == "kort") ~ "kort",
            #     TRUE ~ NA_character_
            # ),
            .groups = "drop"
        ) #|>
        #select(-days_in_year)

    return(data_summarised)
}


#' Pivot Weekly Data to Yearly Format
#'
#' @description
#' Transforms weekly employee absence data from long to wide format
#'
#' @param data A tibble containing employee absence data with columns:
#'   MEDEWERKER_ID, MEDEWERKER_contract_kostenplaats_code, SCHOOLJAAR_naam,
#'   MEDEWERKER_verzuim_week_nummer, MEDEWERKER_verzuim_totaal,
#'   MEDEWERKER_verzuim_percentage, MEDEWERKER_verzuim_duur
#'
#' @returns
#' A wide-format tibble with employee identifiers as rows and weekly metrics
#' as columns
#'
#' @importFrom tidyr pivot_wider
#'
#' @export
pivot_weeks_to_years <- function(data) {

    data_pivoted <- data |>
        # First pivot the week numbers and their corresponding values
        pivot_wider(
            id_cols = c(MEDEWERKER_ID,
                        # MEDEWERKER_contract_kostenplaats_code,
                        SCHOOLJAAR_naam),
            names_from = MEDEWERKER_verzuim_week_nummer,
            values_from = c(MEDEWERKER_verzuim_totaal,
                            MEDEWERKER_verzuim_percentage,
                            MEDEWERKER_verzuim_duur),
            names_glue = "{.value}_{MEDEWERKER_verzuim_week_nummer}"
        )


    return(data_pivoted)
}

#' Transform BPV Statuses to Enrollments
#'
#' @description
#' Summarizes professional practice (BPV) status data into enrollment-level information
#'
#' @param data A dataframe containing BPV status data with columns:
#'   VERBINTENIS_ID, BPV_ID, TEAM_ID, BPV_omvang, BPV_status_begin_datum,
#'   BPV_verwachte_eind_datum, and BPV_status
#'
#' @returns
#' A dataframe with one row per enrollment (VERBINTENIS_ID) containing:
#'   \itemize{
#'     \item Basic information from the first record
#'     \item Date when status became "Definitief"
#'     \item Date when status became "Volledig"
#'   }
#'
#' @importFrom dplyr group_by arrange summarise filter first left_join
#'
#' @export
transform_bpv_statusses_to_enrollments <- function(data) {

    # TODO Split in 3 dataframes and then join, otherwise there were issues

    # Keep all the basic variables
    data_summarised_basics <- data |>
        group_by(VERBINTENIS_ID) |>
        arrange(BPV_status_begin_datum) |>
        summarise(
            BPV_ID = first(BPV_ID),
            BPV_omvang = first(BPV_omvang),
            BPV_status_begin_datum = first(BPV_status_begin_datum),
            BPV_verwachte_eind_datum = first(BPV_verwachte_eind_datum),
            .groups = "drop"
        )
    # Definitief dates
    data_definitive <- data |>
        filter(BPV_status == "Definitief") |>
        group_by(VERBINTENIS_ID) |>
        summarise(
            BPV_status_definitief_datum = first(BPV_status_begin_datum)
        )

    # Volledig dates
    data_complete <- data |>
        filter(BPV_status == "Volledig") |>
        group_by(VERBINTENIS_ID) |>
        summarise(
            BPV_status_volledig_datum = first(BPV_status_begin_datum)
        )

    # Join all together
    data_summarised <- data_summarised_basics |>
        left_join(data_definitive, by = "VERBINTENIS_ID") |>
        left_join(data_complete, by = "VERBINTENIS_ID")


    return(data_summarised)
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
}


add_cohort_start_date <- function(data) {
    data_prepared <- data |>
        mutate(
            COHORT_start_datum = as.Date(paste0(COHORT_startjaar, "-08-01"))
        )

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

    return(data_prepared)

}

calculate_bpv_status_to_start <- function(data) {

    data_prepared <- data |>
        mutate(
            BPV_definitief_dagen_tot_start = as.numeric(BPV_status_definitief_datum - VERBINTENIS_begindatum),
            BPV_volledig_dagen_tot_start = as.numeric(BPV_status_volledig_datum - VERBINTENIS_begindatum),
            BPV_begin_dagen_tot_start = as.numeric(BPV_status_begin_datum - VERBINTENIS_begindatum)
        )

    return(data_prepared)

}

calculate_bpv_status_to_specific_dates <- function(data) {

    data_prepared <- data |>
        mutate(
            datum_definitief = as.Date(paste0(COHORT_startjaar, "-10-01")),
            BPV_is_definitief_voor_1_okt = BPV_status_definitief_datum < datum_definitief,
            datum_volledig = as.Date(paste0(COHORT_startjaar + 1, "-01-01")),
            BPV_is_volledig_voor_1_jan = BPV_status_volledig_datum < datum_volledig

        )
}
