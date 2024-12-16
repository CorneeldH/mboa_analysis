
#' Transform Advice Data to Enrollment Format
#'
#' @description
#' Transforms advice data into an enrollment format by processing preliminary and
#' definitive advice records. The function groups data by enrollment ID and advice type,
#' then pivots the data to create separate columns for different advice types with their
#' respective creation dates.
#'
#' @param study_advices A data frame containing advice records with the following required columns:
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
transform_advices_to_enrollments <- function(study_advices) {

    enrollments_study_advices <- study_advices |>
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

    save_transformed(enrollments_study_advices)

    return(enrollments_study_advices)
}


#' Transform row per student-change to student-year
#'
#' @description
#' Transforms the rows that represent a change in student status to a row per student-year. The
#' years are determined by given variables (or from the config or defaults)
#' @param students_demographics A tibble or data frame containing student records with columns:
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
transform_students_to_student_year <- function(students_demographics, first_year = NULL, last_year = NULL) {

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
    students_demographics_filtered <- students_demographics |>
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

    student_years_demographics <- students_demographics_filtered |>
        cross_join(peildatums) |>
        filter(
            peildatum >= DEELNEMER_begindatum,
            peildatum <= DEELNEMER_einddatum | is.na(DEELNEMER_einddatum)
        ) |>
        # To remove duplicates due to use of is.na einddatum and cross_join
        distinct(DEELNEMER_ID, peildatum, .keep_all = TRUE)

    save_transformed(student_years_demographics)

    return(student_years_demographics)
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
#' @param special_needs A tibble containing columns:
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
summarise_special_needs <- function(special_needs) {

    enrollments_special_needs <- special_needs |>
        group_by(VERBINTENIS_ID) |>
        summarise(
            VERBINTENIS_passend_onderwijs_kenmerk = paste(unique(VERBINTENIS_passend_onderwijs_kenmerk), collapse = ","),
            .groups = "drop"
        )

    save_transformed(enrollments_special_needs)

    return(enrollments_special_needs)
}

#' Summarise Attendance Observations to weekly percentages
#'
#' @description
#' Aggregates student attendance observations into enrollment-level summary statistics,
#' including attendance percentages, lateness indicators, and duration metrics.
#'
#' @param attendance_observations A tibble containing attendance observations with columns:
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
summarise_observations_to_weekly_attendance <- function(attendance_observations) {

    enrollment_weeks_attendance <- attendance_observations |>
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

    ## TODO DRY
    num_years <- employee_absences_in_weeks |>
        distinct(SCHOOLJAAR_naam) |>
        nrow()

    if (num_years == 1) {
        year <- parse_number(unique(employee_absences_in_weeks$SCHOOLJAAR_naam))
        filename <- paste0("employee_absences_in_weeks_", year)
        save_transformed(employee_absences_in_weeks, filename = filename)
    } else {
        save_transformed(employee_absences_in_weeks)
    }

    return(enrollment_weeks_attendance)

}

summarise_attendance_to_enrollment <- function(attendance_observations) {
    enrollment_years_attendance <- attendance_observations |>
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

    # ## TODO DRY
    # num_years <- enrollment_years_attendance |>
    #     distinct(SCHOOLJAAR_naam) |>
    #     nrow()
    #
    # if (num_years == 1) {
    #     year <- parse_number(unique(enrollment_years_attendance$SCHOOLJAAR_naam))
    #     filename <- paste0("enrollment_years_attendance_", year)
    #     save_transformed(enrollment_years_attendance, filename = filename)
    # } else {
    #     save_transformed(enrollment_years_attendance)
    # }

    return(enrollment_years_attendance)

}

summarise_observations_to_yearly_attendance <- function(attendance_observations) {

    attendance_observations_prepared <- attendance_observations |>
        mutate(
            SCHOOLJAAR_naam = get_school_year(Datum)
        )

    attendance_observations_prepared_split <- attendance_observations_prepared |>
        group_by(SCHOOLJAAR_naam) |>
        group_split()

    enrollment_years_attendance <- attendance_observations_prepared_split |>
        map(summarise_attendance_to_enrollment) |>
        map_dfr(bind_rows)

    save_transformed(enrollment_years_attendance)

    return(enrollment_years_attendance)


}



#' Transform Weekly Attendance Data to Enrollment Level
#'
#' @description
#' Aggregates weekly attendance data to enrollment level, combining group information
#' and pivoting weekly observations
#'
#' @param enrollment_weeks_attendance A tibble containing attendance data with columns:
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
transform_attendance_weekly_to_enrollments <- function(enrollment_weeks_attendance) {

    enrollment_years_attendance_summarised <- enrollment_weeks_attendance |>
        group_by(VERBINTENIS_ID, SCHOOLJAAR_naam) |>
        summarise(
            VERBINTENIS_groep_code = paste0(unique(VERBINTENIS_groep_code), collapse = ","),
            VERBINTENIS_groep_organisatie_eenheid = paste0(unique(VERBINTENIS_groep_organisatie_eenheid), collapse = ","),
            VERBINTENIS_groep_naam = paste0(unique(VERBINTENIS_groep_naam), collapse = ","),
            VERBINTENIS_groep_type_omschrijving = paste0(unique(VERBINTENIS_groep_type_omschrijving), collapse = ","),
            VERBINTENIS_groepdeelname_begindatum = min(VERBINTENIS_groepdeelname_begindatum),
            VERBINTENIS_groepdeelname_einddatum = max(VERBINTENIS_groepdeelname_einddatum),
        )


    enrolmment_years_attendance_pivoted <- enrollment_weeks_attendance |>
        # First pivot the week numbers and their corresponding values
        pivot_wider(
            id_cols = c(VERBINTENIS_ID,
                        SCHOOLJAAR_naam),
            names_from = VERBINTENIS_verzuim_week_nummer,
            values_from = contains("_waarneming_"),
            names_glue = "{.value}_{VERBINTENIS_verzuim_week_nummer}"
        )

    enrollments_years_attendance_weekly <- enrollment_years_attendance_summarised |>
        left_join(enrolmment_years_attendance_pivoted, by = c("VERBINTENIS_ID", "SCHOOLJAAR_naam"))

    ## TODO DRY
    num_years <- enrollments_years_attendance_weekly |>
        distinct(SCHOOLJAAR_naam) |>
        nrow()

    if (num_years == 1) {
        year <- parse_number(unique(enrollments_years_attendance_weekly$SCHOOLJAAR_naam))
        filename <- paste0("enrollments_years_attendance_weekly_", year)
        save_transformed(enrollments_years_attendance_weekly, filename = filename)
    } else {
        save_transformed(enrollments_years_attendance_weekly)
    }

    return(enrollments_years_attendance_weekly)
}


#' Summarize Exam Plan Dates by Programme
#'
#' @description
#' Aggregates exam plan dates for each programme and cohort combination,
#' separating mandatory and optional exam components.
#'
#' @param exam_plans A tibble containing exam plan data with columns:
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
summarise_plan_dates_to_programmes <- function(exam_plans) {
    programmes_exam_plan <- exam_plans |>
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


    save_transformed(programmes_exam_plan)

    return(programmes_exam_plan)
}


#' Transform Prior Education Data to Yearly Records
#'
#' @description
#' Expands prior education records across academic years between specified dates
#'
#' @param students_prior_education A dataframe containing prior education data with columns DEELNEMER_ID
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
transform_prior_education_to_student_year <- function(students_prior_education, first_year = NULL, last_year = NULL) {

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

    if (!inherits(students_prior_education$DEELNEMER_vooropleiding_einddatum, "Date")) {
        students_prior_education <- students_prior_education |>
            mutate(
                DEELNEMER_vooropleiding_einddatum = as.Date(
                    DEELNEMER_vooropleiding_einddatum,
                    format = "%d-%m-%Y"
                )
            )
    }

    # Combine reference dates with prior education data
    student_years_prior_education <- students_prior_education |>
        cross_join(datums_start) |>
        filter(
            !is.na(DEELNEMER_vooropleiding_einddatum),
            # TODO Old prior educations have often an end date in the future, but for the last
            # years this works
            DEELNEMER_vooropleiding_einddatum < COHORT_startdatum
        ) |>
        # Keep only the most recent end date
        ungroup()

    save_transformed(student_years_prior_education)

    return(student_years_prior_education)
}


#' Transform Prior Education and Highest Degree Data
#'
#' @description
#' Processes student educational history data to extract information about prior
#' secondary education (VO) and highest obtained degree.
#'
#' @param student_years_prior_education A data frame containing student educational history with columns:
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
transform_prior_education_vo_and_highest_degree <- function(student_years_prior_education) {
    # First get the records for highest VO
    vo_records <- student_years_prior_education |>
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
    gediplomeerde_records <- student_years_prior_education |>
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

    start_kwalificatie <- student_years_prior_education |>
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
    student_years_prior_education_summarised <- start_kwalificatie |>
        full_join(vo_records, by = join_by(DEELNEMER_ID, COHORT_naam)) |>
        full_join(gediplomeerde_records, by = join_by(DEELNEMER_ID, COHORT_naam))

    return(student_years_prior_education_summarised)
}



#' Summarise Component Data to Contract Level
#'
#' @description
#' Aggregates employee component data to contract level, summing FTE adjustments
#'
#' @param fte_components A data frame containing employee contract components with columns:
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
summarise_components_to_employees <- function(fte_components) {

    employees_contract_extra_fte <- fte_components |>
        group_by(MEDEWERKER_ID,
                 MEDEWERKER_contract_kostenplaats_code,
                 MEDEWERKER_contract_fte_peildatum
        ) |>
        summarise(MEDEWERKER_contract_fte_aanpassing = sum(MEDEWERKER_contract_fte_aanpassing))

    save_transformed(employees_contract_extra_fte)

    return(employees_contract_extra_fte)

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
expand_to_daily <- function(employee_absences) {

    # Create daily sequence
    days <- data.frame(
        datum = seq.Date(
            from = min(employee_absences$SCHOOLJAAR_startdatum),
            to = max(employee_absences$SCHOOLJAAR_einddatum),
            by = "day"
        )
    ) |>
        filter(!wday(datum, week_start = 1) %in% c(6, 7))  # Filter out Saturday (6) and Sunday (7)

    # Cross join full employee_absences with days
    employee_absences_in_days <- employee_absences |>
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

    ## TODO DRY
    num_years <- employee_absences_in_days |>
        distinct(SCHOOLJAAR_naam) |>
        nrow()

    if (num_years == 1) {
        year <- parse_number(unique(employee_absences_in_days$SCHOOLJAAR_naam))
        filename <- paste0("employee_absences_in_days_", year)
        save_transformed(employee_absences_in_days, filename = filename)
    } else {
        save_transformed(employee_absences_in_days)
    }

    return(employee_absences_in_days)
}


#' Aggregate Daily Absence Data to Weekly Summaries
#'
#' @description
#' Convert daily absence records into weekly summaries, calculating average absence
#' percentages and determining absence duration categories
#'
#' @param employee_absences_in_days A data frame containing daily absence records with columns:
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
summarise_employee_absence_to_weeks <- function(employee_absences_in_days) {

    employee_absences_in_weeks <- employee_absences_in_days |>
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

    ## TODO DRY
    num_years <- employee_absences_in_weeks |>
        distinct(SCHOOLJAAR_naam) |>
        nrow()

    if (num_years == 1) {
        year <- parse_number(unique(employee_absences_in_weeks$SCHOOLJAAR_naam))
        filename <- paste0("employee_absences_in_weeks_", year)
        save_transformed(employee_absences_in_weeks, filename = filename)
    } else {
        save_transformed(employee_absences_in_weeks)
    }

    return(employee_absences_in_weeks)
}

summarise_employee_absence_to_years <- function(emplyee_absences_in_days) {

    emplyee_absences_in_years <- emplyee_absences_in_days |>
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
            .groups = "drop"
        )

    ## TODO DRY
    num_years <- emplyee_absences_in_years |>
        distinct(SCHOOLJAAR_naam) |>
        nrow()

    if (num_years == 1) {
        year <- parse_number(unique(emplyee_absences_in_years$SCHOOLJAAR_naam))
        filename <- paste0("emplyee_absences_in_years_", year)
        save_transformed(emplyee_absences_in_years, filename = filename)
    } else {
        save_transformed(emplyee_absences_in_years)
    }


    return(emplyee_absences_in_years)
}


#' Pivot Weekly Data to Yearly Format
#'
#' @description
#' Transforms weekly employee absence data from long to wide format
#'
#' @param employee_absence_in_weeks A tibble containing employee absence data with columns:
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
pivot_weeks_to_years <- function(employee_absence_in_weeks) {

    employee_years_absence_weekly <- employee_absence_in_weeks |>
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
    ## TODO DRY
    num_years <- employee_absences_in_weeks |>
        distinct(SCHOOLJAAR_naam) |>
        nrow()

    if (num_years == 1) {
        year <- parse_number(unique(employee_absences_in_weeks$SCHOOLJAAR_naam))
        filename <- paste0("employee_absences_in_weeks_", year)
        save_transformed(employee_absences_in_weeks, filename = filename)
    } else {
        save_transformed(employee_absences_in_weeks)
    }


    save_transformed(employee_years_absence_weekly)

    return(employee_years_absence_weekly)
}

#' Transform BPV Statuses to Enrollments
#'
#' @description
#' Summarizes professional practice (BPV) status data into enrollment-level information
#'
#' @param bpv_statusses A dataframe containing BPV status data with columns:
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
transform_bpv_statusses_to_enrollments <- function(bpv_statusses) {

    # TODO Split in 3 dataframes and then join, otherwise there were issues

    # Keep all the basic variables
    enrollments_bpv_summarised_basics <- bpv_statusses |>
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
    enrollments_bpv_definitive <- bpv_statusses |>
        filter(BPV_status == "Definitief") |>
        group_by(VERBINTENIS_ID) |>
        summarise(
            BPV_status_definitief_datum = first(BPV_status_begin_datum)
        )

    # Volledig dates
    enrollments_bpv_complete <- bpv_statusses |>
        filter(BPV_status == "Volledig") |>
        group_by(VERBINTENIS_ID) |>
        summarise(
            BPV_status_volledig_datum = first(BPV_status_begin_datum)
        )

    # Join all together
    enrollments_bpv <- enrollments_bpv_summarised_basics |>
        left_join(enrollments_bpv_definitive, by = "VERBINTENIS_ID") |>
        left_join(enrollments_bpv_complete, by = "VERBINTENIS_ID")

    save_transformed(enrollments_bpv)

    return(enrollments_bpv)
}


