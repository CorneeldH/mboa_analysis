
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

    save_transformed_and_comment(enrollments_study_advices)

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
#' @importFrom dplyr filter cross_join distinct mutate desc
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
        mutate(
            DEELNEMER_begindatum = as.Date(DEELNEMER_begindatum),
            DEELNEMER_einddatum = as.Date(DEELNEMER_einddatum)
        )
        #filter(DEELNEMER_begindatum >= SCHOOLJAAR_startdatum)

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

    student_demographics_yearly <- students_demographics_filtered |>
        cross_join(peildatums) |>
        filter(
            #peildatum >= DEELNEMER_begindatum,
            peildatum <= DEELNEMER_einddatum | is.na(DEELNEMER_einddatum)
        ) |>
        arrange(DEELNEMER_ID, peildatum, desc(DEELNEMER_begindatum)) |>
        # To remove duplicates due to use of is.na einddatum and cross_join
        distinct(DEELNEMER_ID, peildatum, .keep_all = TRUE)

    save_transformed_and_comment(student_demographics_yearly)

    return(student_demographics_yearly)
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

    save_transformed_and_comment(enrollments_special_needs)

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
#' @param first_n_weeks Integer specifying the number of weeks to include in the analysis
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
summarise_observations_to_weekly_attendance <- function(attendance_observations, first_n_weeks = 14) {

    enrollment_weeks_attendance <- attendance_observations |>
        mutate(
            SCHOOLJAAR_naam = get_school_year(Datum),
            # Create a school week number starting from August 1st
            school_start_date = as.Date(paste0(substr(SCHOOLJAAR_naam, 1, 4), "-08-01")),
            school_start_week = isoweek(school_start_date),
            week_number = isoweek(Datum),
            # Calculate adjusted week number relative to school year start
            adjusted_week = ifelse(
                week_number >= school_start_week,
                week_number - school_start_week + 1,
                week_number + (52 - school_start_week) + 1
            ),
            # Create formatted week number for column names
            VERBINTENIS_verzuim_week_nummer = paste0("week_", sprintf("%02d", adjusted_week))
        ) |>
        # Only include the first n weeks of the school year
        filter(adjusted_week <= first_n_weeks) |>
        group_by(VERBINTENIS_ID, SCHOOLJAAR_naam, VERBINTENIS_verzuim_week_nummer) |>
        summarise(
            # Binary indicators for Te laat and Uitgestuurd
            VERBINTENIS_waarneming_is_laat = as.integer(any(Presentietekst == "Te laat")),
            VERBINTENIS_waarneming_is_uitgestuurd = as.integer(any(Presentietekst == "Uitgestuurd")),
            # Calculate attendance %
            VERBINTENIS_waarneming_totale_duur = sum(Waarnemingsduur),
            VERBINTENIS_waarneming_pct_aanwezig = sum(Waarnemingsduur[Presentietekst == "Aanwezig"]) / VERBINTENIS_waarneming_totale_duur,
            VERBINTENIS_waarneming_pct_geoorloofd = sum(Waarnemingsduur[Presentietekst %in% c("Ziek", "Geoorloofd verzuim")]) /
                VERBINTENIS_waarneming_totale_duur,
            VERBINTENIS_waarneming_pct_ongeoorloofd = sum(Waarnemingsduur[Presentietekst == "Ongeoorloofd verzuim"]) /
                VERBINTENIS_waarneming_totale_duur,
            # Add other variables
            VERBINTENIS_groep_code = paste0(unique(GROEP_groepcode), collapse = ","),
            VERBINTENIS_groep_organisatie_eenheid = paste0(unique(GROEP_organisatie_eenheid), collapse = ","),
            VERBINTENIS_groep_naam = paste0(unique(GROEP_groepnaam), collapse = ","),
            VERBINTENIS_groep_type_omschrijving = paste0(unique(GROEP_type_omschrijving), collapse = ","),
            VERBINTENIS_groepdeelname_begindatum = min(GROEP_groepdeelname_begindatum),
            VERBINTENIS_groepdeelname_einddatum = max(GROEP_groepdeelname_einddatum),
            .groups = "drop"
        ) |>
        mutate(across(contains("_pct_"), round, 2)
               ) |>
        arrange(VERBINTENIS_ID, SCHOOLJAAR_naam, VERBINTENIS_verzuim_week_nummer)

    return(enrollment_weeks_attendance)

}

#' Summarize Attendance Records by Enrollment
#'
#' @description
#' Aggregates student attendance observations to create enrollment-level summaries
#' by school year
#'
#' @param attendance_observations A data frame containing attendance records with columns:
#'   - VERBINTENIS_ID: Enrollment ID
#'   - Datum: Date of attendance
#'   - Presentietekst: Attendance status
#'   - Waarnemingsduur: Duration of observation
#'   - GROEP_* columns: Group-related information
#'
#' @returns
#' A data frame with summarized attendance metrics per enrollment and school year:
#'   - Binary indicators for late arrival and removal from class
#'   - Total duration of attendance
#'   - Percentage of present, authorized, and unauthorized absences
#'   - Group information including codes, names, and dates
#'
#' @importFrom dplyr mutate group_by summarise across arrange
#'
#' @export
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

    return(enrollment_years_attendance)

}

#' Summarize Attendance Observations to Yearly Statistics
#'
#' @description
#' Transform daily attendance observations into yearly summaries per enrollment
#'
#' @param attendance_observations A data frame containing attendance observations with
#' at least a 'Datum' (date) column
#'
#' @returns
#' A data frame of yearly attendance statistics per enrollment. The data is also
#' saved to disk using `save_transformed_and_comment()`.
#'
#' @importFrom dplyr mutate group_by group_split bind_rows
#' @importFrom purrr map map_dfr
#'
#' @export
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

    save_transformed_and_comment(enrollment_years_attendance)

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


    save_transformed_and_comment(programmes_exam_plan)

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
    student_prior_education_yearly_expanded <- students_prior_education |>
        cross_join(datums_start) |>
        filter(
            !is.na(DEELNEMER_vooropleiding_einddatum),
            # TODO Old prior educations have often an end date in the future, but for the last
            # years this works
            DEELNEMER_vooropleiding_einddatum < COHORT_startdatum
        ) |>
        # Keep only the most recent end date
        ungroup()

    save_transformed_and_comment(student_prior_education_yearly_expanded)

    return(student_prior_education_yearly_expanded)
}


#' Transform Prior Education and Highest Degree Data
#'
#' @description
#' Processes student educational history data to extract information about prior
#' secondary education (VO) and highest obtained degree.
#'
#' @param student_prior_education_yearly_expanded A data frame containing student educational history with columns:
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
transform_prior_education_vo_and_highest_degree <- function(student_prior_education_yearly_expanded) {
    # First get the records for highest VO
    vo_records <- student_prior_education_yearly_expanded |>
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
    gediplomeerde_records <- student_prior_education_yearly_expanded |>
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

    overige_records <- student_prior_education_yearly_expanded |>
        select(
            DEELNEMER_ID,
            COHORT_naam,
            DEELNEMER_vooropleiding_hoogst =  DEELNEMER_vooropleiding_vooropleiding,
            DEELNEMER_vooropleiding_hoogste_diploma_soort = DEELNEMER_vooropleiding_vooropleidings_categorie,
            DEELNEMER_vooropleiding_diploma_behaald,
            DEELNEMER_vooropleiding_einddatum
        ) |>
        group_by(DEELNEMER_ID, COHORT_naam) |>
        slice_max(DEELNEMER_vooropleiding_einddatum, n = 1) |>
        slice_tail(n = 1) |>
        ungroup() |>
        select(-DEELNEMER_vooropleiding_einddatum) |>
        mutate(DEELNEMER_vooropleiding_categorie = case_when(
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO_BB" ~ "VMBO_Basis",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO" & str_detect(DEELNEMER_vooropleiding_hoogst, "[Bb]asis") ~ "VMBO_Basis",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO_KB" ~ "VMBO_Kader",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO" & str_detect(DEELNEMER_vooropleiding_hoogst, "[Kk]ader") ~ "VMBO_Kader",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("VMBO_GL") ~ "VMBO_Gemengd",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO" & str_detect(DEELNEMER_vooropleiding_hoogst, "[Gg]emengd") ~ "VMBO_Gemengd",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("VMBO_TL", "VMBOTL") ~ "VMBO_Theoretisch",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO" & str_detect(DEELNEMER_vooropleiding_hoogst, "[Tt]heo") ~ "VMBO_Theoretisch",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("HAVO") ~ "HAVO",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("MBO-1") ~ "MBO1",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "MBO12" & str_detect(DEELNEMER_vooropleiding_hoogst, "1") ~ "MBO1",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "MBO12" ~ "MBO2",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "MBO-2" ~ "MBO2",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("MBO-3", "MBO-4", "MBO34") ~ "MBO34",
            DEELNEMER_vooropleiding_hoogste_diploma_soort == "VMBO" ~ "MBO34",
            DEELNEMER_vooropleiding_hoogst == "Praktijkonderwijs" ~ "Praktijk",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("Basisonderwijs", "Basisvorming") & str_detect(DEELNEMER_vooropleiding_hoogst, "MBO Niveau 2|Niet te achterhalen|Nog onbekend") ~ "Basis",
            DEELNEMER_vooropleiding_hoogste_diploma_soort %in% c("Basisonderwijs", "Basisvorming", "Geen", "VSO", "VO") ~ "Basis",
            is.na(DEELNEMER_vooropleiding_hoogste_diploma_soort) ~ "Onbekend",
            .default = "Overig"
        ))

    start_kwalificatie <- student_prior_education_yearly_expanded |>
        group_by(DEELNEMER_ID, COHORT_naam) |>
        summarise(
            start_kwalificatie = case_when(
                any(DEELNEMER_vooropleiding_startkwalificatie_behaald == "Ja") ~ "Ja",
                any(DEELNEMER_vooropleiding_startkwalificatie_behaald == "Nee") ~ "Nee",
                TRUE ~ NA_character_
            )
        ) |>
        ungroup()

    # Join the three datasets
    student_prior_education_yearly <- start_kwalificatie |>
        full_join(vo_records, by = join_by(DEELNEMER_ID, COHORT_naam)) |>
        full_join(gediplomeerde_records, by = join_by(DEELNEMER_ID, COHORT_naam)) |>
        full_join(overige_records, by = join_by(DEELNEMER_ID, COHORT_naam))


    save_transformed_and_comment(student_prior_education_yearly)

    return(student_prior_education_yearly)
}



#' Summarise Component Data to Contract Level
#'
#' @description
#' Aggregates employee component data to contract level, summing FTE adjustments
#'
#' @param fte_components A data frame containing employee contract components with columns:
#'   MEDEWERKER_ID, TEAM_kostenplaats_code,
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
                 TEAM_kostenplaats_code,
                 MEDEWERKER_contract_fte_peildatum
        ) |>
        summarise(MEDEWERKER_contract_fte_aanpassing = sum(MEDEWERKER_contract_fte_aanpassing))

    save_transformed_and_comment(employees_contract_extra_fte)

    return(employees_contract_extra_fte)

}


#' Expand Absence Records to Daily Level
#'
#' @description
#' Transform employee absence records into daily observations, calculating absence
#' percentages and duration categories for each workday.
#'
#' @param employee_absences A data frame containing employee absence records with columns:
#'   MEDEWERKER_ID, MEDEWERKER_eerste_verzuimdag, MEDEWERKER_laatste_verzuimdag,
#'   MEDEWERKER_percentage_verzuim, TEAM_kostenplaats_code, SCHOOLJAAR_naam
#'
#' @returns A data frame with daily absence records containing:
#'   \itemize{
#'     \item MEDEWERKER_ID: Employee identifier
#'     \item TEAM_kostenplaats_code: Cost center code
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
            # TEAM_kostenplaats_code,
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
        save_transformed_and_comment(employee_absences_in_days, filename = filename)
    } else {
        save_transformed_and_comment(employee_absences_in_days)
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
#'   - TEAM_kostenplaats_code: Cost center code
#'   - SCHOOLJAAR_naam: School year
#'   - verzuim_percentage: Daily absence percentage
#'   - verzuim_duur: Absence duration category ("kort", "middellang", "lang")
#'
#' @returns A data frame with weekly absence summaries per employee and cost center:
#'   - MEDEWERKER_ID: Employee identifier
#'   - TEAM_kostenplaats_code: Cost center code
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
        save_transformed_and_comment(employee_absences_in_weeks, filename = filename)
    } else {
        save_transformed_and_comment(employee_absences_in_weeks)
    }

    return(employee_absences_in_weeks)
}

#' Summarize Employee Absence Data by Year
#'
#' @description
#' Calculate yearly summaries of employee absences, breaking down absence percentages
#' by duration category (short, medium, and long-term).
#'
#' @param employees_absences_in_days A data frame containing daily employee absence records
#' with columns for employee ID (MEDEWERKER_ID), school year (SCHOOLJAAR_naam),
#' absence percentage (verzuim_percentage), and absence duration category (verzuim_duur).
#'
#' @returns A data frame summarizing yearly absence metrics per employee, including:
#'   * Total days in year
#'   * Absolute absence days and percentages for long-term absences
#'   * Absolute absence days and percentages for medium-term absences
#'   * Absolute absence days and percentages for short-term absences
#'
#' @importFrom dplyr group_by summarise
#'
#' @export
summarise_employee_absence_in_days_to_year <- function(employees_absences_in_days) {

    emplyee_absences_yearly <- employees_absences_in_days |>
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

    return(emplyee_absences_yearly)

}

#' Summarise Employee Absences by School Year
#'
#' @description
#' Transforms employee absence data into yearly summaries based on school years
#'
#' @param employee_absences A data frame containing employee absence records
#'
#' @returns
#' A data frame containing yearly summarized absence data for employees
#'
#' @importFrom purrr map map_dfr
#' @importFrom dplyr bind_rows
#'
#' @export
summarise_employee_absence_to_years <- function(employee_absences) {

    employee_absences_in_years_split <- split_absences_into_school_years(employee_absences)

    employees_absence_yearly <- employee_absences_in_years_split |>
        map(expand_to_daily) |>
        map(summarise_employee_absence_in_days_to_year) |>
        map_dfr(bind_rows)

    save_transformed_and_comment(employees_absence_yearly)

    return(employees_absence_yearly)
}

#' Pivot Weekly Data to Yearly Format
#'
#' @description
#' Transforms weekly employee absence data from long to wide format
#'
#' @param employee_absence_in_weeks A tibble containing employee absence data with columns:
#'   MEDEWERKER_ID, TEAM_kostenplaats_code, SCHOOLJAAR_naam,
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
                        # TEAM_kostenplaats_code,
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
        save_transformed_and_comment(employee_absences_in_weeks, filename = filename)
    } else {
        save_transformed_and_comment(employee_absences_in_weeks)
    }


    save_transformed_and_comment(employee_years_absence_weekly)

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

    # TODO Split in 4 dataframes and then join, otherwise there were issues

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

    save_transformed_and_comment(enrollments_bpv)

    return(enrollments_bpv)
}

#' Convert Answer Data to Employee-Level Format
#'
#' @description
#' Restructures employee satisfaction survey data from long to wide format
#'
#' @param employee_answers_satisfaction A dataframe containing employee satisfaction survey responses
#' in long format.
#'
#' @returns
#' A dataframe in wide format where each row represents an employee's responses,
#' with questions as columns and scores as values.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
#'
#' @export
pivot_answers_to_employees <- function(employee_answers_satisfaction) {

    employee_answers_satisfaction_with_helpers <- employee_answers_satisfaction |>
        add_helper_variables()

     employees_satisfaction <- employee_answers_satisfaction_with_helpers |>
        pivot_wider(
            id_cols = c(        SCHOOLJAAR_startjaar,
                                Organisatie,
                                `Characteristic 1`,
                                `Characteristic 2`, appearance_number),
            names_from = omschrijving,
            values_from = c(Score) # TODO Answer text out for now, `Answer Text`)
    ) |>
         clean_names()  |>
         rename_with(~ paste0("MWO_", .))

     save_transformed_and_comment(employees_satisfaction)

     return(employees_satisfaction)


 }

#' Convert Answer Data to Student-Level Format
#'
#' @description
#' Restructures student satisfaction survey data from long to wide format
#'
#' @param student_answers_satisfaction A dataframe containing student satisfaction survey responses
#' in long format.
#'
#' @returns
#' A dataframe in wide format where each row represents an student's responses,
#' with questions as columns and scores as values.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
#'
#' @export
pivot_answers_to_students <- function(student_answers_satisfaction) {

    students_satisfaction <- student_answers_satisfaction |>
        pivot_wider(
            id_cols = c(id,
                        startjaar,
                        opleiding,
                        niveau,
                        leerjaar),
            names_from = attribute,
            values_from = value
        ) |>
        clean_names() #|>
        #rename_with(~ paste0("JOB_Light_", .))

    filename <- paste0("students_satisfaction", "_", unique(students_satisfaction$startjaar))

    save_transformed_and_comment(students_satisfaction, filename = filename)

    return(students_satisfaction)
}


#' Summarize Employee Satisfaction by Groups
#'
#' @description
#' Groups employee satisfaction data by school year, organization, and characteristics
#'
#' @param employee_answers_satisfaction A data frame containing employee satisfaction data
#'
#' @returns
#' A data frame grouped by SCHOOLJAAR_startjaar, Organisatie, Characteristic 1,
#' Characteristic 2, and group_number, with count information removed.
#'
#' @importFrom dplyr count select
#'
#' @export
summarise_satisfaction_to_groups <- function(employee_answers_satisfaction) {

    employee_satisfaction_group <- employee_answers_satisfaction |>
    count(
        SCHOOLJAAR_startjaar,
        Organisatie,
        `Characteristic 1`,
        `Characteristic 2`,
        group_number
    ) |>
        select(-n)

    return(employee_satisfaction_group)
}



#' Pivot Categorical Values to Percentages
#'
#' @description
#' Transform categorical data into percentage columns, combining rare categories
#' into an 'overig' (other) category
#'
#' @param data A data frame containing the categorical column to transform
#' @param col_name A string specifying the column to pivot
#' @param grouping_vars Optional. A character vector of grouping variables.
#'   Defaults to c("TEAM_naam", "COHORT_naam")
#' @param min_pct Optional. A numeric value between 0 and 1 specifying the minimum
#'   percentage threshold for categories. Defaults to 0.10
#'
#' @returns
#' A data frame with rare categories (below min_pct) combined into 'overig' and
#' pivoted to wide format, where each category becomes a column with its percentage
#'
#' @importFrom dplyr select group_by summarise mutate filter across all_of
#' @importFrom tidyr pivot_wider
#' @importFrom rlang := .data
#'
#' @export
pivot_cat_values_to_pct <- function(data,
                                    col_name,
                                    grouping_vars = c("TEAM_naam", "COHORT_naam"),
                                    min_pct = 0.10) {

    data_pivoted <- data |>
        select(all_of(c(grouping_vars, col_name))) |>
        # Initial counts and percentages
        group_by(across(all_of(c(grouping_vars, col_name)))) |>
        summarise(n = n(), .groups = "drop") |>
        group_by(across(all_of(grouping_vars))) |>
        mutate(prop = n / sum(n)) |>
        # Set rare categories to overig
        group_by(across(all_of(grouping_vars))) |>
        mutate(
            {{col_name}} := if_else(prop >= min_pct,
                                    .data[[col_name]],
                                    "overig")
        ) |>
        group_by(across(all_of(c(grouping_vars, col_name)))) |>
        summarise(
            prop = sum(prop),
            .groups = "drop"
        ) |>
        # Remove the rare categories, including rare overig (so no longer 100%)
        filter(prop > min_pct) |>
        pivot_wider(
            id_cols = all_of(grouping_vars),
            names_from = all_of(col_name),
            values_from = prop,
            names_prefix = paste0(col_name, "_"),
            values_fill = 0
        )

    return(data_pivoted)

}


#' Transform Categorical Values to Percentage Columns
#'
#' @description
#' Convert categorical variables into percentage columns based on grouping variables
#'
#' @param data A data frame containing categorical variables.
#' @param grouping_vars Optional. A character vector of column names to group by. Defaults to `c("TEAM_naam", "COHORT_naam")`.
#' @param max_n_values Optional. Maximum number of unique values for a column to be considered categorical. Defaults to 6.
#' @param min_pct Optional. Minimum percentage threshold for including categories. Defaults to 0.10.
#'
#' @returns
#' A data frame with categorical variables transformed into percentage columns.
#' Will error if no variables meet the selection criteria.
#'
#' @importFrom dplyr select where n_distinct left_join all_of
#' @importFrom purrr map reduce
#' @importFrom cli cli_abort
#'
#' @export
transform_cat_val_to_pct_columns <- function(data,
                                         grouping_vars,
                                         max_n_values,
                                         min_pct) {


    # First get your base data with the right columns
    data_selected <- data |>
        select(all_of(grouping_vars),
               where(is.character) & where(~n_distinct(.) <= max_n_values)
        )

    # Get the columns we need to process
    cols_to_process <- data_selected |>
        select(!c(all_of(grouping_vars))) |>
        names()

    # Process all columns and join results
    data_pivoted <- cols_to_process |>
        map(~pivot_cat_values_to_pct(data_selected, ., grouping_vars, min_pct)) #

    tryCatch({
        data_reduced <- data_pivoted |>
            reduce(left_join, by = grouping_vars)
    }, error = function(e) {
        cli_abort("Error in processing categorical variables, no variables are selected. Try to increase the max_n_values")
    })

    return(data_reduced)

}

#' Transform Categorical Values to Percentage Columns for Enrollments
#'
#' @description
#' Convert categorical variables in enrollment data to percentage columns,
#' grouped by team and cohort
#'
#' @param enrollments A data frame containing enrollment data with TEAM_naam
#'   and COHORT_naam columns.
#'
#' @returns
#' A data frame with categorical variables converted to percentage columns.
#'
#' @export
transform_enrollments_cat_val_to_pct_columns <- function(enrollments) {

    teams_enrollments_cat_vars <- enrollments |>
        transform_cat_val_to_pct_columns(
            grouping_vars = c("TEAM_naam", "COHORT_naam"),
            max_n_values = 6,
            min_pct = 0.10)

    return(teams_enrollments_cat_vars)


}

#' Transform Employee Categories to Percentage Columns
#'
#' @description
#' Convert categorical employee data into percentage-based columns by team and school year
#'
#' @param employees A data frame containing employee data with columns for team cost center code and school year.
#'
#' @returns
#' A data frame with categorical variables transformed into percentage columns.
#' Only categories present in at least 20% of the data are included.
#'
#' @export
transform_employees_cat_val_to_pct_columns <- function(employees) {

    teams_employees_cat_vars <- employees |>
        transform_cat_val_to_pct_columns(
            grouping_vars = c("TEAM_kostenplaats_code", "SCHOOLJAAR_naam"),
            # The n values should be very high, since job types are very diverse.
            max_n_values = 100,
            # We increase the min percentage as well, to filter these out.
            min_pct = 0.20
    )

    return(teams_employees_cat_vars)

}

#' Summarize Employee Numeric Variables by Team
#'
#' @description
#' Calculate team-level summaries of employee numeric data like FTE and absence metrics
#'
#' @param employees A data frame containing employee data with columns for team cost center,
#'   school year, FTE values, and absence metrics
#'
#' @returns
#' A data frame grouped by team cost center code and school year, containing:
#'   - Summed FTE values
#'   - Mean FTE per employee
#'   - Mean absence metrics
#'
#' @importFrom dplyr group_by summarise across contains
#'
#' @export
summarise_employees_num_vars_to_teams <- function(employees) {

    teams_employees_num_vars <- employees |>
        group_by(TEAM_kostenplaats_code, SCHOOLJAAR_naam) |>
        summarise(
            MEDEWERKER_contract_fte = sum(MEDEWERKER_contract_fte, na.rm = TRUE),
            MEDEWERKER_contract_fte_aanpassing = sum(MEDEWERKER_contract_fte_aanpassing, na.rm = TRUE),
            MEDEWERKER_contract_fte_totaal = sum(MEDEWERKER_contract_fte_totaal, na.rm = TRUE),
            MEDEWERKER_contract_fte_gemiddelde = mean(MEDEWERKER_contract_fte_totaal, na.rm = TRUE),
            across(contains("verzuim"), ~mean(., na.rm = TRUE)),
            .groups = "drop"
        )

    return()
}


#' Filter Enrollments for Team Aggregation
#'
#' @description
#' Filter student enrollment data based on specific criteria for team-level analysis
#'
#' @param enrollments A tibble containing enrollment data with columns for cohort,
#'   enrollment dates, and enrollment characteristics.
#'
#' @returns
#' A filtered tibble containing only enrollments that meet specific criteria:
#' - Within configured year range
#' - Started within 2 months after or 10 months before cohort start
#' - Active on October 1st reference date
#' - Funded enrollments
#' - Full-time students
#' - Education levels 2, 3, or 4
#'
#' @importFrom dplyr filter select
#' @importFrom config get
#'
#' @export
filter_enrollments_for_team_aggregation <- function(enrollments) {

    enrollments_filtered <- enrollments |>
        filter(COHORT_startjaar >= config::get("first_year") - 1, # Later on we want to calculate growth per year
               COHORT_startjaar <= config::get("last_year"),
               #month(VERBINTENIS_begindatum) == 8,
               COHORT_start_datum + months(2) >= VERBINTENIS_begindatum,
               COHORT_start_datum - months(10) < VERBINTENIS_begindatum,
               VERBINTENIS_actief_op_1_okt_peildatum == TRUE,
               VERBINTENIS_bekostigd == "Ja",
               VERBINTENIS_intensiteit == "Voltijd",
               VERBINTENIS_niveau %in% c("2", "3", "4")) |>
        select(-VERBINTENIS_intensiteit)

    return(enrollments_filtered)
}

#' Filter Employees to Education Teams Only
#'
#' @description
#' Filter the employees dataset to only include those in education teams
#'
#' @param employees A dataframe of employees containing TEAM_kostenplaats_code
#'
#' @returns
#' A filtered dataframe containing only employees in education teams
#'
#' @importFrom dplyr filter
#'
#' @export
filter_employees_to_education_teams_only <- function(employees) {
    employees_filtered <- employees |>
        filter(TEAM_kostenplaats_code %in% enrollments_combined_enriched_filtered$TEAM_kostenplaats_code)

    return(employees_filtered)
}

#' Select columns for correlation analysis
#'
#' @description
#' Remove columns with too many missing values or no variation
#'
#' @param df A data frame.
#' @param na_thresh Optional. A numeric value between 0 and 1 indicating the threshold for NA values
#' (default: 0.8).
#'
#' @returns
#' A data frame with columns removed that either have more NAs than the threshold
#' or only one unique value.
#'
#' @importFrom dplyr select where n_distinct
#'
#' @export
select_cols_for_correlation <- function(df, na_thresh = 0.8) {

    df_selected <- df |>
        select(where(~ (!((mean(is.na(.x)) > na_thresh) |
                              (n_distinct(.x, na.rm = TRUE) == 1)
        ))))

    return(df_selected)
}

#' Calculate Correlations Between Team Variables
#'
#' @description
#' Calculate correlations between numeric variables in team data
#'
#' @param teams A data frame containing team-related numeric variables
#'
#' @returns
#' A correlation matrix of numeric variables, with NA values replaced by 0
#' and rearranged for better visualization.
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyr replace_na
#' @importFrom corrr correlate rearrange
#'
#' @export
correlate_teams <- function(teams) {

    teams_correlations <- teams |>
    select(where(is.numeric)) |>
        correlate() |>
        mutate(across(everything(), ~replace_na(., 0))) |>
        rearrange()

    return(teams_correlations)
}

#' Make Column Names User-Friendly
#'
#' @description
#' Removes technical prefixes and underscores from column names to make them more readable
#'
#' @param teams_correlations A data frame containing column names with 'VERBINTENIS_' and 'TEAM_' prefixes
#'
#' @returns
#' A data frame with cleaned column names, removing 'VERBINTENIS_' and 'TEAM_' prefixes
#' and replacing underscores with spaces
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove str_replace_all
#'
#' @export
set_user_friendly_names <- function(teams_correlations) {

    teams_correlations_friendly_names <- teams_correlations |>
        mutate(term = term |>
                   str_remove("VERBINTENIS_") |>
                   str_remove("TEAM_") |>
                   str_replace_all("_", " "))

    names(teams_correlations_friendly_names) <- names(teams_correlations_friendly_names) |>
        str_remove("VERBINTENIS_") |>
        str_remove("TEAM_") |>
        str_replace_all("_", " ")

    return(teams_correlations_friendly_names)
}

#' Summarize Enrollment Data by Teams and Cohorts
#'
#' @description
#' Creates a summary of enrollment metrics aggregated by team and cohort information
#'
#' @param enrollments A data frame containing enrollment data with team, cohort,
#'   and various numeric metrics columns
#'
#' @returns A data frame with team-level summaries including:
#'  * Count of students per team
#'  * Mean values for variables containing "dagen" (days)
#'  * Mean values for variables containing "waarneming" (observation)
#'  * Mean values for binary indicators (containing "_is_")
#'  * Mean BPV scope
#'  * Mean of definitive BPV before October 1st for BOL students
#'
#' @importFrom dplyr group_by summarise across contains n
#'
#' @export
summarise_enrollments_num_vars_to_teams <- function(enrollments) {

    teams_enrollments <- enrollments |>
        group_by(
            TEAM_naam,
            TEAM_kostenplaats_code,
            TEAM_naam_afk,
            TEAM_school,
            TEAM_school_afk,
            COHORT_naam,
            COHORT_startjaar
        ) |>
        summarise(
            # Do this before summarising the _is_ variables otherwise it becomes a mean of summarised variable
            BPV_is_definitief_voor_1_okt_BOL = mean(
                BPV_is_definitief_voor_1_okt[OPLEIDING_leerweg == "BOL"],
                na.rm = TRUE
            ),
            BPV_is_definitief_voor_1_okt_BBL = mean(
                BPV_is_definitief_voor_1_okt[OPLEIDING_leerweg == "BBL"],
                na.rm = TRUE
            ),
            TEAM_studenten_aantal = n(),
            across(contains("dagen"), ~mean(., na.rm = TRUE)),
            across(contains("waarneming"), ~mean(., na.rm = TRUE)),
            across(contains("_is_"), ~mean(., na.rm = TRUE)),
            DEELNEMER_passend_niveau = mean(DEELNEMER_passend_niveau, na.rm = TRUE),
            BPV_omvang = mean(BPV_omvang, na.rm = TRUE),
            .groups = "drop"
        )

    return(teams_enrollments)
}

summarise_employees_num_vars_to_teams <- function(employees) {

    teams_employees <- employees |>
        group_by(TEAM_kostenplaats_code, SCHOOLJAAR_naam) |>
        summarise(
            MEDEWERKER_contract_fte = sum(MEDEWERKER_contract_fte, na.rm = TRUE),
            MEDEWERKER_contract_fte_aanpassing = sum(MEDEWERKER_contract_fte_aanpassing, na.rm = TRUE),
            MEDEWERKER_contract_fte_totaal = sum(MEDEWERKER_contract_fte_totaal, na.rm = TRUE),
            across(contains("verzuim"), ~mean(., na.rm = TRUE)),
            .groups = "drop"
        )

    return(teams_employees)
}
