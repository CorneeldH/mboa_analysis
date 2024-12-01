#' Ingest Cohort Data
#'
#' @description
#' Reads and processes cohort data from a CSV file, standardizing column names
#' and removing duplicates. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed cohort data with columns:
#'   \itemize{
#'     \item COHORT_ID: Unique cohort identifier
#'     \item COHORT_naam: Cohort name
#'   }
#'
#' @importFrom dplyr distinct select rename
#'
#' @export
ingest_cohorts <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("cohorts")
    }

    data <- safe_read_csv(filename, ...) |>
        select(ID, NAAM) |>
        rename(COHORT_ID = ID, COHORT_naam = NAAM) |>
        distinct()
}

#' Ingest Education Team Information
#'
#' @description
#' Reads and processes education team hierarchical data from a CSV file.
#' Handles cluster, school, and team information along with cost center codes.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#' @return A tibble containing processed team data with prefixed column names:
#'   \itemize{
#'     \item TEAM_cluster: Full cluster name
#'     \item TEAM_cluster_afk: Cluster abbreviation
#'     \item TEAM_school: Full school name
#'     \item TEAM_school_afk: School abbreviation
#'     \item TEAM_naam: Full team name
#'     \item TEAM_naam_afk: Team abbreviation
#'     \item TEAM_kostenplaats: Cost center code
#'     \item TEAM_sk_kostenplaats: SK cost center code
#'     \item TEAM_sk_kostenplaats_hr2day: HR2Day cost center code
#'   }
#'
#' @importFrom dplyr select rename rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_teams <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("teams")
    }

    data <- safe_read_csv(filename, ...) |>
        select(
            ORG1ID,
            Cluster,
            ClusterAfk,
            School,
            SchoolAfk,
            Team,
            TeamAfk,
            Kostenplaats,
            SK_Kostenplaats,
            SK_KostenplaatsHR2Day
        ) |>
        clean_names() |>
        rename(ID = org1id,
               naam = team,
               naam_afk = team_afk) |>
        rename_with(~ paste0("TEAM_", .))
}

#' Ingest Flex Status Information
#'
#' @description
#' Reads and processes flex status data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed flex status data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Unique connection identifier
#'     \item VERBINTENIS_Is_flex: Boolean indicating flex status
#'   }
#'
#' @importFrom dplyr select rename mutate
#'
#' @export
ingest_enrollment_flex_status <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("flex")
    }

    data <- safe_read_csv(filename, ...) |>
        select(`ID Verbintenis`, IsFlex) |>
        rename(VERBINTENIS_ID = `ID Verbintenis`,
               VERBINTENIS_is_flex_omschrijving = IsFlex)
}

#' Ingest Education Programme Information
#'
#' @description
#' Reads and processes education programme data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed programme data with columns prefixed with OPLEIDING_
#'
#' @importFrom dplyr select rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_programmes <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("programmes")
    }

    data <- safe_read_csv(filename, ...) |>
        select(
            ID,
            CODE,
            NAAM,
            WERVINGSNAAM,
            LEERWEG,
            DEFAULTINTENSITEIT,
            COMMUNICERENMETBRON,
            NEGEERLANDELIJKECRITERIA,
            NEGEERLANDELIJKEPRODUCTREGELS
        ) |>
        clean_names() |>
        rename_with(~ paste0("OPLEIDING_", .))
}

#' Ingest Enrollment Level Information
#'
#' @description
#' Reads and processes enrollment level data from a CSV file without headers.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing enrollment level data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Unique enrollment identifier
#'     \item VERBINTENIS_niveau: Enrollment level
#'   }
#'
#' @export
ingest_enrollment_levels <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("enrollment_levels")
    }

    # Add column names since they miss in raw data
    data <- safe_read_csv(filename, col_names = c("VERBINTENIS_ID", "VERBINTENIS_niveau"))
}


#' Ingest Enrollment Information
#'
#' @description
#' Reads and processes enrollment data from a CSV file, standardizing column names
#' by converting prefixes and applying consistent casing.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing enrollment data with standardized column names:
#'   - Prefix 'verbintenis_' becomes 'VERBINTENIS_'
#'   - Prefix 'id_' becomes capitalized suffixed '_ID'
#'
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove
#' @importFrom dplyr rename_with matches
#'
#' @export
ingest_enrollments <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("enrollments")
    }

    data <- safe_read_csv(filename, ...) |>
        clean_names() |>
        rename_with(
            ~{rest_part <- str_remove(., "^verbintenis_")
            paste0("VERBINTENIS_", rest_part)
            },
            matches("^verbintenis_")
        ) |>
        rename_with(
            ~{name_part <- str_remove(., "^id_")
              paste0(toupper(name_part), "_ID")
            },
            matches("^id_")
        )

}

#' Ingest Application Information
#'
#' @description
#' Reads and processes application data from a CSV file, selecting relevant columns
#' and standardizing column names. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed application data with columns:
#'   \itemize{
#'     \item AANMELDING_begin_datum: Start date
#'     \item AANMELDING_laatst_gewijzigd_datum: Last modified date
#'     \item VERBINTENIS_ID: Enrollment reference
#'     \item AANMELDING_is_eerste_jaar: First year application indicator
#'   }
#'
#' @importFrom dplyr select rename rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_applications <- function(..., filename = NULL) {

    if (is.null(filename)) {
        filename <- get_filename_from_config("applications")
    }

    data_raw <- safe_read_csv(filename, ...)

    data_clean <- data_raw |>
        select(AANMELDING_begin_datum = BEGINDATUM,
               AANMELDING_laatst_gewijzigd_datum = LAST_MODIFIED_AT,
               # Like TYPE and INGETROKKEN only available from 2023
               AANMELDING_is_eerste_jaar = AANMELDINGVOOREERSTELEERJAAR,
               VERBINTENIS_ID = VERBINTENIS)



}
