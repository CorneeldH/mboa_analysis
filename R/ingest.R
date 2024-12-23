#' Ingest Cohort Data
#'
#' @description
#' Reads and processes cohort data from a CSV file, standardizing column names
#' and removing duplicates. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "cohorts")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
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
ingest_cohorts <- function(..., filename = NULL, path = NULL, config_key = "cohorts", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(ID, NAAM) |>
        rename(COHORT_ID = ID, COHORT_naam = NAAM) |>
        distinct()

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Education Team Information
#'
#' @description
#' Reads and processes education team hierarchical data from a CSV file.
#' Handles cluster, school, and team information along with cost center codes.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "teams")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed team data with prefixed column names:
#'   \itemize{
#'     \item TEAM_cluster: Full cluster name
#'     \item TEAM_cluster_afk: Cluster abbreviation
#'     \item TEAM_school: Full school name
#'     \item TEAM_school_afk: School abbreviation
#'     \item TEAM_naam: Full team name
#'     \item TEAM_naam_afk: Team abbreviation
#'     \item TEAM_kostenplaats_code: Cost center code
#'     \item TEAM_sk_kostenplaats: SK cost center code
#'     \item TEAM_sk_kostenplaats_hr2day: HR2Day cost center code
#'   }
#'
#' @importFrom dplyr select rename rename_with distinct
#' @importFrom janitor clean_names
#'
#' @export
ingest_teams <- function(..., filename = NULL, path = NULL, config_key = "teams", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
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
        rename_with(~ paste0("TEAM_", .)) |>
        rename(TEAM_kostenplaats_code = TEAM_kostenplaats) |>
        distinct()

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Flex Status Information
#'
#' @description
#' Reads and processes flex status data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "enrollments_flex")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed flex status data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Unique connection identifier
#'     \item VERBINTENIS_flex_omschrijving: Text description of flex status
#'   }
#'
#' @importFrom dplyr select rename mutate
#'
#' @export
ingest_enrollments_flex <- function(..., filename = NULL, path = NULL, config_key = "enrollments_flex", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(`ID Verbintenis`, IsFlex) |>
        rename(VERBINTENIS_ID = `ID Verbintenis`,
               VERBINTENIS_flex_omschrijving = IsFlex)

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Education Programme Information
#'
#' @description
#' Reads and processes education programme data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "programmes_basics")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed programme data with columns prefixed with OPLEIDING_:
#'   \itemize{
#'     \item OPLEIDING_ID: Programme identifier
#'     \item OPLEIDING_code: Programme code
#'     \item OPLEIDING_naam: Programme name
#'     \item OPLEIDING_wervingsnaam: Recruitment name
#'     \item OPLEIDING_leerweg: Learning path
#'     \item OPLEIDING_defaultintensiteit: Default intensity
#'     \item OPLEIDING_communicerenmetbron: Communication with source
#'     \item OPLEIDING_negeerlandelijkecriteria: National criteria negation
#'     \item OPLEIDING_negeerlandelijkeproductregels: National product rules negation
#'   }
#'
#' @importFrom dplyr select rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_programmes_basics <- function(..., filename = NULL, path = NULL, config_key = "programmes_basics", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
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
        rename(ID = id) |>
        rename_with(~ paste0("OPLEIDING_", .))

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Enrollment Level Information
#'
#' @description
#' Reads and processes enrollment level data from a CSV file without headers.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "enrollments_level")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing enrollment level data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Unique enrollment identifier
#'     \item VERBINTENIS_niveau_omschrijving: Enrollment level description
#'   }
#'
#' @export
ingest_enrollments_level <- function(..., filename = NULL, path = NULL, config_key = "enrollments_level", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            VERBINTENIS_ID = `ID Verbintenis`,
            VERBINTENIS_niveau_omschrijving = Niveau
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Enrollment Information
#'
#' @description
#' Reads and processes enrollment data from a CSV file, standardizing column names
#' by converting prefixes and applying consistent casing.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "enrollments_basics")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing enrollment data with standardized column names:
#'   \itemize{
#'     \item All columns with prefix 'verbintenis_' become 'VERBINTENIS_'
#'     \item All columns with prefix 'id_' become capitalized with suffix '_ID'
#'     \item TEAM_ID: Replaces ORGANISATIE_EENHEID_ID
#'   }
#'
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove
#' @importFrom dplyr rename_with matches
#'
#' @export
ingest_enrollments_basics <- function(..., filename = NULL, path = NULL, config_key = "enrollments_basics", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        clean_names() |>
        rename_with(
            ~{rest_part <- str_remove(., "^verbintenis_")
            paste0("VERBINTENIS_", rest_part)
            },
            matches("^verbintenis_")
        ) |>
        rename_with(
            ~ str_remove(., "^id_") |>
                toupper() |>
                paste0("_ID"),
            matches("^id_")
        ) |>
        rename(TEAM_ID = ORGANISATIE_EENHEID_ID)

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Application Information
#'
#' @description
#' Reads and processes application data from a CSV file, selecting relevant columns
#' and standardizing column names. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "enrollments_application")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
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
ingest_enrollments_application <- function(..., filename = NULL, path = NULL, config_key = "enrollments_application", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(AANMELDING_begin_datum = BEGINDATUM,
               AANMELDING_laatst_gewijzigd_datum = LAST_MODIFIED_AT,
               # Like TYPE and INGETROKKEN only available from 2023
               AANMELDING_is_eerste_jaar = AANMELDINGVOOREERSTELEERJAAR,
               VERBINTENIS_ID = VERBINTENIS)

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest Enrollment Absence Information
#'
#' @description
#' Reads and processes enrollment absence data from a CSV file, selecting relevant columns
#' for tracking student attendance and absences.
#'
#' @param ... Additional arguments passed to readr::read_delim
#' @param filename Optional character string specifying the name of the file to read.
#'                If NULL, filename is retrieved from config using config_key.
#' @param path Optional character string specifying the path to read from.
#'            If NULL, path is retrieved from config using config_data_path.
#' @param config_key Character string specifying the configuration key to use.
#'                   Defaults to "attendance_observations".
#' @param config_data_path Character string specifying the config key for raw data directory.
#'                         Defaults to "data_raw_dir".
#'
#' @return A tibble containing processed absence data with columns:
#'   \itemize{
#'     \item SK_GroepInschrijving: Group enrollment identifier
#'     \item SK_Leereenheid: Learning unit identifier
#'     \item Datum: Date of the absence record
#'     \item SK_afspraak: Appointment identifier
#'     \item Waarnemingsduur: Duration of observation
#'     \item Presentietekst: Attendance status text
#'   }
#'
#' @details
#' The function loads raw data using the load_data function and selects specific columns
#' for absence tracking. The original config_key is preserved as a comment in the
#' returned data for traceability.
#'
#' @importFrom dplyr select
#'
#' @export
ingest_attendance_observations <- function(..., filename = NULL, path = NULL, config_key = "attendance_observations", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(SK_GroepInschrijving,
               SK_Leereenheid,
               Datum,
               SK_afspraak,
               Waarnemingsduur,
               Presentietekst
               )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}


ingest_enrollment_to_sk_mapping <- function(..., filename = NULL, path = NULL, config_key = "enrollment_to_sk_mapping", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(SK_inschrijving = SK_Inschrijving,
               VERBINTENIS_ID = ID_Verbintenis
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # add check if there is config enrollments_application
    # audit(data_clean, data_raw)
    return(data_clean)

}

# No longer needed due to group_placements
# #' Ingest Group Participation Data
# #'
# #' @description
# #' Reads and processes group participation data from a CSV file, standardizing
# #' identifiers and column names. Expects CSV files with semicolon (;) as separator.
# #'
# #' @param filename Character string specifying the name of the CSV file to read
# #' @param path Character string specifying the path to the CSV file
# #' @param config_key Character string specifying the configuration key to use (default: "group_participation")
# #' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
# #' @param ... Additional arguments passed to readr::read_delim
# #'
# #' @return A tibble containing processed group participation data with columns:
# #'   \itemize{
# #'     \item GROEP_ID: Group identifier
# #'     \item DEELNEMER_ID: Participant identifier
# #'     \item TEAM_ID: Team identifier
# #'     \item GROEP_code: Group code
# #'     \item GROEP_naam: Group name
# #'     \item GROEP_type: Group type
# #'   }
# #'
# #' @importFrom dplyr contains select rename_with matches
# #' @importFrom stringr str_remove
# #' @importFrom janitor clean_names
# #'
# #' @export
# ingest_group_participation <- function(..., filename = NULL, path = NULL, config_key = "group_participation", config_data_path = "data_raw_dir") {
#
#     # Name arguments since order behind ... is not guaranteed
#     # Suppress messages to ignore message for columns with same name, this is solved by read_delim
#     data_raw <- suppressMessages(load_data(config_key,
#                           ...,
#                          filename = filename,
#                           path = path,
#                           config_data_path = config_data_path))
#     data_clean <- data_raw |>
#         select(
#             ID_Groep,
#             ID_Deelnemer,
#             ID_Team = ID_Organisatieeenheid,
#             CODE,
#             naam = NAAM...6,
#             type = NAAM...11
#         ) |>
#         clean_names() |>
#         rename_with(
#             ~ str_remove(., "^id_") |>
#                 toupper() |>
#                 paste0("_ID"),
#             matches("^id_")
#         ) |>
#         rename_with(
#             ~paste0("GROEP_", .),
#             !contains("ID")
#         )
#
#     # keep the config with the data for later use
#     comment(data_clean) <- config_key     save_ingested(data_clean)     save_ingested(data_clean)
#
#     # audit(data_clean, data_raw)
#     return(data_clean)
# }


#' Ingest Group Placement Data
#'
#' @description
#' Reads and processes group placement data from a CSV file, managing column names
#' and prefixes
#'
#' @param filename Optional. Character string specifying the name of the CSV file to read.
#' @param path Optional. Character string specifying the path to the CSV file.
#' @param config_key Character string specifying the configuration key to use. Optional, defaults to "group_placements".
#' @param config_data_path Character string specifying the config path for raw data. Optional, defaults to "data_raw_dir".
#' @param ... Additional arguments passed to readr::read_delim.
#'
#' @returns
#' A tibble containing processed group placement data with column names prefixed
#' with "GROEP_" (except SK columns which maintain their "SK_" prefix).
#'
#' @importFrom janitor clean_names
#' @importFrom dplyr rename_with matches
#' @importFrom stringr str_replace
#'
#' @export
ingest_group_placements <- function(..., filename = NULL, path = NULL, config_key = "group_placements", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        clean_names() |>
        rename_with(~paste0("GROEP_", .),
                    !matches("SK")) |>
        rename_with(
            ~ str_replace(., "^sk_", "SK_"))

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)


    # audit(data_clean, data_raw)
    return(data_clean)

}



#' Ingest Reasons for Leaving Data
#'
#' @description
#' Reads and processes student withdrawal reason data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "reasons_for_leaving")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed withdrawal reason data with columns:
#'   \itemize{
#'     \item REDEN_UITSCHRIJVING_ID: Unique identifier for the withdrawal reason
#'     \item VERBINTENIS_reden_uitschrijving: Description of the withdrawal reason
#'   }
#'
#' @importFrom dplyr select
#'
#' @export
ingest_reasons_for_leaving <- function(..., filename = NULL, path = NULL, config_key = "reasons_for_leaving", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            REDEN_UITSCHRIJVING_ID = `ID Reden Uitschrijving`,
            REDEN_actief = ACTIEF, # gaat over gebruik van deze reden / code
            # OVERLIJDEN, # REDENUITVAL ook
            VERBINTENIS_reden_uitschrijving = `Reden Uitschrijving`
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest Employee Absence Data
#'
#' @description
#' Reads and processes employee absence data from a CSV file, including sick leave
#' and absence periods. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employee_absences")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed employee absence data with columns:
#'   \itemize{
#'     \item TEAM_kostenplaats_code: Department cost center code
#'     \item MEDEWERKER_ID: Employee identifier
#'     \item FUNCTIE_ID: Function identifier
#'     \item MEDEWERKER_eerste_verzuimdag: First day of absence
#'     \item MEDEWERKER_laatste_verzuimdag: Last day of absence
#'     \item MEDEWERKER_percentage_verzuim: Sick leave percentage
#'   }
#'
#' @importFrom dplyr select rename_with contains
#' @importFrom stringr str_remove
#' @importFrom janitor clean_names
#'
#' @export
ingest_employee_absences <- function(..., filename = NULL, path = NULL, config_key = "employee_absences", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            `ID Medewerker`,
            # contract_kostenplaats_code = `Kostenplaats Afdeling`,
            `Eerste verzuimdag`,
            `Laatste verzuimdag`,
            #`Soort Verzuim`, privacy-gevoelig
            percentage_verzuim_omschrijving = HR2D__SICKPERC__C
        ) |>
        clean_names() |>
        rename_with(
            ~ str_remove(., "^id_") |>
                toupper() |>
                paste0("_ID"),
            matches("^id_")
        ) |>
        rename_with(~ paste0("MEDEWERKER_", .),
                    !contains("ID"))

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Study Advice Data
#'
#' @description
#' Reads and processes study advice data from a CSV file, including provisional
#' and final study recommendations. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "study_advices")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed study advice data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Enrollment identifier
#'     \item VERBINTENIS_advies_aanmaakdatum: Creation date of advice
#'     \item VERBINTENIS_advies_voorlopig: Provisional advice
#'     \item VERBINTENIS_advies_definitief: Final advice
#'   }
#'
#' @importFrom dplyr contains select rename rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_study_advices <- function(..., filename = NULL, path = NULL, config_key = "study_advices", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            #`ID Deelnemer`,
            `ID Verbintenis`,
            Aanmaakdatum,
            Voorlopig,
            Definitief
        ) |>
        clean_names() |>
        rename(VERBINTENIS_ID = id_verbintenis) |>
        rename_with(
            .fn = ~paste0("VERBINTENIS_advies_", .),
            .cols = !contains("ID")
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Programme BC Codes Data
#'
#' @description
#' Reads and processes programme BC (Beroepsopleiding) codes data from a CSV file,
#' including level information and labels. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "programme_bc_codes")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed programme BC codes data with columns:
#'   \itemize{
#'     \item OPLEIDING_code: Programme code
#'     \item OPLEIDING_bc_code: Professional education code
#'     \item OPLEIDING_bc_label: Professional education label
#'     \item OPLEIDING_niveau: Education level
#'     \item OPLEIDING_niveau_beroep: Professional level
#'   }
#'
#' @importFrom dplyr select rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_programme_bc_codes <- function(..., filename = NULL, path = NULL, config_key = "programme_bc_codes", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            Code,
            bc_code = BeroepOpleidingCode,
            bc_label = BeroepopleidingLabel
        ) |>
        clean_names() |>
        rename_with(~ paste0("OPLEIDING_", .))


    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}


#' Ingest Student Demographics Data
#'
#' @description
#' Reads and processes student demographic data from a CSV file, standardizing column
#' names with the DEELNEMER_ prefix. Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "students_demographics")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed student demographic data with columns:
#'   \itemize{
#'     \item DEELNEMER_ID: Student identifier (renamed from id_deelnemer)
#'     \item Additional demographic columns prefixed with DEELNEMER_
#'   }
#'
#' @details
#' The function processes the raw data by:
#' 1. Loading the data using load_data
#' 2. Cleaning column names using janitor::clean_names
#' 3. Renaming ID column from id_deelnemer
#' 4. Prefixing all columns with DEELNEMER_
#' The original config_key is preserved as a comment in the returned data.
#'
#' @importFrom janitor clean_names
#' @importFrom dplyr rename rename_with
#'
#' @export
ingest_students_demographics <- function(..., filename = NULL, path = NULL, config_key = "students_demographics", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        clean_names() |>
        rename(ID = id_deelnemer) |>
        rename_with(~ paste0("DEELNEMER_", .))

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}


#' Ingest Special Educational Needs Data
#'
#' @description
#' Reads and processes data about students with special educational needs from a CSV file
#'
#' @param filename Optional. A string specifying the name of the CSV file to read.
#' @param path Optional. A string specifying the path to the CSV file.
#' @param config_key Optional. A string specifying the configuration key (defaults to "enrollments_special_needs").
#' @param config_data_path Optional. A string specifying the config path for raw data (defaults to "data_raw_dir").
#' @param ... Additional arguments passed to the data loading function.
#'
#' @returns
#' A tibble containing:
#' \itemize{
#'   \item VERBINTENIS_ID: Enrollment identifier
#'   \item VERBINTENIS_passend_onderwijs_kenmerk: Special educational needs characteristic
#' }
#'
#' @importFrom dplyr select
#'
#' @export
ingest_enrollments_special_needs <- function(..., filename = NULL, path = NULL, config_key = "enrollments_special_needs", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            VERBINTENIS_ID = IDVerbintenis,
            VERBINTENIS_passend_onderwijs_kenmerk = Kenmerk_Passend
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

# TODO Excluded for now due to missing data
# #' Ingest Individual Education Plan (IBP) Data
# #'
# #' @description
# #' Reads and processes Individual Education Plan (IBP) data from a CSV file,
# #' focusing on enrollment IDs and IBP creation dates. Expects CSV files with
# #' semicolon (;) as separator.
# #'
# #' @param filename Character string specifying the name of the CSV file to read
# #' @param path Character string specifying the path to the CSV file
# #' @param config_key Character string specifying the configuration key to use (default: "enrollments_ibp")
# #' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
# #' @param ... Additional arguments passed to readr::read_delim
# #'
# #' @return A tibble containing processed IBP data with columns:
# #'   \itemize{
# #'     \item VERBINTENIS_ID: Enrollment identifier
# #'     \item VERBINTENIS_ibp_datum: IBP creation date
# #'   }
# #'
# #' @details
# #' The function processes the raw data by selecting essential columns and
# #' standardizing column names. Student IDs are intentionally excluded as
# #' enrollment IDs provide sufficient identification for most records.
# #' The original config_key is preserved as a comment in the returned data.
# #'
# #' @importFrom dplyr select
# #'
# #' @export
# ingest_enrollments_ibp <- function(..., filename = NULL, path = NULL, config_key = "enrollments_ibp", config_data_path = "data_raw_dir") {
#
#     # Name arguments since order behind ... is not guaranteed
#     data_raw <- load_data(config_key,
#                           ...,
#                           filename = filename,
#                           path = path,
#                           config_data_path = config_data_path)
#
#     data_clean <- data_raw |>
#         # Only a few ID's of verbintenis are missing, so we drop deelnemer
#         select(
#             VERBINTENIS_ID = ID_Verbintenis,
#             VERBINTENIS_ibp_datum = IBPAanmaakdatum
#         )
#
#     # keep the config with the data for later use
#     comment(data_clean) <- config_key     save_ingested(data_clean)
#
#     # audit(data_clean, data_raw)
#     return(data_clean)
# }

#' Ingest BPV (Internship) Registration Data
#'
#' @description
#' Reads and processes internship registration data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "bpv_registrations")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed internship registration data with columns:
#'   \itemize{
#'     \item BPV_ID: Unique internship identifier
#'     \item VERBINTENIS_ID: Enrollment identifier
#'     \item BPV_omvang: Total scope/size of internship
#'     \item BPV_verwachte_eind_datum: Expected end date
#'   }
#'
#' @importFrom dplyr select
#'
#' @export
ingest_bpv_registrations <- function(..., filename = NULL, path = NULL, config_key = "bpv_registrations", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    # Suppress warnings to ignore warning for unused variable
    data_raw <- suppressWarnings(load_data(config_key,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path))

    data_clean <- data_raw |>
        select(
            BPV_ID = ID,
            VERBINTENIS_ID = VERBINTENIS,
            BPV_omvang = TOTALEOMVANG,
            BPV_verwachte_eind_datum = VERWACHTEEINDDATUM
            # volgnummer?
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest BPV (Internship) Status Data
#'
#' @description
#' Reads and processes internship status data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "bpv_statusses")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed internship status data with columns:
#'   \itemize{
#'     \item BPV_ID: Internship identifier
#'     \item BPV_status_begin_datum: Status start date
#'     \item BPV_status_eind_datum: Status end date
#'     \item BPV_status: Status description
#'     \item TEAM_ID: Team identifier
#'   }
#'
#' @importFrom dplyr select
#'
#' @export
ingest_bpv_statusses <- function(..., filename = NULL, path = NULL, config_key = "bpv_statusses", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            BPV_ID = ID_BPVInschrijving,
            BPV_status_begin_datum = CREATED_AT,
            BPV_status_eind_datum = LAST_MODIFIED_AT,
            BPV_status = NAARSTATUS
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Process Basic Job Contract Data
#'
#' @description
#' Internal helper function that processes basic job contract data from CSV files.
#' This function handles the core data transformation logic shared by year-specific
#' ingest functions.
#'
#' @param config_key Character string specifying the configuration key to use
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed basic job contract data with columns:
#'   \itemize{
#'     \item MEDEWERKER_ID: Employee identifier
#'     \item TEAM_kostenplaats_code: Department cost center code
#'     \item MEDEWERKER_contract_fte: Full Time Equivalent value
#'     \item MEDEWERKER_contract_fte_peildatum: Reference date derived from config filename
#'   }
#'
#' @importFrom dplyr select mutate
#' @importFrom lubridate ymd
#'
#' @keywords internal
ingest_employees_contract_basics_helper <- function(config_key,..., filename = NULL, path = NULL, config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            MEDEWERKER_ID = `ID Medewerker`,
            TEAM_kostenplaats_code = `Kostenplaats Afdeling`,
            MEDEWERKER_contract_fte = FTE
        ) |>
        mutate(
            MEDEWERKER_contract_fte_peildatum = ymd(get_filename_from_config(config_key))
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest Basic Job Contract Data for 2023
#'
#' @description
#' Reads and processes basic job contract data specific to the year 2023.
#' Uses the helper function ingest_employees_contract_basics_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_basics_2023")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed basic job contract data for 2023
#'
#' @seealso ingest_employees_contract_basics_helper
#'
#' @export
ingest_employees_contract_basics_2023 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_basics_2023", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_contract_basics_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Basic Job Contract Data for 2022
#'
#' @description
#' Reads and processes basic job contract data specific to the year 2022.
#' Uses the helper function ingest_employees_contract_basics_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_basics_2022")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed basic job contract data for 2022
#'
#' @seealso ingest_employees_contract_basics_helper
#'
#' @export
ingest_employees_contract_basics_2022 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_basics_2022", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_contract_basics_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Basic Job Contract Data for 2021
#'
#' @description
#' Reads and processes basic job contract data specific to the year 2021.
#' Uses the helper function ingest_employees_contract_basics_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_basics_2020")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed basic job contract data for 2021
#'
#' @seealso ingest_employees_contract_basics_helper
#'
#' @export
ingest_employees_contract_basics_2021 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_basics_2021", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_contract_basics_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Basic Job Contract Data for 2020
#'
#' @description
#' Reads and processes basic job contract data specific to the year 2020.
#' Uses the helper function ingest_employees_contract_basics_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_basics_2020")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed basic job contract data for 2020
#'
#' @seealso ingest_employees_contract_basics_helper
#'
#' @export
ingest_employees_contract_basics_2020 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_basics_2020", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_contract_basics_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Basic Job Contract Data for 2019
#'
#' @description
#' Reads and processes basic job contract data specific to the year 2019.
#' Uses the helper function ingest_employees_contract_basics_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_basics_2019")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed basic job contract data for 2019
#'
#' @seealso ingest_employees_contract_basics_helper
#'
#' @export
ingest_employees_contract_basics_2019 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_basics_2019", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_contract_basics_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Helper Function for Processing Extra FTE Job Contract Data
#'
#' @description
#' Internal helper function that processes extra FTE job contract data from CSV files.
#' This function handles the core data transformation logic shared by year-specific
#' ingest functions.
#'
#' @param config_key Character string specifying the configuration key to use
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed job contract FTE data with columns:
#'   \itemize{
#'     \item MEDEWERKER_ID: Employee identifier
#'     \item TEAM_kostenplaats_code: Department cost center code
#'     \item MEDEWERKER_contract_fte_aanpassing: FTE adjustment value
#'     \item MEDEWERKER_contract_fte_peildatum: Date derived from config filename
#'   }
#'
#' @details
#' The function reads the raw data using load_data and performs the following transformations:
#' - Selects and renames relevant columns
#' - Adds a date column based on the filename in the configuration
#' - Preserves the config_key as a comment in the returned data
#'
#' @importFrom dplyr select mutate
#' @importFrom lubridate ymd
#'
#' @keywords internal
ingest_job_components_extra_fte_helper <- function(config_key, ..., filename = NULL, path = NULL, config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            MEDEWERKER_ID = `ID Medewerker`,
            TEAM_kostenplaats_code = `Kostenplaats Afdeling`,
            MEDEWERKER_contract_fte_aanpassing = `FTE Looncomponent`
        ) |>
        mutate(
            MEDEWERKER_contract_fte_peildatum = ymd(get_filename_from_config(config_key))
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Extra FTE Job Contract Data for 2023
#'
#' @description
#' Reads and processes extra FTE job contract data specific to the year 2023.
#' Uses the helper function ingest_job_components_extra_fte_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_extra_fte_2023")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed job contract FTE data for 2023
#'
#' @seealso ingest_job_components_extra_fte_helper
#'
#' @export
ingest_job_components_extra_fte_2023 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_extra_fte_2023", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_job_components_extra_fte_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Extra FTE Job Contract Data for 2022
#'
#' @description
#' Reads and processes extra FTE job contract data specific to the year 2022.
#' Uses the helper function ingest_job_components_extra_fte_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_extra_fte_2022")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed job contract FTE data for 2022
#'
#' @seealso ingest_job_components_extra_fte_helper
#'
#' @export
ingest_job_components_extra_fte_2022 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_extra_fte_2022", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_job_components_extra_fte_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Extra FTE Job Contract Data for 2021
#'
#' @description
#' Reads and processes extra FTE job contract data specific to the year 2021.
#' Uses the helper function ingest_job_components_extra_fte_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_extra_fte_2021")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed job contract FTE data for 2021
#'
#' @seealso ingest_job_components_extra_fte_helper
#'
#' @export
ingest_job_components_extra_fte_2021 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_extra_fte_2021", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_job_components_extra_fte_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Extra FTE Job Contract Data for 2020
#'
#' @description
#' Reads and processes extra FTE job contract data specific to the year 2020.
#' Uses the helper function ingest_job_components_extra_fte_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_extra_fte_2020")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed job contract FTE data for 2020
#'
#' @seealso ingest_job_components_extra_fte_helper
#'
#' @export
ingest_job_components_extra_fte_2020 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_extra_fte_2020", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_job_components_extra_fte_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Extra FTE Job Contract Data for 2019
#'
#' @description
#' Reads and processes extra FTE job contract data specific to the year 2019.
#' Uses the helper function ingest_job_components_extra_fte_helper for processing.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "employees_contract_extra_fte_2019")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed job contract FTE data for 2019
#'
#' @seealso ingest_job_components_extra_fte_helper
#'
#' @export
ingest_job_components_extra_fte_2019 <- function(..., filename = NULL, path = NULL, config_key = "employees_contract_extra_fte_2019", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_job_components_extra_fte_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

ingest_employees_job_type_helper <- function(config_key, ..., filename = NULL, path = NULL, config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(MEDEWERKER_ID = `ID Medewerker`,
               TEAM_kostenplaats_code = `Kostenplaats Afdeling`,
               MEDEWERKER_functie = Functie) |>
        mutate(
            MEDEWERKER_functie_peildatum = ymd(get_filename_from_config(config_key))
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

ingest_employees_job_type_2019 <- function(..., filename = NULL, path = NULL, config_key = "employees_job_type_2019", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_job_type_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

ingest_employees_job_type_2020 <- function(..., filename = NULL, path = NULL, config_key = "employees_job_type_2020", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_job_type_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

ingest_employees_job_type_2021 <- function(..., filename = NULL, path = NULL, config_key = "employees_job_type_2021", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_job_type_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

ingest_employees_job_type_2022 <- function(..., filename = NULL, path = NULL, config_key = "employees_job_type_2022", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_job_type_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

ingest_employees_job_type_2023 <- function(..., filename = NULL, path = NULL, config_key = "employees_job_type_2023", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_job_type_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

ingest_employees_job_type_2024 <- function(..., filename = NULL, path = NULL, config_key = "employees_job_type_2024", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employees_job_type_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}


#' Ingest Location Data
#'
#' @description
#' Reads and processes location data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "locations")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed location data with columns:
#'   \itemize{
#'     \item LOCATIE_ID: Location identifier
#'     \item LOCATIE_naam: Location name
#'   }
#'
#' @importFrom dplyr select
#'
#' @export
ingest_locations <- function(..., filename = NULL, path = NULL, config_key = "locations", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(LOCATIE_ID = ID,
               LOCATIE_naam = NAAM)

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Exam Plan Data
#'
#' @description
#' Reads and processes exam plan data from a CSV file.
#' Expects CSV files with semicolon (;) as separator.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "exam_plans")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed exam plan data with columns:
#'   \itemize{
#'     \item EXAMENPLAN_ID: Exam plan identifier
#'     \item OPLEIDING_ID: Programme identifier
#'     \item COHORT_ID: Cohort identifier
#'     \item EXAMENPLAN_begindatum_omschrijving: Creation date
#'     \item EXAMENPLAN_einddatum_omschrijving: Last modified date
#'     \item EXAMENPLAN_afkorting: Abbreviation
#'     \item EXAMENPLAN_verplicht: Mandatory indicator
#'     \item EXAMENPLAN_naam: Exam plan name
#'   }
#'
#' @importFrom dplyr select
#'
#' @export
ingest_exam_plans <- function(..., filename = NULL, path = NULL, config_key = "exam_plans", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- suppressWarnings(load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path))

    data_clean <- data_raw |>
        select(EXAMENPLAN_ID = ID,
               OPLEIDING_ID = OPLEIDING,
               COHORT_ID = COHORT,
               EXAMENPLAN_begindatum_omschrijving = CREATED_AT,
               EXAMENPLAN_einddatum_omschrijving = LAST_MODIFIED_AT,
               EXAMENPLAN_afkorting = AFKORTING,
               EXAMENPLAN_verplicht = VERPLICHT,
               EXAMENPLAN_naam = NAAM)

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest Prior Education Data
#'
#' @description
#' Reads and processes prior education data from a CSV file.
#'
#' @param filename Character string specifying the name of the CSV file to read
#' @param path Character string specifying the path to the CSV file
#' @param config_key Character string specifying the configuration key to use (default: "enrollments_prior_education")
#' @param config_data_path Character string specifying the config path for raw data (default: "data_raw_dir")
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed prior education data with columns:
#'   \itemize{
#'     \item DEELNEMER_ID: Student identifier
#'     \item Additional columns with prefix DEELNEMER_vooropleiding_
#'   }
#'
#' @details
#' The function standardizes column names by:
#' 1. Converting all names to lowercase using clean_names
#' 2. Renaming id_deelnemer to DEELNEMER_ID
#' 3. Prefixing remaining columns with DEELNEMER_vooropleiding_
#'
#' @importFrom dplyr rename rename_with contains
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove
#'
#' @export
ingest_students_prior_education <- function(..., filename = NULL, path = NULL, config_key = "students_prior_education", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    # TODO Deelnemer lijkt me niet 100%
    data_clean <- data_raw |>
        clean_names() |>
        rename(DEELNEMER_ID = id_deelnemer) |>
        rename_with(
            ~{rest_part <- str_remove(., "_vooropleiding")
            paste0("DEELNEMER_vooropleiding_", rest_part)
            },
            !contains("_ID")
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Ingest Team Retention Results
#'
#' @description
#' Reads and processes team retention data, including first-year student counts and progression numbers
#'
#' @param filename Optional. A string specifying the filename to read.
#' @param path Optional. A string specifying the path to the file.
#' @param config_key Optional. A string specifying the configuration key (default: "teams_results_retention_start").
#' @param config_data_path Optional. A string specifying the config path for raw data (default: "data_raw_dir").
#' @param ... Additional arguments passed to load_data().
#'
#' @returns
#' A tibble containing cleaned team retention data with columns:
#'   \itemize{
#'     \item SCHOOLJAAR_naam: School year
#'     \item TEAM_naam: Team name
#'     \item TEAM_aantal_eerstejaars: Number of first-year students
#'     \item TEAM_aantal_doorstroom: Number of continuing students
#'   }
#'
#' @importFrom janitor clean_names
#' @importFrom dplyr rename rename_with
#'
#' @export
ingest_teams_results_retention_start <- function(..., filename = NULL, path = NULL, config_key = "teams_results_retention_start", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          delim = ",",
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        clean_names() |>
        rename_with(~ paste0("TEAM_", .)) |>
        rename(SCHOOLJAAR_naam_met_streep = TEAM_teljaar,
               TEAM_naam = TEAM_team,
               TEAM_aantal_eerstejaars = TEAM_noemer_sr_1_jaars,
               TEAM_aantal_doorstroom = TEAM_teller_sr_1_jaars,
               TEAM_startersresultaat_1_jaars_omschrijving = TEAM_startersresultaat_1_jaars
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)
}

#' Helper Function to Ingest Student Satisfaction Data
#'
#' @description
#' Processes and cleans student satisfaction survey data from raw files
#'
#' @param config_key A string representing the school year (e.g., "2021")
#' @param ... Additional arguments passed to load_data
#' @param filename Optional. A string specifying the input file name
#' @param path Optional. A string specifying the path to the input file
#' @param config_data_path Optional. A string specifying the config path for raw data (default: "data_raw_dir")
#'
#' @returns
#' A cleaned tibble containing student satisfaction data with added school year information.
#' The original config key is stored as a comment attribute.
#'
#' @importFrom readr cols col_integer col_guess parse_number
#' @importFrom dplyr mutate select everything
#'
#' @export
ingest_students_satisfaction_helper <- function(config_key, ..., filename = NULL, path = NULL, config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          col_types = cols(
                              CREBO = col_integer(),
                              .default = col_guess()
                          ),
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        mutate(
            SCHOOLJAAR_startjaar = parse_number(config_key)
        ) |>
        select(
            SCHOOLJAAR_startjaar,
            everything()
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)
    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest Student Satisfaction Data for 2019
#'
#' @description
#' Load and process student satisfaction survey data from 2019
#'
#' @param ... Additional arguments passed to the underlying data loading function
#' @param filename Optional. A string specifying the input file name
#' @param path Optional. A string specifying the path to the input file
#' @param config_key Optional. A string specifying the configuration key (default: "students_satisfaction_2019")
#' @param config_data_path Optional. A string specifying the configuration path for raw data (default: "data_raw_dir")
#'
#' @returns
#' A cleaned dataset containing student satisfaction responses from 2019
#'
#' @export
ingest_students_satisfaction_2019 <- function(..., filename = NULL, path = NULL, config_key = "students_satisfaction_2019", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_students_satisfaction_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Student Satisfaction Data for 2021
#'
#' @description
#' Read and process student satisfaction survey data from 2021
#'
#' @param ... Additional arguments passed to the underlying data reading function
#' @param filename Optional. A string specifying the input file name
#' @param path Optional. A string specifying the file path
#' @param config_key Optional. A string specifying the configuration key (defaults to "students_satisfaction_2021")
#' @param config_data_path Optional. A string specifying the configuration data path (defaults to "data_raw_dir")
#'
#' @returns
#' A cleaned data frame containing student satisfaction data
#'
#' @export
ingest_students_satisfaction_2021 <- function(..., filename = NULL, path = NULL, config_key = "students_satisfaction_2021", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_students_satisfaction_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Student Satisfaction Data for 2023
#'
#' @description
#' Read and process student satisfaction survey data for the year 2023
#'
#' @param ... Additional arguments passed to readr::read_delim
#' @param filename Optional. A string specifying the input file name.
#' @param path Optional. A string specifying the path to the input file.
#' @param config_key Optional. A string specifying the configuration key (defaults to "students_satisfaction_2023").
#' @param config_data_path Optional. A string specifying the config path for raw data (defaults to "data_raw_dir").
#'
#' @returns
#' A tibble containing processed student satisfaction data, with CREBO as integer
#' and studentenhuisvesting_05 as integer columns, and other columns guessed.
#'
#' @importFrom readr cols col_integer col_guess
#'
#' @export
ingest_students_satisfaction_2023 <- function(..., filename = NULL, path = NULL, config_key = "students_satisfaction_2023", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_students_satisfaction_helper(
        config_key,
        col_types = cols(
            CREBO = col_integer(),
            studentenhuisvesting_05 = col_integer(),
            .default = col_guess()
        ),
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Helper Function to Ingest Employee Satisfaction Survey Answers
#'
#' @description
#' Loads and processes employee satisfaction survey data from a file
#'
#' @param config_key A string identifying the configuration and school year.
#' @param ... Additional arguments passed to `load_data()`.
#' @param filename Optional. Name of the file to load.
#' @param path Optional. Path to the file.
#' @param config_data_path Optional. Configuration path for raw data (default: "data_raw_dir").
#'
#' @returns
#' A cleaned data frame containing employee satisfaction survey responses with
#' an added school year column. The config key is stored as a comment attribute.
#'
#' @importFrom readr cols col_character col_guess parse_number
#' @importFrom dplyr mutate select everything
#'
#' @export
ingest_employee_answers_satisfaction_helper <- function(config_key, ..., filename = NULL, path = NULL, config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          col_types = cols(
                              QuestionId = col_character(),
                              .default = col_guess()
                          ),
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        mutate(
            SCHOOLJAAR_startjaar = parse_number(config_key)
        ) |>
        select(
            SCHOOLJAAR_startjaar,
            everything()
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest Employee Satisfaction Survey Answers for 2020
#'
#' @description
#' Import and process employee satisfaction survey responses from the 2020 survey
#'
#' @param filename Optional. A single string for the filename.
#' @param path Optional. A single string for the file path.
#' @param config_key Optional. A single string specifying the configuration key (default: "employees_satisfaction_2020").
#' @param config_data_path Optional. A single string specifying the config path for raw data (default: "data_raw_dir").
#' @param ... Additional arguments passed to the helper function.
#'
#' @returns
#' A tibble containing processed employee satisfaction survey responses.
#'
#' @export
ingest_employee_answers_satisfaction_2020 <- function(..., filename = NULL, path = NULL, config_key = "employees_satisfaction_2020", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employee_answers_satisfaction_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Employee Satisfaction Survey Answers 2022
#'
#' @description
#' Read and process employee satisfaction survey answers from 2022
#'
#' @param filename Optional. A string specifying the name of the input file.
#' @param path Optional. A string specifying the path to the input file.
#' @param config_key Optional. A string specifying the configuration key (default: "employees_satisfaction_2022").
#' @param config_data_path Optional. A string specifying the config path for raw data (default: "data_raw_dir").
#' @param ... Additional arguments passed to the underlying helper function.
#'
#' @returns
#' A cleaned data frame containing employee satisfaction survey responses.
#'
#' @export
ingest_employee_answers_satisfaction_2022 <- function(..., filename = NULL, path = NULL, config_key = "employees_satisfaction_2022", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employee_answers_satisfaction_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Employee Satisfaction Survey Data for 2024
#'
#' @description
#' Reads and processes employee satisfaction survey responses from 2024
#'
#' @param filename Optional. A string specifying the name of the input file.
#' @param path Optional. A string specifying the path to the input file.
#' @param config_key Optional. A string specifying the configuration key (default: "employees_satisfaction_2024").
#' @param config_data_path Optional. A string specifying the config path for raw data (default: "data_raw_dir").
#' @param ... Additional arguments passed to the underlying helper function.
#'
#' @returns
#' A tibble containing processed employee satisfaction survey data.
#'
#' @export
ingest_employee_answers_satisfaction_2024 <- function(..., filename = NULL, path = NULL, config_key = "employees_satisfaction_2024", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employee_answers_satisfaction_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Employee Satisfaction Survey Codebook
#'
#' @description
#' Reads and processes employee satisfaction survey codebook data with specific column type handling
#'
#' @param config_key A string indicating the configuration key to use.
#' @param ... Additional arguments passed to load_data.
#' @param filename Optional. A string specifying the input file name.
#' @param path Optional. A string specifying the input file path.
#' @param config_data_path Optional. A string specifying the configuration data path.
#'
#' @returns
#' Returns the processed data frame with QuestionId as character and other columns
#' guessed. The data is also saved using save_ingested().
#'
#' @importFrom readr cols col_character col_guess
#'
#' @export
ingest_employee_answers_satisfaction_codebook_helper <- function(config_key, ..., filename = NULL, path = NULL, config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          col_types = cols(
                              QuestionId = col_character(),
                              .default = col_guess()
                          ),
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw

    # keep the config with the data for later use
    comment(data_clean) <- config_key
    save_ingested(data_clean)

    # audit(data_clean, data_raw)
    return(data_clean)

}

#' Ingest 2020 Employee Satisfaction Survey Codebook
#'
#' @description
#' Load and process the 2020 employee satisfaction survey codebook data
#'
#' @param filename Optional. A single string for the input file name.
#' @param path Optional. A single string for the input file path.
#' @param config_key Optional. A single string specifying the configuration key (defaults to "employees_satisfaction_codebook_2020").
#' @param config_data_path Optional. A single string specifying the data directory in config (defaults to "data_raw_dir").
#' @param ... Additional arguments passed to the underlying helper function.
#'
#' @returns
#' A processed data frame containing the employee satisfaction survey codebook data.
#'
#' @export
ingest_employee_answers_satisfaction_codebook_2020 <- function(..., filename = NULL, path = NULL, config_key = "employees_satisfaction_codebook_2020", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employee_answers_satisfaction_codebook_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Employee Satisfaction Codebook Data for 2022
#'
#' @description
#' Load and process employee satisfaction survey codebook data from 2022
#'
#' @param filename Optional. A string specifying the name of the file to read.
#' @param path Optional. A string specifying the path to the file.
#' @param config_key Optional. A string specifying the configuration key (default: "employees_satisfaction_codebook_2022").
#' @param config_data_path Optional. A string specifying the config path for raw data (default: "data_raw_dir").
#' @param ... Additional arguments passed to the underlying helper function.
#'
#' @returns
#' A cleaned data frame containing employee satisfaction codebook data.
#'
#' @export
ingest_employee_answers_satisfaction_codebook_2022 <- function(..., filename = NULL, path = NULL, config_key = "employees_satisfaction_codebook_2022", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employee_answers_satisfaction_codebook_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}

#' Ingest Employee Satisfaction Survey Codebook Data for 2024
#'
#' @description
#' Reads and processes employee satisfaction survey codebook data from a file
#'
#' @param filename Optional. A string specifying the name of the file to read.
#' @param path Optional. A string specifying the path to the file.
#' @param config_key Optional. A string specifying the configuration key to use (default: "employees_satisfaction_codebook_2024").
#' @param config_data_path Optional. A string specifying the config path for raw data (default: "data_raw_dir").
#' @param ... Additional arguments passed to the underlying data loading function.
#'
#' @returns
#' A tibble containing processed employee satisfaction codebook data.
#'
#' @export
ingest_employee_answers_satisfaction_codebook_2024 <- function(..., filename = NULL, path = NULL, config_key = "employees_satisfaction_codebook_2024", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_clean <- ingest_employee_answers_satisfaction_codebook_helper(
        config_key,
        ...,
        filename = filename,
        path = path,
        config_data_path = config_data_path)

    return(data_clean)
}
