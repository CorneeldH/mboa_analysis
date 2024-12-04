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

    # add check if there is config applications
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
        distinct()

    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
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
               VERBINTENIS_is_flex_omschrijving = IsFlex)

    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
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
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing processed programme data with columns prefixed with OPLEIDING_
#'
#' @importFrom dplyr select rename_with
#' @importFrom janitor clean_names
#'
#' @export
ingest_programmes_basic <- function(..., filename = NULL, path = NULL, config_key = "programmes_basic", config_data_path = "data_raw_dir") {

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

    # add check if there is config applications
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
#' @param ... Additional arguments passed to readr::read_delim
#'
#' @return A tibble containing enrollment level data with columns:
#'   \itemize{
#'     \item VERBINTENIS_ID: Unique enrollment identifier
#'     \item VERBINTENIS_niveau: Enrollment level
#'   }
#'
#' @export
ingest_enrollments_level <- function(..., filename = NULL, path = NULL, config_key = "enrollments_level", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          # add col names since they lack in the file
                          col_names = c("VERBINTENIS_ID", "VERBINTENIS_niveau"),
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw

    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
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
ingest_enrollments_basic <- function(..., filename = NULL, path = NULL, config_key = "enrollments_basic", config_data_path = "data_raw_dir") {

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

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}



#' Ingest Application Information
#'
#' @description
#' Reads and processes application data from a CSV file, selecting relevant columns
#' and standardizing column names. Expects CSV files with semicolon (;) as separator.
#'
#' @param ... Additional arguments passed to readr::read_delim
#' @param filename Character string specifying the name of the CSV file to read
#' @param config_value Character string specifying the config key to use
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
ingest_applications <- function(..., filename = NULL, path = NULL, config_key = "applications", config_data_path = "data_raw_dir") {

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

    # add check if there is config applications
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
#'                   Defaults to "enrollment_absences".
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
ingest_enrollment_absences <- function(..., filename = NULL, path = NULL, config_key = "enrollment_absences", config_data_path = "data_raw_dir") {

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

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}

ingest_group_participation <- function(..., filename = NULL, path = NULL, config_key = "group_participation", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            ID_Groep,
            ID_Deelnemer,
            ID_Team = ID_Organisatieeenheid,
            CODE,
            naam = NAAM...6,
            type = NAAM...11
        ) |>
        clean_names() |>
        rename_with(
            ~ str_remove(., "^id_") |>
                toupper() |>
                paste0("_ID"),
            matches("^id_")
        ) |>
        rename_with(
            ~paste0("GROEP_", .),
            !contains("ID")
        )

    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}

ingest_reasons_for_leaving <- function(..., filename = NULL, path = NULL, config_key = "reasons_for_leaving", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            REDEN_UITSCHRIJVING_ID = ID,
            # REDEN_actief = ACTIEF, # gaat over gebruik van deze reden / code
            # OVERLIJDEN, # REDENUITVAL ook
            VERBINTENIS_reden_uitschrijving = Redenuitschrijving
        )


    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}

ingest_employee_absences <- function(..., filename = NULL, path = NULL, config_key = "employee_absences", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            kostenplaats_code = `Kostenplaats Afdeling`,
            `ID Medewerker`,
            `ID Functie`,
            `Eerste verzuimdag`,
            `Laatste verzuimdag`,
            #`Soort Verzuim`, privacy-gevoelig
            percentage_verzuim = HR2D__SICKPERC__C
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

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}

ingest_job_contracts <- function(..., filename = NULL, path = NULL, config_key = "job_contracts", config_data_path = "data_raw_dir") {

    # Name arguments since order behind ... is not guaranteed
    data_raw <- load_data(config_key,
                          ...,
                          filename = filename,
                          path = path,
                          config_data_path = config_data_path)

    data_clean <- data_raw |>
        select(
            kostenplaats_code = `Kostenplaats Afdeling`,
            kostenplaats_omschrijving = `Naam afdeling`,
            begin_datum_omschrijving = HR2D__GELDIG_VAN__C...8,
            eind_datum_omschrijving = HR2D__GELDIG_TOT__C...9,
            MEDEWERKER_ID = `ID Medewerker`,
            functie = Functie,
            fte = HR2D__DEELTIJDFACTOR__C,
            afwijking_kostenplaats = NAME...11,
            afwijking_omschrijving = NAME...12,
            afwijking_begin_datum_omschrijving = HR2D__GELDIG_VAN__C...13,
            afwijking_eind_datum_omschrijving = HR2D__GELDIG_TOT__C...14,
            afwijking_fte = HR2D__AANTAL__C
        ) |>
        rename_with(~ paste0("CONTRACT_", .),
                    !contains("ID"))


    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}

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

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}

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
            bc_label = BeroepopleidingLabel,
            Niveau,
            NiveauBeroep
        ) |>
        clean_names() |>
        rename_with(~ paste0("OPLEIDING_", .))


    # keep the config with the data for later use
    comment(data_clean) <- config_key

    # add check if there is config applications
    # audit(data_clean, data_raw)
    return(data_clean)

}
