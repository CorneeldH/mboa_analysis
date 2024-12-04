#' Calculate the Duration of Application Process
#'
#' @description
#' Takes a data frame containing application data and calculates the number of days
#' between the application start date and last modification date.
#'
#' @param data A data frame containing application data with columns
#'             AANMELDING_begin_datum and AANMELDING_laatst_gewijzigd_datum
#'
#' @return A data frame with an additional column AANMELDING_dagen_in_proces
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
#' applications <- calculate_application_duration(raw_applications)
#' }
calculate_application_duration <- function(data) {
    data_prepared <- data |>
        mutate(AANMELDING_dagen_in_proces = round(as.numeric(
            difftime(
                AANMELDING_laatst_gewijzigd_datum,
                AANMELDING_begin_datum,
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
#' enrollments_basic <- combine_enrollment_numbers_for_order(raw_enrollments_basic)
#' }
combine_enrollment_numbers_for_order <- function(data) {

    data_prepared <- data |>
        mutate(VERBINTENIS_volg_en_blad_nummers = VERBINTENIS_volgnummer * 1000 + VERBINTENIS_bladnummer)

    return(data_prepared)
}


#' Create Boolean Flex Status
#'
#' @description
#' Converts flex status from text ("ja") to boolean (TRUE/FALSE).
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
        mutate(VERBINTENIS_is_flex = ifelse(VERBINTENIS_is_flex_omschrijving == "ja", TRUE, FALSE))

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
        group_by(VERBINTENIS_ID, type) %>%
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

transform_job_contracts_to_teacher_terms <- function(data) {

    data_prepared <- data |>
        mutate(
            CONTRACT_begin_datum = as.Date(CONTRACT_begin_datum_omschrijving, format = "%d-%m-%Y"),
            CONTRACT_eind_datum = as.Date(CONTRACT_eind_datum_omschrijving,  format = "%d-%m-%Y"),
            CONTRACT_fte_numeriek = suppressWarnings(as.numeric(CONTRACT_fte)),
            CONTRACT_fte_gecorrigeerd = ifelse(CONTRACT_fte_numeriek == 100,
                                               1,
                                               CONTRACT_fte_numeriek)
        ) |>
        group_by(across(-contains("afwijking"))) |>
        summarize(
            .groups = "drop"
        )

    peildatums <- data.frame(
        peildatum = seq.Date(
            from = as.Date("2013-10-01"),
            to = as.Date("2050-10-01"),
            by = "1 year"
        )
    )

    # Assuming your data is called 'data_prepared'
    result <- data_prepared %>%
        # Convert NA to actual dates if needed
        mutate(
            CONTRACT_begin_datum = as.Date(CONTRACT_begin_datum),
            CONTRACT_eind_datum = as.Date(CONTRACT_eind_datum)
        ) %>%
        # Cross join with peildatums
        cross_join(peildatums) %>%
        # Keep only rows where peildatum falls between contract dates
        filter(
            peildatum >= CONTRACT_begin_datum | is.na(CONTRACT_begin_datum),
            peildatum <= CONTRACT_eind_datum
        )

    # > tabyl(result$peildatum)
    # result$peildatum    n      percent
    # 2013-10-01 1286 0.1123732961
    # 2014-10-01 1236 0.1080041943
    # 2015-10-01 1284 0.1121985320
    # 2016-10-01 1243 0.1086158686
    # 2017-10-01 1185 0.1035477106
    # 2018-10-01 1171 0.1023243621
    # 2019-10-01 1177 0.1028486543
    # 2020-10-01 1211 0.1058196435
    # 2021-10-01  476 0.0415938483
    # 2022-10-01  499 0.0436036351
    # 2023-10-01  377 0.0329430269
    # 2024-10-01  281 0.0245543516
    # 2025-10-01   15 0.0013107305
    # 2026-10-01    3 0.0002621461

    # data_prepared <- data |>
    #     mutate(
    #         CONTRACT_begin_datum = as.Date(CONTRACT_begin_datum, format = "%d-%m-%Y"),
    #         CONTRACT_eind_datum = as.Date(CONTRACT_eind_datum,  format = "%d-%m-%Y"),
    #         CONTRACT_afwijking_fte = suppressWarnings(as.numeric(CONTRACT_afwijking_fte))
    #     ) |>
    #     filter(is.null(CONTRACT_eind_datum) | CONTRACT_eind_datum > "2019-10-01",
    #            ) |>
    #     group_by(across(-c(CONTRACT_afwijking_fte, CONTRACT_afwijking_omschrijving))) |>
    #     summarize(CONTRACT_afwijking_fte = sum(CONTRACT_afwijking_fte, na.rm = TRUE), .groups = "drop")


}

create_employee_absence_type_cat <- function(data) {

    # onverwachte_soorten <- c(
    #     "Ziekte",
    #     "Ziekte (no-risk van toepassing)",
    #     "Ongeval door derde",
    #     "Ziekteverlof i.v.m. zwangerschap"
    # )
    #
    # data_prepared <- data |>
    #     mutate(
    #         absence_cat = case_when(
    #             MEDEWERKER_soort_verzuim %in% onverwachte_soorten  ~ "Onverwacht",
    #             TRUE ~ "Zwangerschap_geboorte"
    #         )
    #     )
}

create_employee_absence_length_cat <- function(data) {

    data_prepared <- data |>
        mutate(
            MEDEWERKER_verzuim_duur = as.numeric(
                difftime(MEDEWERKER_laatste_verzuimdag,
                         MEDEWERKER_eerste_verzuimdag,
                         units = "days")
                ),
            MEDEWEKRER_verzuim_duur_cat = case_when(
                MEDEWERKER_verzuim_duur < 7 ~ "Kort",
                MEDEWERKER_verzuim_duur < 42 ~ "Middellang",
                TRUE ~ "Lang"
            )
        )
}

