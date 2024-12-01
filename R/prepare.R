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
    data <- data |>
        mutate(AANMELDING_dagen_in_proces = round(as.numeric(
            difftime(
                AANMELDING_laatst_gewijzigd_datum,
                AANMELDING_begin_datum,
                units = "days"
            )
        )))
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
#' enrollments <- combine_enrollment_numbers_for_order(raw_enrollments)
#' }
combine_enrollment_numbers_for_order <- function(data) {

    data <- data |>
        mutate(VERBINTENIS_volg_en_blad_nummers = VERBINTENIS_volgnummer * 1000 + VERBINTENIS_bladnummer)
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

    data <- data |>
        mutate(VERBINTENIS_is_flex = ifelse(VERBINTENIS_is_flex_omschrijving == "ja", TRUE, FALSE))
}
