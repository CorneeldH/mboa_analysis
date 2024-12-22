
calculate_exam_plan_to_start <- function(data) {
    data_enriched <- data |>
        mutate(
            OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_verplicht_begindatum),
            OPLEIDING_examen_plan_keuze_begin_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_keuze_begindatum),
            OPLEIDING_examen_plan_verplicht_eind_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_verplicht_einddatum),
            OPLEIDING_examen_plan_keuze_eind_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_keuze_einddatum),

            across(matches(".*examen_plan.*dagen_tot_start"),
                   ~if_else(. == Inf | . > 365 | . < -365, NA_integer_, .))
        )

    return(data_enriched)

}

calculate_application_to_start <- function(data) {

    data_enriched <- data |>
        mutate(
            VERBINTENIS_aanmelding_begin_datum = as.Date(VERBINTENIS_aanmelding_begin_datum, format = "%Y-%m-%d"),
            VERBINTENIS_aanmelding_laatst_gewijzigd_datum = as.Date(VERBINTENIS_aanmelding_laatst_gewijzigd_datum, format = "%Y-%m-%d"),
            VERBINTENIS_begindatum = as.Date(VERBINTENIS_begindatum, format = "%d-%m-%Y"),
            VERBINTENIS_aanmelding_begin_dagen_tot_start = as.numeric(VERBINTENIS_begindatum - VERBINTENIS_aanmelding_begin_datum),
            VERBINTENIS_aanmelding_afgerond_dagen_tot_start = as.numeric(VERBINTENIS_begindatum - VERBINTENIS_aanmelding_laatst_gewijzigd_datum)
        ) |>
        # Set unrealistic values to NA
        mutate(VERBINTENIS_aanmelding_begin_dagen_tot_start = if_else(
            VERBINTENIS_aanmelding_begin_dagen_tot_start >= 365 | VERBINTENIS_aanmelding_begin_dagen_tot_start < 0,
            NA_integer_,
            VERBINTENIS_aanmelding_begin_dagen_tot_start
        )) |>
        mutate(VERBINTENIS_aanmelding_afgerond_dagen_tot_start = if_else(
            VERBINTENIS_aanmelding_afgerond_dagen_tot_start >= 365 | VERBINTENIS_aanmelding_afgerond_dagen_tot_start < 0,
            NA,
            VERBINTENIS_aanmelding_afgerond_dagen_tot_start
        ))


    return(data_enriched)

}

calculate_bpv_status_to_start <- function(data) {

    data_enriched <- data |>
        mutate(
            BPV_definitief_dagen_tot_start = as.numeric(BPV_status_definitief_datum - VERBINTENIS_begindatum),
            BPV_volledig_dagen_tot_start = as.numeric(BPV_status_volledig_datum - VERBINTENIS_begindatum),
            BPV_begin_dagen_tot_start = as.numeric(BPV_status_begin_datum - VERBINTENIS_begindatum)
        )

    return(data_enriched)

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

    data_enriched <- data |>
        mutate(
            datum_definitief = as.Date(paste0(COHORT_startjaar, "-10-01")),
            BPV_is_definitief_voor_1_okt = BPV_status_definitief_datum < datum_definitief,
            datum_volledig = as.Date(paste0(COHORT_startjaar + 1, "-01-01")),
            BPV_is_volledig_voor_1_jan = BPV_status_volledig_datum < datum_volledig

        )

    return(data_enriched)

}


calculate_exam_plan_to_specific_dates <- function(data) {

    data_enriched <- data |>
        mutate(
            datum_exampenplan_af = as.Date(paste0(COHORT_startjaar, "-04-01")),
            OPLEIDING_examen_plan_verplicht_begin_is_voor_1_april =
                OPLEIDING_examen_plan_verplicht_begindatum < datum_exampenplan_af,
            OPLEIDING_examen_plan_keuze_begin_is_voor_1_april =
                OPLEIDING_examen_plan_keuze_begindatum < datum_exampenplan_af,
            OPLEIDING_examen_plan_verplicht_eind_is_voor_1_april =
                OPLEIDING_examen_plan_verplicht_einddatum < datum_exampenplan_af,
            OPLEIDING_examen_plan_keuze_eind_is_voor_1_april =
                OPLEIDING_examen_plan_keuze_einddatum < datum_exampenplan_af
        )

    return(data_enriched)

}

