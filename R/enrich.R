#' Calculate Days Between Exam Plan Dates and Start Date
#'
#' @description
#' Calculate the number of days between various exam plan dates and the cohort start date
#'
#' @param data A data frame containing exam plan dates and cohort start dates
#'
#' @returns
#' A data frame with additional columns calculating days between exam plan dates
#' and cohort start date. Values outside the range of -365 to 365 days or infinite
#' values are converted to NA.
#'
#' @importFrom dplyr mutate across if_else matches
#'
#' @export
calculate_exam_plan_to_start <- function(data) {
    data_enriched <- data |>
        mutate(
            OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_verplicht_begindatum),
            OPLEIDING_examen_plan_keuze_begin_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_keuze_begindatum),
            OPLEIDING_examen_plan_verplicht_eind_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_verplicht_einddatum),
            OPLEIDING_examen_plan_keuze_eind_dagen_tot_start = as.numeric(COHORT_start_datum - OPLEIDING_examen_plan_keuze_einddatum),
            OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start = if_else(OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start == Inf | OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start > 365 | OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start < -365, NA_integer_, OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start),
            # Set unrealistic values to NA
            across(matches(".*examen_plan.*dagen_tot_start"),
                   ~if_else(. == Inf | . > 365 | . < -365, NA_integer_, .))
        )

    return(data_enriched)

}

#' Calculate Days Between Application and Start Date
#'
#' @description
#' Calculates the number of days between application dates and enrollment start dates
#'
#' @param data A data frame containing columns:
#'   \itemize{
#'     \item AANMELDING_begin_datum: Application start date
#'     \item AANMELDING_laatst_gewijzigd_datum: Last application modification date
#'     \item VERBINTENIS_begindatum: Enrollment start date
#'   }
#'
#' @returns
#' The input data frame with two additional columns:
#'   \itemize{
#'     \item AANMELDING_begin_dagen_tot_start: Days between application and start
#'     \item AANMELDING_afgerond_dagen_tot_start: Days between last modification and start
#'   }
#' Values greater than 365 days or less than 0 days are set to NA.
#'
#' @importFrom dplyr mutate if_else
#'
#' @export
calculate_application_to_start <- function(data) {

    data_enriched <- data |>
        mutate(
            AANMELDING_begin_datum = as.Date(AANMELDING_begin_datum, format = "%Y-%m-%d"),
            AANMELDING_laatst_gewijzigd_datum = as.Date(AANMELDING_laatst_gewijzigd_datum, format = "%Y-%m-%d"),
            VERBINTENIS_begindatum = as.Date(VERBINTENIS_begindatum, format = "%d-%m-%Y"),
            AANMELDING_begin_dagen_tot_start = as.numeric(VERBINTENIS_begindatum - AANMELDING_begin_datum),
            AANMELDING_afgerond_dagen_tot_start = as.numeric(VERBINTENIS_begindatum - AANMELDING_laatst_gewijzigd_datum)
        ) |>
        # Set unrealistic values to NA
        mutate(AANMELDING_begin_dagen_tot_start = if_else(
            AANMELDING_begin_dagen_tot_start >= 365 | AANMELDING_begin_dagen_tot_start < 0,
            NA_integer_,
            AANMELDING_begin_dagen_tot_start
        )) |>
        mutate(AANMELDING_afgerond_dagen_tot_start = if_else(
            AANMELDING_afgerond_dagen_tot_start >= 365 | AANMELDING_afgerond_dagen_tot_start < 0,
            NA,
            AANMELDING_afgerond_dagen_tot_start
        ))


    return(data_enriched)

}

#' Calculate Days Between BPV Status and Start Date
#'
#' @description
#' Calculate the number of days between various BPV status dates and the enrollment start date
#'
#' @param data A data frame containing BPV status dates and enrollment start date
#'
#' @returns
#' The input data frame with three additional columns:
#' \itemize{
#'   \item BPV_definitief_dagen_tot_start: Days between definitive status and start
#'   \item BPV_volledig_dagen_tot_start: Days between complete status and start
#'   \item BPV_begin_dagen_tot_start: Days between initial status and start
#' }
#'
#' @importFrom dplyr mutate
#'
#' @export
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


#' Calculate Exam Plan Date Comparisons
#'
#' @description
#' Adds columns comparing exam plan dates to April 1st of the cohort's start year
#'
#' @param data A dataframe containing exam plan dates and COHORT_startj
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

#' Fix Missing Absence Values
#'
#' @description
#' Convert NA values in absence columns to zeros to indicate no absences
#'
#' @param data A data frame with absence columns (containing "verzuim")
#'
#' @returns
#' A data frame with NA values in absence columns converted to zeros
#'
#' @importFrom dplyr mutate across
#'
#' @export
fix_absences <- function(data) {

    # NA on absence after joining to all employees means the employee was not absent in that year
    data_enriched <- data |>
        mutate(across(contains("verzuim"), ~if_else(is.na(.), 0, .)))

    return(data_enriched)

}

#' Create Total FTE
#'
#' @description
#' Calculate the total FTE by adding contract FTE and FTE adjustments
#'
#' @param data A data frame containing columns MEDEWERKER_contract_fte and
#'   MEDEWERKER_contract_fte_aanpassing
#'
#' @returns
#' A data frame with an additional column MEDEWERKER_contract_fte_totaal
#'
#' @importFrom dplyr mutate
#'
#' @export
create_total_fte <- function(data) {

    data_enriched <- data |>
        mutate(
            MEDEWERKER_contract_fte_totaal = MEDEWERKER_contract_fte + MEDEWERKER_contract_fte_aanpassing
        )

    return(data_enriched)

}

#' Add Filled Boolean Columns
#'
#' @description
#' Create new boolean columns with NA values replaced by a specified value
#'
#' @param data A data frame containing logical columns with "_is_" in their names
#' @param fill Optional. A logical value to replace NA values with (default: FALSE)
#' @param text Optional. A string to append to the new column names (default: "opgevuld")
#'
#' @returns
#' A data frame with additional boolean columns. New columns are named as
#' original_name_text where NA values are replaced with the fill value.
#'
#' @importFrom dplyr mutate across where
#'
#' @export
add_filled_booleans <- function(data, fill = FALSE, text = "opgevuld") {

    new_col_names_spec = paste0("{.col}_", text)

    data_enriched <- data |>
        mutate(
            # If boolean is NA, set to FALSE and create new var
            across(
                where(is.logical) & where(~any(is.na(.))),
                ~if_else(is.na(.), fill, .), .names = new_col_names_spec
            )
        )

    return(data_enriched)
}

#' Calculate Student-Staff Ratio
#'
#' @description
#' Calculate the ratio between students and staff FTE per team
#'
#' @param teams A data frame containing team data with columns TEAM_studenten_aantal and MEDEWERKER_contract_fte_totaal.
#'
#' @returns
#' The input data frame with an additional column TEAM_student_staf_ratio.
#'
#' @importFrom dplyr mutate
#'
#' @export
create_student_staff_ratio <- function(teams) {
    teams_enriched <- teams |>
        mutate(TEAM_student_staf_ratio = TEAM_studenten_aantal / MEDEWERKER_contract_fte_totaal)

    return(teams_enriched)
}

#' Create Student Growth Variables for Teams
#'
#' @description
#' Calculate year-over-year growth metrics for student numbers in teams
#'
#' @param teams A data frame containing team-level student counts with columns
#' 'TEAM_naam', 'COHORT_startjaar', and 'TEAM_studenten_aantal'
#'
#' @returns
#' A data frame with additional columns:
#' \itemize{
#'   \item TEAM_studenten_aantal_vorig_jaar: Previous year's student count
#'   \item TEAM_student_aantal_groei: Absolute growth in student numbers
#'   \item TEAM_studenten_aantal_pct_groei: Percentage growth in student numbers
#' }
#'
#' @importFrom dplyr arrange group_by mutate ungroup lag
#'
#' @export

create_student_growth_vars <- function(teams) {
    teams_enriched <- teams |>
        arrange(TEAM_naam, COHORT_startjaar) |>
        group_by(TEAM_naam) |>
        mutate(
            TEAM_studenten_aantal_vorig_jaar = lag(TEAM_studenten_aantal),
            TEAM_student_aantal_groei = TEAM_studenten_aantal - TEAM_studenten_aantal_vorig_jaar,
            TEAM_studenten_aantal_pct_groei = (
                (TEAM_studenten_aantal - TEAM_studenten_aantal_vorig_jaar) / TEAM_studenten_aantal_vorig_jaar
            ) |>
                round(2)
        ) |>
        ungroup()
}

#' Replace infinite values with NA
#'
#' @description
#' Replace infinite values in numeric columns with NA
#'
#' @param data A data frame or tibble.
#'
#' @returns
#' A data frame with the same structure as the input, but with infinite values
#' replaced by NA in all numeric columns.
#'
#' @importFrom dplyr mutate across where
#'
#' @export
fix_inf_values <- function(data) {
    data_fixed <- data |>
        mutate(across(where(is.numeric),
                      ~if_else(. == Inf, NA_real_, .)))
}
