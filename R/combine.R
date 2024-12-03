

combine_enrollments <- function(enrollments_basic, enrollment_flex_status, enrollment_levels, applications, cohorts, teams) {
    data <- enrollments_basic |>
        left_join(enrollment_levels, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
        left_join(enrollment_flex_status, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
        left_join(applications, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
        left_join(programmes_basic, by = join_by(OPLEIDING_ID), relationship = "many-to-one") |>
        left_join(cohorts, by = join_by(COHORT_ID), relationship = "many-to-one") |>
        left_join(teams, by = join_by(TEAM_ID), relationship = "many-to-one")
}
