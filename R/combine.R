

combine_enrollments <- function(enrollments_basic, enrollments_flex, enrollments_level, applications, cohorts, teams) {
    data <- enrollments_basic |>
        left_join(enrollments_level, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
        left_join(enrollments_flex, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
        left_join(applications, by = join_by(VERBINTENIS_ID), relationship = "many-to-one") |>
        left_join(programmes_basic, by = join_by(OPLEIDING_ID), relationship = "many-to-one") |>
        left_join(cohorts, by = join_by(COHORT_ID), relationship = "many-to-one") |>
        left_join(teams, by = join_by(TEAM_ID), relationship = "many-to-one")
}
