

test <- enrollments_combined_enriched |>
    count(VERBINTENIS_aanmelding_begin_dagen_tot_start) #, VERBINTENIS_aanmelding_begin_datum, VERBINTENIS_begindatum)

ggplot(test, aes(x = VERBINTENIS_aanmelding_begin_dagen_tot_start, y = n)) +
    # scale y max 400
    geom_bar(stat = "identity") +
    geom_point() +
    ylim(0, 400)

test2 <- enrollments_combined_enriched2 |>
    count(VERBINTENIS_aanmelding_afgerond_dagen_tot_start) #, VERBINTENIS_aanmelding_begin_datum, VERBINTENIS_begindatum

ggplot(test2, aes(x = VERBINTENIS_aanmelding_afgerond_dagen_tot_start, y = n)) +
    # scale y max 400
    geom_bar(stat = "identity") +
    geom_point() +
    ylim(0, 400)
