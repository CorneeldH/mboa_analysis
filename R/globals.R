
utils::globalVariables(
    c(
        # Model variables
        ".estimate", ".metric", "Importance", "Variable", "avg_importance", 
        "baseline", "group", "pct", "reorder", "tune", "ylim", "geom_vline",
        "%+%",
        
        # From add_grouped_enrollment_dates
        "OPLEIDING_bc_code",

        # From calculate_active_on_1_okt
        "VERBINTENIS_begindatum_eerst",
        "VERBINTENIS_eerste_1_okt_eerst",

        # From calculate_is_havo_vwo_dropout
        "DEELNEMER_vooropleiding_hoogste_diploma_soort",
        "DEELNEMER_vooropleiding_hoogst",

        # From ingest_mapping_employees_satisfaction
        "mto_characteristic_1",
        "mto_characteristic_2",
        "mto_characteristic_3",

        # From summarise_enrollments_num_vars_to_teams
        "DEELNEMER_passend_niveau",

        # From summarise_observations_to_weekly_attendance
        "school_start_date",
        "week_number",
        "school_start_week",
        "adjusted_week",

        # From transform_prior_education_vo_and_highest_degree
        "DEELNEMER_vooropleiding_diploma_behaald",

        # ingest_cohorts
        "ID",
        "NAAM",
        "ID_cohort",
        "COHORT_naam",

        # ingest_teams
        "Cluster",
        "ClusterAfk",
        "School",
        "SchoolAfk",
        "Team",
        "TeamAfk",
        "Kostenplaats",
        "TEAM_kostenplaats",
        "SK_Kostenplaats",
        "SK_KostenplaatsHR2Day",
        "team",
        "team_afk",
        "ORG1ID",
        "org1id",

        # ingest_programmes_basics
        "ID",
        "CODE",
        "WERVINGSNAAM",
        "LEERWEG",
        "DEFAULTINTENSITEIT",
        "COMMUNICERENMETBRON",
        "NEGEERLANDELIJKECRITERIA",
        "NEGEERLANDELIJKEPRODUCTREGELS",
        "id",

        # ingest_flex
        "ID Verbintenis",
        "IsFlex",

        # ingest_enrollments_application
        "BEGINDATUM",
        "LAST_MODIFIED_AT",
        "AANMELDINGVOOREERSTELEERJAAR",
        "VERBINTENIS",

        # calculate_application_duration
        "AANMELDING_begin_datum",
        "AANMELDING_laatst_gewijzigd_datum",

        # create_flex_boolean
        "VERBINTENIS_is_flex_omschrijving",

        # combine_enrollment_numbers_for_order
        "VERBINTENIS_volgnummer",
        "VERBINTENIS_bladnummer",

        # combine
        "VERBINTENIS_ID",

        # combine_enrollments
        "OPLEIDING_ID",
        "COHORT_ID",
        "TEAM_ID",
        "programmes_basics",

        # create_employee_absence_length_cat
        "MEDEWERKER_eerste_verzuimdag",
        "MEDEWERKER_laatste_verzuimdag",

        # ingest_employee_absences
        "Kostenplaats Afdeling",
        "ID Medewerker",
        "ID Functie",
        "Eerste verzuimdag",
        "Laatste verzuimdag",
        "HR2D__SICKPERC__C",

        # ingest_attendance_observations
        "SK_GroepInschrijving",
        "SK_Leereenheid",
        "Datum",
        "SK_afspraak",
        "Waarnemingsduur",
        "Presentietekst",

        # ingest_enrollments_basics
        "ORGANISATIE_EENHEID_ID",

        # ingest_group_participation
        # "ID_Groep",
        # "ID_Deelnemer",
        # "ID_Organisatieeenheid",
        # "NAAM...6",
        # "NAAM...11",

        # ingest_employees_contract
        # "Naam afdeling",
        # "HR2D__GELDIG_VAN__C...8",
        # "HR2D__GELDIG_TOT__C...9",
        # "Functie",
        # "HR2D__DEELTIJDFACTOR__C",
        # "NAME...11",
        # "NAME...12",
        # "HR2D__GELDIG_VAN__C...13",
        # "HR2D__GELDIG_TOT__C...14",
        # "HR2D__AANTAL__C",

        # ingest_programme_bc_codes
        "Code",
        "BeroepOpleidingCode",
        "BeroepopleidingLabel",
        "Niveau",
        "NiveauBeroep",

        # ingest_reasons_for_leaving
        "ID Reden Uitschrijving",
        "ACTIEF",
        "Reden Uitschrijving",

        # ingest_study_advices
        "Aanmaakdatum",
        "Voorlopig",
        "Definitief",
        "id_verbintenis",

        # transform_advices_to_enrollments
        "type",
        "VERBINTENIS_advies_aanmaakdatum",

        # transform_employees_contract_to_teacher_terms
        # "CONTRACT_begin_datum_omschrijving",
        # "CONTRACT_eind_datum_omschrijving",
        # "CONTRACT_fte",
        # "CONTRACT_fte_numeriek",
        # "CONTRACT_begin_datum",
        # "CONTRACT_eind_datum",
        # "peildatum",
        # "MEDEWERKER_ID",

        # ingest_student_demographics
        "id_deelnemer",

        # ingest_enrollments_special_needs
        "IDVerbintenis",
        "Kenmerk_Passend",

        # summarise_special_needs
        "VERBINTENIS_passend_onderwijs_kenmerk",

        # transform_students_to_student_year
        "DEELNEMER_begindatum",
        "DEELNEMER_einddatum",
        "DEELNEMER_ID",

        # ingest_enrollments_ibp
        "ID_Verbintenis",
        "IBPAanmaakdatum",

        # ingest_bpv_registrations
        "TOTALEOMVANG",
        "VERWACHTEEINDDATUM",

        # ingest_bpv_statusses
        "ID_BPVInschrijving",
        "CREATED_AT",
        "NAARSTATUS",
        "ORGANISATIE",

        # ingest_enrollment_to_sk_mapping
        "SK_Inschrijving",

        # transform_observations_to_enrollments
        "total_valid_duration",

        # filter_cohorts
        "COHORT_start_year",

        # ingest_exam_plans
        "OPLEIDING",
        "COHORT",
        "AFKORTING",
        "VERPLICHT",

        # ingest_employees_contract_basics_helper
        "FTE",

        # ingest_job_components_extra_fte_helper
        "FTE Looncomponent",

        # summarise_plan_dates_to_programmes
        "EXAMENPLAN_begindatum_omschrijving",
        "EXAMENPLAN_einddatum_omschrijving",
        "EXAMENPLAN_begindatum",
        "EXAMENPLAN_verplicht",
        "EXAMENPLAN_einddatum",

        # transform_statusses_to_enrollments
        "BPV_ID",
        "BPV_status_begin_datum",
        "BPV_status",
        "BPV_omvang",
        "BPV_verwachte_eind_datum",
        "VERBINTENIS_bpv_status_begin_datum",

        # add_school_year
        "MEDEWERKER_contract_fte_peildatum",

        # summarise_employee_absence_to_weeks
        "datum",
        "MEDEWERKER_ID",
        "TEAM_kostenplaats_code",
        "MEDEWERKER_verzuim_week_nummer",
        "SCHOOLJAAR_naam",
        "verzuim_percentage",
        "MEDEWERKER_verzuim_totaal_week",

        # calculate_application_duration
        "AANMELDING_laatst_gewijzigd_datum",
        "AANMELDING_begin_datum",

        # convert_absence_types
        "MEDEWERKER_percentage_verzuim",

        # expand_to_daily
        "datum",
        "MEDEWERKER_ID",
        "TEAM_kostenplaats_code",
        "SCHOOLJAAR_naam",
        "verzuim_percentage",

        # filter_cohorts
        "COHORT_startjaar",

        # split_absences_into_school_years
        "SCHOOLJAAR_startdatum",
        "SCHOOLJAAR_einddatum",
        "SCHOOLJAAR_naam",

        # summarise_components_to_employees
        "MEDEWERKER_ID",
        "TEAM_kostenplaats_code",
        "MEDEWERKER_contract_fte_peildatum",
        "MEDEWERKER_contract_fte_aanpassing",

        # transform_prior_education_to_student_year
        "COHORT_startdatum",
        "DEELNEMER_vooropleiding_einddatum",

        # transform_prior_education_vo_and_highest_degree
        "DEELNEMER_vooropleiding_hoogst_vo",
        "DEELNEMER_vooropleiding_vooropleiding",
        "DEELNEMER_vooropleiding_vooropleidings_categorie",
        "DEELNEMER_vooropleiding_soort",
        "DEELNEMER_vooropleiding_soort_naam",
        "DEELNEMER_vooropleiding_soort_code",
        "DEELNEMER_vooropleiding_begin_datum",
        "DEELNEMER_vooropleiding_einddatum",
        "DEELNEMER_vooropleiding_vo_begin_datum",
        "DEELNEMER_vooropleiding_hoogst_gediplomeerde",
        "DEELNEMER_vooropleiding_hoogste_diploma_begin_datum",

        # transform_students_to_student_year
        "peildatum",

        # convert_absence_types
        "MEDEWERKER_percentage_verzuim_omschrijving",

        # parse_enrollment_level
        "VERBINTENIS_niveau_omschrijving",

        # pivot_weeks_to_years
        "MEDEWERKER_verzuim_totaal",
        "MEDEWERKER_verzuim_percentage",
        "MEDEWERKER_verzuim_duur",

        # summarise_employee_absence_to_weeks
        "MEDEWERKER_verzuim_totaal",
        "days_in_week",

        # summarise_observations_to_weekly_attendance
        "VERBINTENIS_verzuim_week_nummer",
        "VERBINTENIS_waarneming_totale_duur",
        "GROEP_groepcode",
        "GROEP_organisatie_eenheid",
        "GROEP_groepnaam",
        "GROEP_type_omschrijving",
        "GROEP_groepdeelname_begindatum",
        "GROEP_groepdeelname_einddatum",

        # transform_attendance_weekly_to_enrollments
        "VERBINTENIS_groep_code",
        "VERBINTENIS_groep_organisatie_eenheid",
        "VERBINTENIS_groep_naam",
        "VERBINTENIS_groep_type_omschrijving",
        "VERBINTENIS_groepdeelname_begindatum",
        "VERBINTENIS_groepdeelname_einddatum",
        "VERBINTENIS_verzuim_week_nummer",

        # add_helper_variables
        "QuestionId",
        "SCHOOLJAAR_startjaar",
        "Organisatie",
        "Characteristic 1",
        "Characteristic 2",

        # calculate_application_to_start
        "VERBINTENIS_begindatum",
        "AANMELDING_begin_dagen_tot_start",
        "AANMELDING_afgerond_dagen_tot_start",

        # calculate_bpv_status_to_specific_dates
        "BPV_status_definitief_datum",
        "datum_definitief",
        "BPV_status_volledig_datum",
        "datum_volledig",

        # calculate_bpv_status_to_start
        "BPV_status_definitief_datum",
        "VERBINTENIS_begindatum",
        "BPV_status_volledig_datum",

        # calculate_exam_plan_to_specific_dates
        "OPLEIDING_examen_plan_verplicht_begindatum",
        "datum_exampenplan_af",
        "OPLEIDING_examen_plan_keuze_begindatum",
        "OPLEIDING_examen_plan_verplicht_einddatum",
        "OPLEIDING_examen_plan_keuze_einddatum",

        # calculate_exam_plan_to_start
        "COHORT_start_datum",
        "OPLEIDING_examen_plan_verplicht_begindatum",
        "OPLEIDING_examen_plan_keuze_begindatum",
        "OPLEIDING_examen_plan_verplicht_einddatum",
        "OPLEIDING_examen_plan_keuze_einddatum",
        "OPLEIDING_examen_plan_verplicht_begin_dagen_tot_start",

        # create_flex_boolean
        "VERBINTENIS_flex_omschrijving",

        # format_school_year_name
        "SCHOOLJAAR_naam_met_streep",

        # ingest_employee_answers_satisfaction_helper
        "SCHOOLJAAR_startjaar",

        # ingest_employees_job_type_helper
        "Functie",

        # ingest_students_satisfaction_helper
        "SCHOOLJAAR_startjaar",

        # ingest_teams_results_retention_start
        "TEAM_teljaar",
        "TEAM_team",
        "TEAM_noemer_sr_1_jaars",
        "TEAM_teller_sr_1_jaars",
        "TEAM_startersresultaat_1_jaars",

        # parse_result_pct
        "TEAM_startersresultaat_1_jaars_omschrijving",

        # pivot_answers_to_employees
        "SCHOOLJAAR_startjaar",
        "Organisatie",
        "Characteristic 1",
        "Characteristic 2",
        "appearance_number",
        "Question Text",
        "Score",

        # pivot_weeks_to_years
        "employee_absences_in_weeks",

        # summarise_employee_absence_to_years
        "verzuim_duur",
        "MEDEWERKER_verzuim_lang",
        "days_in_year",
        "MEDEWERKER_verzuim_middellang",
        "MEDEWERKER_verzuim_kort",
        "employee_absence_yearly",

        # summarise_observations_to_weekly_attendance
        "employee_absences_in_weeks",

        # summarise_satisfaction_to_groups
        "SCHOOLJAAR_startjaar",
        "Organisatie",
        "Characteristic 1",
        "Characteristic 2",
        "group_number",

        # pivot_cat_values_to_pct:
        "grouping_vars",
        "prop",

        # transform_to_cat_val_pct_columns:
        "TEAM_naam",

        # create_total_fte
        "MEDEWERKER_contract_fte",

        # create_active_on_1_okt
        "VERBINTENIS_einddatum",
        "VERBINTENIS_eerste_1_okt",

        # create_student_growth_vars
        "TEAM_studenten_aantal",
        "TEAM_studenten_aantal_vorig_jaar",

        # create_student_staff_ratio
        "TEAM_studenten_aantal",
        "MEDEWERKER_contract_fte_totaal",

        # filter_employees_to_education_teams_only
        "enrollments_combined_enriched_filtered",

        # filter_enrollments_for_team_aggregation
        "VERBINTENIS_actief_op_1_okt_peildatum",
        "VERBINTENIS_bekostigd",
        "VERBINTENIS_intensiteit",
        "VERBINTENIS_niveau",

        # set_user_friendly_names
        "term",

        # summarise_employees_num_vars_to_teams
        "MEDEWERKER_contract_fte_totaal",

        # summarise_enrollments_num_vars_to_teams
        "TEAM_naam_afk",
        "TEAM_school",
        "TEAM_school_afk",
        "BPV_is_definitief_voor_1_okt",
        "OPLEIDING_leerweg",

        # summarise_eployees_num_vars_to_teams
        "MEDEWERKER_contract_fte_totaal",

        # ingest_mapping_employees_satisfaction
        "mwo_characteristic_1",
        "mwo_characteristic_2",
        "MWO_characteristic_3",
        "kostenplaats_code",
        "kostenplaats_naam",

        # ingest_student_answers_satisfaction_2020
        "bij_welke_school_welk_team_volg_je_een_opleiding",

        # ingest_teams_results_degree
        "TEAM_noemer_dr_1_jaars",
        "TEAM_teller_dr_1_jaars",
        "TEAM_diplomaresultaat_1_jaars",

        # ingest_teams_results_year
        "TEAM_noemer_jr_1_jaars",
        "TEAM_teller_jr_1_jaars",
        "TEAM_jaarresultaat_1_jaars",

        # parse_result_pct
        "ends_with", # This is a dplyr function that should be imported

        # pivot_answers_to_employees
        "omschrijving",

        # pivot_answers_to_students
        "startjaar",
        "opleiding",
        "niveau",
        "leerjaar",
        "attribute",
        "value"

    )
)
