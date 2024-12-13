
utils::globalVariables(
    c(
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

        # ingest_job_contracts
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

        # transform_job_contracts_to_teacher_terms
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

        # ingest_job_contracts_basics_helper
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
        "VERBINTENIS_bpv_status_begin_datum"

    )
)
