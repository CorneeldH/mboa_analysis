utils::globalVariables(c(
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

    # ingest_programmes_basic
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

    # ingest_applications
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
    "VERBINTENIS_ID"

))
