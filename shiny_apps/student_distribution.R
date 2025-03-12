library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggalluvial)
library(config)
library(DT)
library(stringr)  # For string manipulation

ui <- fluidPage(
    titlePanel("Visualisatie diversiteit instroom per team"),
    fluidRow(
        column(width = 4,  # Left column for filters and table (1/3 of the width)
               wellPanel(  # Use wellPanel for visual separation of filters
                   selectInput("team",
                               "Select Team:",
                               choices = NULL,
                               selected = "Bouwtechniek"),
                   selectInput("year",
                               "Select Cohort Year:",
                               choices = NULL,
                               selected = 2023),
                   selectInput("color_by",
                               "Color By:",
                               choices = c("Niveau" = "VERBINTENIS_niveau",
                                           "Leerweg" = "OPLEIDING_leerweg",
                                           "Branche" = "OPLEIDING_bc_label_kort"),
                               selected = "VERBINTENIS_niveau"),
                   sliderInput("min_n",
                               "Minimum Number of Students:",
                               min = 1,
                               max = 50,  # Adjust max based on your data's maximum n, or make it dynamic
                               value = 1,
                               step = 1)
               ),
               wellPanel(  # Use wellPanel for visual separation of table
                   dataTableOutput("summary_table")
               )
        ),
        column(width = 8,  # Right column for visualizations (2/3 of the width)
               plotOutput("flowPlot", height = "600px"),
               htmlOutput("summary_text")  # Using htmlOutput for HTML rendering
        )
    )
)

server <- function(input, output, session) {

    counts_students <- readRDS("counts_students.rds")

    observe({
        updateSelectInput(session, "team",
                          choices = unique(counts_students$TEAM_naam_kort),
                          selected = "Bouwtechniek")
        updateSelectInput(session, "year",
                          choices = unique(counts_students$COHORT_startjaar),
                          selected = 2023)
    })

    counts_students_one_year <- reactive({
        counts_students |>
            filter(COHORT_startjaar == as.numeric(input$year))
    })

    counts_students_one_year_n <- reactive({
        counts_students_one_year() |>
            filter(n >= input$min_n)
    })

    filtered_data <- reactive({
        counts_students_one_year_n() |>
            filter(TEAM_naam_kort == input$team) |>
            group_by(OPLEIDING_naam_kort) |>
            mutate(opleiding_niveau = mean(as.numeric(VERBINTENIS_niveau))) |>
            ungroup() |>
            group_by(OPLEIDING_bc_label_kort) |>
            mutate(bc_niveau = mean(as.numeric(VERBINTENIS_niveau))) |>
            ungroup() |>
            mutate(
                OPLEIDING_naam_kort = fct_reorder2(OPLEIDING_naam_kort,
                                                   -opleiding_niveau,
                                                   OPLEIDING_naam_kort),
                OPLEIDING_bc_label_kort = fct_reorder2(OPLEIDING_bc_label_kort,
                                                       -bc_niveau,
                                                       OPLEIDING_bc_label_kort)
            )
    })

    # Get unfiltered data for the selected team and year for summary text
    unfiltered_team_data <- reactive({
        counts_students_one_year() |>
            filter(TEAM_naam_kort == input$team)
    })

    output$flowPlot <- renderPlot({
        if (nrow(filtered_data()) == 0) {
            plot.new()
            text(0.5, 0.5, "No data available for the selected filters", cex = 1.5)
        } else {
            filtered_data() |>
                ggplot(aes(axis1 = TEAM_naam_kort,
                           axis2 = VERBINTENIS_niveau,
                           axis3 = OPLEIDING_naam_kort,
                           axis4 = OPLEIDING_bc_label_kort,
                           axis5 = OPLEIDING_leerweg,
                           y = n)) +
                geom_alluvium(aes(fill = .data[[input$color_by]]),
                              alpha = 0.7) +
                geom_stratum() +
                geom_text(stat = "stratum",
                          aes(label = paste0(after_stat(stratum),
                                             " (", after_stat(count), ")"))) +
                scale_x_discrete(limits = c("Team", "Niveau", "Opleiding", "Branche", "Leerweg"),
                                 expand = c(0.15, 0.15)) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
                      legend.position = "bottom") +
                labs(y = "Aantal studenten",
                     fill = input$color_by)
        }
    })

    output$summary_text <- renderText({
        unfiltered_data <- unfiltered_team_data()
        filtered_data_sum <- filtered_data()

        if (nrow(unfiltered_data) == 0) {
            HTML("No data available for the selected filters.")
        } else {
            total_students_unfiltered <- sum(unfiltered_data$n)  # Total before min_n filter
            total_combinations_unfiltered <- nrow(unfiltered_data)
            avg_students_unfiltered <- if (total_combinations_unfiltered > 0) round(total_students_unfiltered / total_combinations_unfiltered, 1) else 0

            total_students_filtered <- if (nrow(filtered_data_sum) > 0) sum(filtered_data_sum$n) else 0  # Total after min_n filter
            total_combinations_filtered <- nrow(filtered_data_sum)
            avg_students_filtered <- if (total_combinations_filtered > 0) round(total_students_filtered / total_combinations_filtered, 1) else 0

            # Use HTML for bolding numbers and line breaks, distinguishing unfiltered vs. filtered totals
            HTML(paste0("Totaal aantal studenten voor team ", input$team, " in ", input$year, ": <b>", total_students_unfiltered, "</b> (ongefilterd).<br>",
                        "Totaal aantal studenten na filter (≥ ", input$min_n, "): <b>", total_students_filtered, "</b>.<br>",
                        "Totaal aantal combinaties ongefilterd: <b>", total_combinations_unfiltered, "</b>; na filter: <b>", total_combinations_filtered, "</b>.<br>",
                        "Gemiddeld ongefilterd: <b>", avg_students_unfiltered, "</b> studenten per combinatie; na filter: <b>", avg_students_filtered, "</b>."))
        }
    })

    # Simplified table without filters, keeping selection, using abbreviated values
    output$summary_table <- renderDataTable({
        table_data <- filtered_data() |>
            group_by(TEAM_naam_kort,
                     VERBINTENIS_niveau,
                     OPLEIDING_naam_kort,
                     OPLEIDING_bc_label_kort,
                     OPLEIDING_leerweg) |>
            summarise(Instroom = sum(n), .groups = "drop") |>
            select(-TEAM_naam_kort) |>
            rename(
                #Team = TEAM_naam_kort,
                Niveau = VERBINTENIS_niveau,
                Opleiding = OPLEIDING_naam_kort,
                Branche = OPLEIDING_bc_label_kort,
                Leerweg = OPLEIDING_leerweg
            )

        if (nrow(table_data) == 0) {
            data.frame(Message = "No data available for the selected filters") %>%
                datatable(
                    rownames = FALSE,
                    options = list(
                        pageLength = 1,
                        scrollX = TRUE,
                        dom = 't'
                    ),
                    width = "100%",
                    style = "bootstrap",
                    class = "compact"
                )
        } else {
            datatable(
                table_data,
                rownames = FALSE,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = 'tip',  # Removed 'f' for filters, keeping table, info, pagination
                    columnDefs = list(
                        list(width = '50px', targets = "_all")  # Uniform width for all columns
                    ),
                    order = list(list(4, 'desc'))  # Sort by "Instroom" (column 4, 0-based index) in descending order
                ),
                selection = 'multiple',  # Keep multiple row selection
                width = "100%",
                style = "bootstrap",
                class = "compact"
            )
        }
    })
}

shinyApp(ui = ui, server = server)



