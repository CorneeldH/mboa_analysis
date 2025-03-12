ui <- fluidPage(
    titlePanel("Student Flow Visualization"),
    sidebarLayout(
        sidebarPanel(
            selectInput("team",
                        "Select Team:",
                        choices = unique(counts_students$TEAM_naam),
                        selected = "Bouwtechniek")
        ),
        mainPanel(
            plotOutput("flowPlot", height = "600px"),  # Back to regular plot
            # Add hover info in a separate output
            verbatimTextOutput("hover_info")
        )
    )
)

server <- function(input, output) {

    # Create a reactive for hover
    hover_data <- reactiveVal(NULL)

    output$flowPlot <- renderPlot({
        counts_students |>
            filter(COHORT_startjaar == 2023) |>
            filter(TEAM_naam == input$team) |>
            group_by(OPLEIDING_naam) |>
            mutate(opleiding_niveau = mean(as.numeric(VERBINTENIS_niveau))) |>
            ungroup() |>
            group_by(OPLEIDING_bc_label) |>
            mutate(bc_niveau = mean(as.numeric(VERBINTENIS_niveau))) |>
            ungroup() |>
            mutate(
                OPLEIDING_naam = fct_reorder2(OPLEIDING_naam,
                                              -opleiding_niveau,
                                              OPLEIDING_naam),
                OPLEIDING_bc_label = fct_reorder2(OPLEIDING_bc_label,
                                                  -bc_niveau,
                                                  OPLEIDING_bc_label)
            ) |>
            ggplot(aes(axis1 = TEAM_naam,
                       axis2 = VERBINTENIS_niveau,
                       axis3 = OPLEIDING_naam,
                       axis4 = OPLEIDING_bc_label,
                       axis5 = OPLEIDING_leerweg,
                       y = n)) +
            geom_alluvium(aes(fill = VERBINTENIS_niveau,
                              text = paste("Aantal studenten:", n)),
                          alpha = 0.7) +
            geom_stratum(aes(text = paste("Aantal studenten:", after_stat(count)))) +
            # Modified the geom_text to include the count
            geom_text(stat = "stratum",
                      aes(label = paste0(after_stat(stratum),
                                         " (", after_stat(count), ")"))) +
            scale_x_discrete(limits = c("Team", "Niveau", "Opleiding", "Branche", "Leerweg"),
                             expand = c(0.15, 0.15)) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
                  legend.position = "none") +
            labs(y = "Aantal studenten")
    })

    # Add hover functionality
    observeEvent(input$flowPlot_hover, {
        hover_data(paste0("Aantal studenten: ",
                          round(input$flowPlot_hover$y)))
    })

    output$hover_info <- renderText({
        if(is.null(hover_data())) return()
        hover_data()
    })
}

shinyApp(ui = ui, server = server)
