library(corrmorant)
library(shinycssloaders)


corrmorant_mod_ui <- function(id , control) {
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    selectizeInput(ns("dataset_selection"), "Select Dataset", choices =control$dataset_names()  ,
                   multiple = FALSE, width = 400 , options = NULL ),

    box(
      title = "Explore Corrlations with Corrmorant",
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      collapsible = FALSE,
      maximizable = TRUE,
      plotOutput(ns("corrmorant") , width = "100%", height = "8

                 {{00px")  %>% withSpinner(color="#0dc5c1")
    )
  )

}

corrmorant_mod_server <- function(id , control){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    names <- control$dataset_names()
    #TODO : Fix situation when no datasets available
    data_r <- reactiveValues(data = control$data_by_index(1), name = names(1))
    observe({
      data_r$data <- control$dataset_by_name(input$dataset_selection)
       data_r$name <- input$dataset_selection
    })

    output$corrmorant <- renderPlot({
      data_r$data %>%
      ggcorrm(rescale = "by_sd") +
        utri_heatmap(alpha = 0.5) +
        lotri_heatcircle(alpha = 0.5, col = 1) +
        utri_corrtext() +
        dia_names(y_pos = 0.15, size = 3) +
        dia_density(lower = 0.3, fill = "lightgrey", color = 1) +
        scale_fill_corr()
    })
  })
  }
