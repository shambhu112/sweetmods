


#' esquisse_wrapper UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for \code{shiny}
#' @param control
#' @importFrom esquisse esquisse_ui esquisseContainer
#' @importFrom shiny NS tagList
#' @export
esquisse_wrapper_ui <- function(id , control){
  ns <- NS(id)
  esquisse::esquisseContainer(width = "100%", height = "700px", fixed = FALSE)

    fluidRow(
      shinyjs::useShinyjs(),
      selectizeInput(ns("dataset_selection"), "Select Dataset", choices =control$dataset_names()  ,
                     multiple = FALSE, width = 400 , options = NULL ),

      box(
        title = "Visualize data with Esquisse",
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        maximizable = TRUE,
        id = "esquuisse_tabs",
        esquisse::esquisse_ui(
            id = ns("esquisse"),
            header = FALSE # dont display gadget title
           )
      )
    )

}



#' esquisse_wrapper Server Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for \code{shiny}
#' @param control
#' @importFrom esquisse esquisse_server
#' @importFrom shiny NS tagList
#' @export

esquisse_wrapper_server <- function(id , control){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    names <- control$dataset_names()
    #TODO : Fix situation when no datasets available

    data_r <- reactiveValues(data = control$data_by_index(1), name = names(1))

    observe({
      data_r$data <- control$dataset_by_name(input$dataset_selection)
      data_r$name <- input$dataset_selection
    })

    results <- esquisse::esquisse_server(
      id = "esquisse",
      data_rv = data_r
    )

#    output$code <- renderPrint({
#      results$code_plot
#    })

#    output$filters <- renderPrint({
#      results$code_filters
#    })

#    output$data <- renderPrint({
#      str(results$data)
#    })



  })
}
