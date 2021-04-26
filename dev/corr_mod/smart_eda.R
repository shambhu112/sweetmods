#' Mod for EDA analysis
#'
#' @description A File Data Upload Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @import shiny
#' @import SmartEDA
smart_eda_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Datasets for Analysis"),
      fluidRow(
        column(8,
               shinyjs::useShinyjs(),
               wellPanel(
                 x <- uiOutput(ns('eda_choices')),
                 actionButton('submit', label = "Submit"),
               )

        ),
        column(4, offset = 4,
               textOutput('text2')
        )
      ),
      fluidRow(
        column(12 ,
               tabsetPanel(type = "tabs",
                           tabPanel("Summary", reactable::reactableOutput(ns("eda_summary"))),
                           tabPanel("Structure", reactable::reactableOutput(ns("eda_structure")))
               )
        )
      )
    )
  )
}



#' EDA Server components
#' @import shiny
#' @param id standard shiny server signature
#' @param master the \code{app_master} reference for this module
#' @export
smart_eda_server <- function(id , master){
  moduleServer(id, function(input, output, session ){
    ns <- session$ns

    output$eda_choices <- renderUI({
      d_choices <-master$dataset_names()
      cli::cli_alert_info("setting  choices {d_choices} ")
      # The options are dynamically generated on the server
      radioButtons(ns('select_eda_data'), 'Select Dataset to visualize', d_choices, selected = character(0) ,
                   inline = TRUE)
    })

    observeEvent(input$select_eda_data, {
      cli::cli_alert_info("Radio selected ")
      nm <- input$select_eda_data
      df <- master$data_by_name(nm)
      #   data_r <- reactiveValues(data = df, name = nm)

      summary_df <- SmartEDA::ExpData(data=df,type = 1)
      structure_df <-SmartEDA::ExpData(data=df,type = 2)
      #print(d)
      output$eda_summary <- reactable::renderReactable({
        reactable::reactable(summary_df)
      })

      output$eda_structure <- reactable::renderReactable({
        reactable::reactable(structure_df)
      })

    })


  })
}

eda_summary <- function(df){
  reactable::reactable(df , )
}
