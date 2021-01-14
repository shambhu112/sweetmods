
utils::globalVariables(c("esquisserServer"))

#' esquisse_wrapper UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for \code{shiny}
#'
#' @importFrom shiny NS tagList
#' @export
esquisse_wrapper_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(

      titlePanel("Visualize Loaded Datasets with Esquisse"),
      fluidRow(
        column(8,
               shinyjs::useShinyjs(),
               wellPanel(
                 x <- uiOutput(ns('radios')),
                 #  actionButton('submit', label = "Submit"),

               )

        ),
        column(4, offset = 4,
               textOutput('text')

        )
      )
    ),
    fluidRow(column(12 ,
                    esquisse::esquisserUI(
                      id = ns("esquisse"),
                      header = FALSE, # dont display gadget title
                      choose_data = FALSE, # dont display button to change data,
                      container = esquisse::esquisseContainer(height = "700px")
                    )
    ))

  )
}

#' esquisse wrapper Server Functions
#'
#' Calls esquisse module based on the selection made in radios input
#' @param id standard shiny param
#' @param master the reference the app_master R6Class
#' @export
esquisse_wrapper_server <- function(id , master){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$radios <- renderUI({

      d_choices <-master$dataset_names()
      # The options are dynamically generated on the server
      radioButtons(ns('radio_select'), 'Select Dataset to visualize', d_choices, selected = character(0) ,
                   inline = TRUE)
    })


    observeEvent(input$radio_select, {
      cli::cli_alert_info("Radio selected ")
      nm <- input$radio_select
      df <- master$data_by_name(nm)
      data_r <- reactiveValues(data = df, name = nm)
      cli::cli_alert_info("Befoer Call Module {nm} ")
      # browser()
      callModule(module = esquisserServer, id = "esquisse", data = data_r)
      #  browser()
    })

  })
}

