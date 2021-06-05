
#' onLoad for selector_mod
#'
#' @description A shiny spring Module.
#'
#' @param control the controller  \code{app_master}
#' @param params for the mod
selector_mod_onLoad <- function(control , params){

}

#' ui_function for selector_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
selector_mod_ui <- function(id , control , params ){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    bs4Dash::box(
      title = "Select Dataset and Columns",
      width = 12,
      status = "primary",
      side = "right",
      solidHeader = FALSE,
      collapsible = TRUE,
      maximizable = TRUE,
      id = "selection_box",
      fluidRow(
        column(4 ,
               bs4Dash::blockQuote(HTML("<strong> Select Columns </strong> that you want to include in your corelation analysis.Columns with Numerics are used for Corelation analysis.
               Hence showing only numerics. selection you can then created the <strong> Network Graph</strong> or the <strong> Corelation Matrix </strong> in tabs below") ,
                                   color = "teal")
        ),
        column(8 ,
               column_selection_mod_ui(id = ns("column_selector") , dataset_names =  control$dataset_names() )
        )
      )
    )
    )
}


#' server_function for selector_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
selector_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns

    names <- control$dataset_names()
    dq_v <- lapply(names , function(x){
      dq <- control$get_dq_detail(x ,1000 )
      dq
    })


  ## Your server code here
    selections <- column_selection_mod_server("column_selector" , control , dq_v , names )

    })
}

