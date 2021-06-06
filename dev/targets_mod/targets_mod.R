
#' onLoad for targets_mod
#'
#' @description A shiny spring Module.
#'
#' @param control the controller  \code{app_master}
#' @param params for the mod
targets_mod_onLoad <- function(control , params){

}

#' ui_function for targets_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
targets_mod_ui <- function(id , control , params ){
  ns <- NS(id)
  fluidRow(
     ##Your UI Code here
     h3(" Hello targets_mod this is the UI function")
    )
}


#' server_function for targets_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
targets_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns
  ## Your server code here

    })
}

