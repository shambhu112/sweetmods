
#' onLoad for Dummy Mod
#'
#' @description A shiny Module.
#'
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export

dummy_mod_onLoad <- function(control , params){

}


#'  UI Function for dummy mod
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for \code{shiny}
#' @param control the app_master controller object
#' @export

dummy_mod_ui <- function(id , control , params){
  ns <- NS(id)
  fluidRow(
    h3(params$message) ,

   ##Your UI Code here

    )
}

#'  Server Function for dummy mod
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for \code{shiny}
#' @param control the app_master controller object
#' @export

dummy_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns
  ## Your server code here

    })
}

