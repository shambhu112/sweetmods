#' datasets connection UI Function
#'
#' @description Connect various datasets to sweetmods for use
#'
#' @param id Internal parameters for {shiny}.
#'
#' @import shiny
#' @import shiny
#' @export
datasets_connection_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
}

datasets_connection_server <- function(id , master){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
