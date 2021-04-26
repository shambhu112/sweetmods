
#' onLoad for rmd_mod
#'
#' @description A shiny spring Module.
#'
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
rmd_mod_onLoad <- function(control , params){
   library(markdown)
  library(knitr)
 }

#' ui_function for rmd_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
rmd_mod_ui <- function(id , control , params ){
  ns <- NS(id)
  rmd_file <- params$rmd_file
  knit <- as.logical(params$knit)
  if(knit){
    bs4Dash::box(
      title = "Users Guide",
      width = 12,
      status = "primary",
      maximizable = TRUE,
      solidHeader = TRUE ,
      uiOutput(ns('markdown'))
    )


 #   fluidPage(uiOutput(ns('markdown')))
  }
  else{

    bs4Dash::box(
      title = "Users Guide",
      width = 12,
      status = "primary",
      maximizable = TRUE,
      solidHeader = TRUE ,
      shiny::includeMarkdown(rmd_file)
    )

#    fluidPage(
#      shiny::includeMarkdown(rmd_file)
#    )
  }
}


#' server_function for rmd_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
rmd_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns

    rmd_file <- params$rmd_file
    knit <- as.logical(params$knit)
    if(knit){
      output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knitr::knit(rmd_file, quiet = TRUE)))
      })
    }
  })
}

