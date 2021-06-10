
#' onLoad for rmd_mod
#'
#' @description A shiny spring Module.
#'
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
rmd_mod_onLoad <- function(control , params){
 #  library(markdown)
#   library(knitr)
 }

#' ui_function for rmd_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @import shiny
#' @export
rmd_mod_ui <- function(id , control , params ){
  ns <- NS(id)
  show_rmd(params$rmd_file , params$box_title )
}



#' server_function for rmd_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @import shiny
#' @export
rmd_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns

    rmd_file <- params$rmd_file

    #TODO : knit is not implemented here
#    knit <- FALSE
#    if(!is.null(params$knit)){
#      knit <- as.logical(params$knit)
#    }
#    if(knit){
#      output$markdown <- renderUI({
#        HTML(markdown::markdownToHTML(knitr::knit(rmd_file, quiet = TRUE)))
#      })

#    }
  })
}


show_rmd <- function(rmd_file , box_title  ){
    bs4Dash::box(
      title = box_title,
      width = 12,
      status = "primary",
      maximizable = TRUE,
      solidHeader = TRUE ,
      shiny::includeMarkdown(rmd_file)
    )
}

#' Created rmd mod dependencies and displays the properties needed for rmd mod
#' @param registry the registry
#' @param rmd_file optional
#' @param template optional
#' @export
use_rmd_mod <- function(registry , rmd_file = "intro.Rmd" , template = "templates/rmd_mod_t1.mst"){
    master_params <- registry$master_params
    rmd_template <- readr::read_file(system.file( template , package = "sweetmods"))
    rmd_script <- whisker::whisker.render(rmd_template , master_params)
    writeLines(rmd_script, con = file.path(rmd_file))
    cli::cli_alert_success("Created RMD file : {rmd_file} ")
    registry$mod_definition("rmd_mod")
}
