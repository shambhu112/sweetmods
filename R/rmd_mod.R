
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


#'
#' @description  Design time function to create skeleton files  for dependencies.Will create template RMD files based on params
#' @param master_params the params
#' @param mod_name the mod name refeered in config
#' @export
rmd_mod_dependencies <- function(master_params , mod_name = NULL){
    reg <- shinyspring::mod_registry$new(master_params)

    filename <- "intro.Rmd"
    params <- NULL
    if(!is.null(mod_name)){
      params <- reg$params_for_mod(mod_name)
      if(!identical(params$mod_name , "rmd_mod")){
        cli::cli_alert_warning(" the {mod_name} is currently not rmd_mod. Please change this to rmd_mod in config file")
      }else{
          filename <- params$rmd_file
      }
    }

    #TODO : not well thought approach below
    if(is.null(params)){
      params <- master_params
    }

   rmd_template <- readr::read_file(system.file("inst/templates/rmd_mod_t1.mst" , package = "sweetmods"))
   rmd_script <- whisker::whisker.render(rmd_template , params)
   writeLines(rmd_script, con = file.path(filename))
   cli::cli_alert_success("Created RMD script : {filename} ")

}
