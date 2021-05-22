
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
  files <- params$rmd_files
  rmd_files <- shinyspring::parse_preloads_in_config(params$rmd_files)
  x <-length(rmd_files)
#  knit <- as.logical(params$knit) #TODO : implement this later

  if(x == 0){
    cli::cli_alert_danger("rmd_files property needs to be set on config file : {rmd_files}")
    return
  }else if(x ==1){
    show_rmd(rmd_files[1] , params$box_title )
  }else {
    tab_titles <- shinyspring::parse_preloads_in_config(params$tab_titles)
    show_rmd_tabs(rmd_files , tab_titles , params$box_title)
  }
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
    knit <- as.logical(params$knit)
    if(knit){
      output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knitr::knit(rmd_file, quiet = TRUE)))
      })
    }
  })
}

# TODO : this does not work at this time
#' server_function for rmd_mod
#'
#' @description A shiny spring Module UI function
show_rmd_tabs <- function(rmd_files , tab_titles ,box_title){

bs4Dash::tabBox(
  title = box_title,
  side = "right",
  elevation = 2,
  id = "tabcard1",
  width = 12,
  collapsible = FALSE,
  closable = FALSE,
  type = "tabs",
  status = "primary",
  solidHeader = TRUE,
  sapply(1:length(rmd_files), function(x){
    tabPanel(
      title = tab_titles[x] ,
      shiny::includeMarkdown(rmd_files[x])
    )
  })
)
}


show_rmd <- function(rmd_file , box_title , knit = FALSE ){
#  if(knit){
#    bs4Dash::box(
#      title = box_title,
#      width = 12,
#      status = "primary",
#      maximizable = TRUE,
#      solidHeader = TRUE ,
#      uiOutput(ns('markdown'))
#    )
#  }
#  else{
    bs4Dash::box(
      title = box_title,
      width = 12,
      status = "primary",
      maximizable = TRUE,
      solidHeader = TRUE ,
      shiny::includeMarkdown(rmd_file)
    )
#  }
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
