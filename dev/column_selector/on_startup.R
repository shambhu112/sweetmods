## This file created by Shinyspring - http://www.shinyspring.dev
## This file is sourced from the shinyapp file (eg: app.R) at startup time.
## The developers  needs to update this file for his/her application needs

## Typically you should load all dependent libraries and implement prep_on_start

library(shiny)
library(thematic)
library(waiter)
library(stringr)
library(dplyr)
library(sweetmods)

library(bs4Dash)

source("selector_mod.R")

#TODO for developer ... add more dependencies that neede to be loaded on startup

prep_on_start <- function(control , registry){

}

# KEEP this Function and DO NOT Change . This function is called from app.R.
on_load_for_mods <- function(control , registry){
  mod_names <- registry$mods_names()
  onl <- sapply(mod_names, function(x){
    p <- registry$params_for_mod(x)
    package_prefix <- ifelse("package" %in% names(p) , paste0(p$package , "::") , "" )
    if("onload_function" %in% names(p)){
      onload_f <- paste0(package_prefix , p$onload_function , "(control ,params = p)")
      eval(parse(text= onload_f))
      cli::cli_alert_info("Executed onload for {x} : {onload_f}")
    }
  })
  onl <- NULL
}


# Function to create the tab item and call the ui_function in module
# KEEP this Function and DO NOT Change . This function is called from app.R.
create_tab_module <- function(tab_module , registry , controller){
  p <- registry$params_for_mod(tab_module)
  ui_function <-  p$ui_function
  package_prefix <- ifelse("package" %in% names(p) , paste0(p$package , "::") , "" )
  tabItem(
    tabName = tab_module,
    eval(parse(text = paste0(package_prefix , ui_function , "(id = '" , tab_module  ,"' , control = controller ,params = p)"  )))
  )
}


