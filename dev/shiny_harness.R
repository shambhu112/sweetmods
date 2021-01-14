## Basic robservable Shiny app

library(shiny)

library(reactable)
library(tidyr)
library(dplyr)
library(shinyjs)
library(cli)
library(stringr)
library(tibble)
library(esquisse)

source("zz.R")

params <- config::get(file = "dev/dev_config.yml")
print(params)
print(getwd())
md <- app_master$new(params )
md$preload_master_with_config()

ui <- fluidPage(
  esquisse_wrapper_ui("es_wrap")
  #file_upload_ui("data_upload_ui_1" )
  #smart_eda_ui("eda_mod_ui")
)

server <- function(input, output) {
   # file_upload_server("data_upload_ui_1" , master = md)
  esquisse_wrapper_server("es_wrap" , md)
}

shinyApp(ui = ui, server = server)
