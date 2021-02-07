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
library(bs4Dash)
#library(shinydashboard)

#source("zz.R")
#library(sweetmods)

source("R/app_master.R")
source("R/file_upload_mod.R")
source("R/sweetutil.R")
source("R/esquisse_wrapper.R")


params <- config::get(file = "dev/dev_config.yml")
print(params)
print(getwd())
md <- app_master$new(params )
md$preload_master_with_config()


menu_side_tabs <- function(){
  shinydashboardPlus::sid(
    shinydashboard::sidebarMenu(
      id = "tabs-menu",
      shinydashboard::menuItem("Tutorial", tabName = "tutorial", icon = icon("leanpub")),
      shinydashboard::menuItem("Data", tabName = "data", icon = icon("database") ,
                               shinydashboard::menuSubItem("Upload" , tabName = "upload_data" , icon = icon("upload")),
                               shinydashboard::menuSubItem("Visualize" , tabName = "vis_data" , icon = icon("chart-bar")),
                               shinydashboard::menuSubItem("EDA" , tabName = "eda_data" , icon = icon("project-diagram"))
      ),
      shinydashboard::menuItem("Macro Analysis", tabName = "macro_analysis", icon = icon("archway")),
      shinydashboard::menuItem("Micro Analysis", tabName = "micro_analysis", icon = icon("atom")),
      shinydashboard::menuItem("Credits", tabName = "credits_tab", icon = icon("heart"))
    )
  )
}

body_tab_items <- function(){
  tabItems(
    tabItem("tutorial" ,"tutorial" ) ,
    tabItem("upload_data" , "data_upload_ui_1" ) ,
    tabItem("credits_tab", "Credits"),
    tabItem("macro_analysis", "Macro Tab Content"),
    tabItem("micro_analysis", "Micro Tab Content"),
    tabItem("vis_data",  "esquisse_wrapper_ui_1"),
    tabItem("data", "Data Tab Content")
  )
}
header_create <- function(){
  shinydashboardPlus::dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "Shiny Spring"),
      img(src = "https://www.flaticon.com/svg/static/icons/svg/892/892926.svg" , width = 30)
    )
  )
}


ui <- fluidPage(
  shinydashboardPlus::dashboardPagePlus(
   # options = list(sidebarExpandOnHover = FALSE),
    header = header_create(),
    sidebar = menu_side_tabs(),
    body = shinydashboard::dashboardBody(
      # All tabs
      body_tab_items()
    ),
  #  controlbar = shinydashboardPlus::dashboardControlbar(),
    footer = shinydashboardPlus::dashboardFooter(right = "Shiny Spring" , left = "Build Fast Shiny Apps"),
    title = "Build Fast Shiny Apps "
)
)

server <- function(input, output) {
   file_upload_server("data_upload_ui_1" , master = md)
  #esquisse_wrapper_server("es_wrap" , md)
}

shinyApp(ui = ui, server = server)


