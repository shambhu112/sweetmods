## This Shiny App created with Shinyspring - http://www.shinyspring.dev
## template bs4Dash

library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(stringr)
library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(esquisse)

#library(sweetmods)



##detach("package:semantic.dashboard", unload = TRUE)
##detach("package:shiny.semantic", unload = TRUE)
source("../R/app_master.R")
source("../R/esquisse_wrapper.R")
source("../R/sweetutil.R")
source("../R/corrmorant_mod.R")

thematic_shiny()

params <- config::get(file = "dev_config.yml")

controller <- app_master$new(params)

controller$preload_master_with_config()



demo_ui <- function(id, control) {
  ns <- NS(id)
  h4(paste0("Hello " , id))
}



demo_server <- function(id, control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}


create_tab_module <- function(tab_name , module_name , module_function , control = "controller"){
  tabItem(
    tabName = tab_name,
    eval(parse(text = paste0(module_function , "(id = '" , module_name  ,"' , control = " , control, " )"  )))
  )
}


## Define UI for application that draws a histogram
ui <- bs4Dash::dashboardPage(
  ##     preloader = list(
  ##         waiter = list(html = tagList(spin_1(), "Loading ..."), color = "##343a40"),
  ##         duration = 0
  ##     ),

  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "ShinySpring", ## @@app_title
      color = "primary",
      href = "www.shinyspring.dev", ## @@header_href
      image = "https://storage.googleapis.com/shiny-pics/spring_logo.png", ##@@header_image
      opacity = 0.8
    ),
    fixed = TRUE, ## @@header_fixed
    rightUi = tagList(
      dropdownMenu(
        badgeStatus = "info",
        type = "notifications",
        messageItem(
          inputId = "triggerAction1",
          message = "FDIC issues list of Bank Examined for CRA compliance",
          from = "FDIC",
          image = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Seal_of_the_United_States_Federal_Deposit_Insurance_Corporation.svg/1200px-Seal_of_the_United_States_Federal_Deposit_Insurance_Corporation.svg.png",
          time = "today",
          color = "lime"
        )
      )
      ##    ,
      ##    userOutput("user")
    ),
    leftUi = tagList(
      ## Close dropdownMenu
      tags$li(class = "dropdown",
              tags$h3("Sweet Mods Test") ## $$app_title_h3
      )
    ) ## close left UI
  ),
  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    id = "sidebar",

    ##    sidebarUserPanel(
    ##      image = "https://image.flaticon.com/icons/svg/1149/1149168.svg", ## @@welcome_image
    ##      name = "CORA" ## @@welcome_message
    ##    ),
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Data Exploration"),
      menuItem(
        "Esquisse",
        tabName = "esquisse_tab",
        icon = icon("university")
      ),
      menuItem(
        "Corelations",
        tabName = "corrmant_tab",
        icon = icon("piggy-bank")
      )
    )

  ), ## Close of sidebar
  body = dashboardBody(
    tabItems(
      create_tab_module(tab_name = "esquisse_tab" ,module_name = "esquisse_mod_1" , module_function = "esquisse_wrapper_ui" ) ,
      create_tab_module(tab_name = "corrmant_tab" ,module_name = "corrmant_tab_1" , module_function = "corrmorant_mod_ui" )
    )
  ) ## close of body
)

## Define server logic required to draw a histogram
server <- function(input, output , session) {
  esquisse_wrapper_server(id = "esquisse_mod_1" , control = controller)
  corrmorant_mod_server(id = "corrmant_tab_1" , control = controller)

}

## Run the application
shinyApp(ui = ui, server = server)
