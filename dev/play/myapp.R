## This Shiny App created with Shinyspring - http://www.shinyspring.dev
## template : bs4Dash
## app_type : minimal

library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(stringr)
library(dplyr)
library(sweetmods)

thematic_shiny()

params <- config::get(file = "config.yml") ## config.yml
controller <- app_master$new(params)
#controller$preload_master_with_config()

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


## Define UI
ui <- bs4Dash::dashboardPage(

  dark = FALSE , ## FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Sample App",
      color = "primary", ## primary
      href = "https://www.shinyspring.dev", ## @ https://www.shinyspring.dev
      image = "https://storage.googleapis.com/shiny-pics/spring_logo.png", ## https://storage.googleapis.com/shiny-pics/spring_logo.png
      opacity = 0.8
    ),
    fixed = TRUE , ## TRUE
    leftUi = tagList(
      ## Title Text here
      tags$li(class = "dropdown",
              tags$h3("Bs4 Dash Minimalistic") ## $$app_title_h3
      )
    ) ## close left UI
  ),
  sidebar = dashboardSidebar(
    fixed = TRUE, ## @@side_bar_fixed
    skin = "light",
    status = "primary",
    id = "sidebar",

    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Sample App"),

# Whisker:  Menus
        menuItem(
          "Introduction" ,
          tabName = "bank_mod_tab",
          icon = icon("university")
        ),

        menuItem(
          "Core Analysis" ,
          tabName = "branch_mod_tab",
          icon = icon("indent")
        ),

        menuItem(
          "Data Exploration" ,
          tabName = "esquiee_mod_tab",
          icon = icon("chart-bar")
        ),

        menuItem(
          "Credits" ,
          tabName = "credits_mod_tab",
          icon = icon("heart")
        )

          )

    ),  ## Close of sidebar
  body = dashboardBody(
    tabItems(


        create_tab_module(tab_name = "bank_mod_tab" ,
                          module_name = "bank_mod" ,
                          module_function = "demo_ui" ) ,
        create_tab_module(tab_name = "branch_mod_tab" ,
                          module_name = "branch_mod" ,
                          module_function = "demo_ui" ) ,
        create_tab_module(tab_name = "esquiee_mod_tab" ,
                          module_name = "esquiee_mod" ,
                          module_function = "demo_ui" ) ,
        create_tab_module(tab_name = "credits_mod_tab" ,
                          module_name = "credits_mod" ,
                          module_function = "demo_ui" ) 
                            )

    ) # Close of tab items
)



## Define server logic required to draw a histogram
server <- function(input, output , session) {

    demo_server(id = "bank_mod" , control = controller)
    demo_server(id = "branch_mod" , control = controller)
    demo_server(id = "esquiee_mod" , control = controller)
    demo_server(id = "credits_mod" , control = controller)

  }

    ## Run the application
    shinyApp(ui = ui, server = server)

