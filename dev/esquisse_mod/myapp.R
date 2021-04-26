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

params <- config::get(file = "config.yml") ## @@sweetmod_config
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


## Define UI
ui <- bs4Dash::dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
  dark = FALSE , ## FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "ShinySpring",
      color = "primary", ## @@title_color
      href = "https://www.shinyspring.dev", ## @@ header_href
      image = "https://storage.googleapis.com/shiny-pics/spring_logo.png", ## @@header_image
      opacity = 0.8
    ),
    fixed = TRUE , ## @@header_fixed
    leftUi = tagList(
      ## Title Text here
      tags$li(class = "dropdown",
              tags$h3("Esquiee Mod Sample") ## @@app_title_h3
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
      sidebarHeader("Data Exploration"),

# Whisker:  Menus - niraj
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
   create_tab_module(tab_name = "esquiee_mod_tab" , module_name = "esquiee_mod" , module_function = "esquisse_wrapper_ui" ) ,
   create_tab_module(tab_name = "credits_mod_tab" , module_name = "credits_mod" , module_function = "demo_ui" )
    )
  ) # Close of tab items
)

## Define server logic required to draw a histogram
server <- function(input, output , session) {
    esquisse_wrapper_server(id = "esquiee_mod" , control = controller)
    demo_server(id = "credits_mod" , control = controller)
  }

    ## Run the application
    shinyApp(ui = ui, server = server)

