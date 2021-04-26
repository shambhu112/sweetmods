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
mod_names <- controller$mods_names()

# call on_load function on all modules
onl <- sapply(mod_names, function(x){
  p <- controller$params_for_mod(x)
  package_prefix <- ifelse("package" %in% names(p) , paste0(p$package , "::") , "" )
  if("onload_function" %in% names(p)){
    onload_f <- paste0(package_prefix , p$onload_function , "(controller ,params = p)")
    eval(parse(text= onload_f))
    cli::cli_alert_info("Executed onload for {x} : {onload_f}")
  }
})
onl <- NULL

# Function to create the tab item and call the ui_function in module
create_tab_module <- function(tab_module){
  p <- controller$params_for_mod(tab_module)
  ui_function <-  p$ui_function
  package_prefix <- ifelse("package" %in% names(p) , paste0(p$package , "::") , "" )
 tabItem(
    tabName = tab_module,
    eval(parse(text = paste0(package_prefix , ui_function , "(id = '" , tab_module  ,"' , control = controller ,params = p)"  )))
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
      color = "primary", ## @@title_color
      href = "https://www.shinyspring.dev", ## @@ header_href
      image = "https://storage.googleapis.com/shiny-pics/spring_logo.png", ## @@header_image
      opacity = 0.8
    ),
    fixed = TRUE , ## @@header_fixed
    leftUi = tagList(
      ## Title Text here
      tags$li(class = "dropdown",
              tags$h3("Bs4 Dash Minimalistic") ## @@app_title_h3
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
          tabName = "intro_mod",
          icon = icon("university")
        ),
        menuItem(
          "Core Analysis" ,
          tabName = "core_mod",
          icon = icon("indent")
        ),
        menuItem(
          "Data Exploration" ,
          tabName = "esquiee_mod",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Credits" ,
          tabName = "credits_mod",
          icon = icon("heart")
        )
          )
    ),  ## Close of sidebar
  body = dashboardBody(
    tabItems(
      create_tab_module(tab_module = "intro_mod") ,
      create_tab_module(tab_module = "core_mod") ,
      create_tab_module(tab_module = "esquiee_mod") ,
      create_tab_module(tab_module = "credits_mod") 
      )
    ) # Close of tab items
)



## Define server logic required to draw a histogram
server <- function(input, output , session) {
  mods <- controller$mods_names()
  for(i in 1:length(mods)){
    id <- mods[i]
    p <- controller$params_for_mod(id)
    index <- which(names(controller$params) == paste0(id , ".server_function"))
  if(length(index) > 0 ){
      server_function <- controller$params[index]
      eval(parse(text= paste0(server_function , "(id = '" , id , "' , control = controller , params = p)")))
     }
  }
  }

    ## Run the application
    shinyApp(ui = ui, server = server)

