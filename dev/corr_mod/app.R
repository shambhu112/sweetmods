## This Shiny App created with Shinyspring - http://www.shinyspring.dev
## template : bs4Dash
## app_type : minimal

source("on_startup.R")


params <- config::get(file = "config.yml") ## @@sweetmod_config
controller <- app_master$new(params)
controller$preload_master_with_config()
registry <- sweetmods::mod_registry$new(params)


# Note: This function is to be implemented by app developer in the file on_startup.R
prep_on_start(controller , registry)

# call on_load function on all modules
on_load_for_mods(controller , registry)


## Define UI
ui <- bs4Dash::dashboardPage(

  dark = FALSE , ## FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = "Corelation Mod",
      color = "primary", ## @@title_color
      href = "https://www.shinyspring.dev", ## @@ header_href
      image = "https://storage.googleapis.com/shiny-pics/spring_logo.png", ## @@header_image
      opacity = 0.8
    ),
    fixed = TRUE , ## @@header_fixed
    leftUi = tagList(
      ## Title Text here
      tags$li(class = "dropdown",
              tags$h3("Corelation Analysis") ## @@app_title_h3
      )
    ) ## close left UI
  ),
  sidebar = bs4Dash::dashboardSidebar(
    fixed = TRUE, ## @@side_bar_fixed
    skin = "light",
    status = "primary",
    id = "sidebar",

    bs4Dash::sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Sample App"),

# Whisker:  Menus
        bs4Dash::menuItem(
          "Corelation Analysis" ,
          tabName = "corr_tab",
          icon = icon("indent")
        ),
        bs4Dash::menuItem(
          "Data Exploration" ,
          tabName = "explore_tab",
          icon = icon("chart-bar")
        ),
        bs4Dash::menuItem(
          "Credits" ,
          tabName = "credits_tab",
          icon = icon("heart")
        )
          )
    ),  ## Close of sidebar
  body = bs4Dash::dashboardBody(
    tabItems(
      create_tab_module(tab_module = "corr_tab" , registry , controller) ,
      create_tab_module(tab_module = "explore_tab" , registry , controller) ,
      create_tab_module(tab_module = "credits_tab" , registry , controller) 
      )
    ) # Close of tab items
)



## Define server logic required to draw a histogram
server <- function(input, output , session) {
  mods <- registry$mods_names()
  for(i in 1:length(mods)){
    id <- mods[i]
    p <- registry$params_for_mod(id)
    server_function <- registry$mod_params[[id]]$server_function
    eval(parse(text= paste0(server_function , "(id = '" , id , "' , control = controller , params = p)")))
  }
  }

    ## Run the application
    shinyApp(ui = ui, server = server)

