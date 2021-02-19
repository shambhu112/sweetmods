library(bs4Dash)
library(shiny)
library(thematic)
library(waiter)
library(sweetmods)
library(shinyjs)
library(leaflet)
library(shinyWidgets)

setwd("~/Rprojects/sweetmods/dev")

thematic_shiny()

params <- config::get(file = "dev_config.yml")

controller <- sweetmods::app_master$new(params)
#controller <- on_app_start(controller)


demo_ui <- function(id, control) {
  ns <- NS(id)
  h4(paste0("Hello " , id))
}



demo_server <- function(id, control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}


# toast options
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)

sweet_tab <- function(tab_name , module_name , module_function , control = "controller"){
  tabItem(
    tabName = tab_name,
    eval(parse(text = paste0(module_function , "(id = '" , module_name  ,"' , control = " , control, " )"  )))
  )
}

# Define UI for application that draws a histogram
ui <- bs4Dash::dashboardPage(
  #  preloader = list(
  #    waiter = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  #    duration = 0
  #  ),

  dark = TRUE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,

  header = dashboardHeader(
    title = dashboardBrand(
      title = "Shiny Spring",
      color = "primary",
      href = "http://www.shinyspring.dev",
      image = "https://lh6.googleusercontent.com/ib4GlfxE5b8ADpDcrL3Oyj9O6Y97EWGFV-ZQmcb9gvPVZbBjsnMkdl2Emobaz7ovaFn3yFnASbYlnb7HlFaSuLQ=w16383",
      opacity = 0.8
    ),
    fixed = TRUE,
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
      ),
      userOutput("user")
    ),
    leftUi = tagList(
      # Close dropdownMenu
      tags$li(class = "dropdown",
              tags$h3("Bank Branch Optimizer")
      )
    ) # close left UI
  ),
  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    id = "sidebar",
    customArea = fluidRow(
      actionButton(
        inputId = "myAppButton",
        label = NULL,
        icon = icon("users"),
        width = NULL,
        status = "primary",
        style = "margin: auto",
        dashboardBadge(textOutput("btnVal"), color = "danger")
      )
    ),
    sidebarUserPanel(
      image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
      name = "Welcome"
    ),
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Banks"),
      menuItem(
        "Institutions",
        tabName = "institutions_tab",
        icon = icon("university")
      ),
      menuItem(
        "Branch Network",
        tabName = "branches_tab",
        icon = icon("piggy-bank")
      )

    )
  ), # Close of sidebar
  body = dashboardBody(
    tabItems(
      # List all modules for tab here

      sweet_tab(tab_name = "institutions_tab" ,module_name = "inst_mod_1" , module_function = "demo_ui" ) ,
      sweet_tab(tab_name = "branches_tab" ,module_name = "branch_mod_1" , module_function = "demo_ui" )


    )
  ) , #close of body
  controlbar = dashboardControlbar(
    id = "controlbar",
    skin = "light",
    pinned = FALSE,
    overlay = FALSE,
    controlbarMenu(
      id = "controlbarMenu",
      type = "pills",
      controlbarItem(
        "Years",
        column(
          width = 12,
          align = "center",
          radioButtons(
            inputId = "analytics_years",
            label = "Subset Analysis for",
            c(
              "3 years" = "yrs_3",
              "5 years" = "yrs_5",
              "10 years" = "yrs_10",
              "20 years" = "yrs_20"
            )
          )
        )
      ),
      controlbarItem(
        "Skin",
        skinSelector()
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output , session) {
  demo_server(id = "inst_mod_1" , control = controller)
  demo_server(id = "branch_mod_1" , control = controller)

  output$user <- renderUser({
    dashboardUser(
      name = "Niraj",
      image = "https://upload.wikimedia.org/wikipedia/commons/1/12/User_icon_2.svg",
      title = "webscale",
      subtitle = "Admin",
      footer = p("The footer", class = "text-center"),
      fluidRow(
        dashboardUserItem(
          width = 6,
          "Item 1"
        ),
        dashboardUserItem(
          width = 6,
          "Item 2"
        )
      )
    )
})

}

# Run the application
shinyApp(ui = ui, server = server)
