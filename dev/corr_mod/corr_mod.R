corr_mod_libraries <- function(control ,params){

}

corr_mod_onLoad <- function(control , params){

}


corr_mod_ui <- function(id , control , params){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),
    selectizeInput(ns("dataset_selection"), "Select Dataset", choices =control$dataset_names()  ,
                   multiple = FALSE, width = 400 , options = NULL ),
    box(
      title = "Explore Corrlations with Corrmorant",
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      collapsible = FALSE,
      maximizable = TRUE,
      plotOutput(ns("corrmorant") , width = "100%", height = "8

                 {{00px")  %>% withSpinner(color="#0dc5c1")
    )

    )
}


corr_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns
  ## Your server code here

    })
}

