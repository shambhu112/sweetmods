my_custom_mod_libraries <- function(control ,params){

}

my_custom_mod_onLoad <- function(control , params){

}


my_custom_mod_ui <- function(id , control , params){
  ns <- NS(id)
  fluidRow(

   ##Your UI Code here

    )
}


my_custom_mod_server <- function(id , control , params){
moduleServer( id, function(input, output, session){
    ns <- session$ns
  ## Your server code here

    })
}

