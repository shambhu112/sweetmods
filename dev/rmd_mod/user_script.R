## Start your Shiny Spring Journey here ----

## Step 1 : Install dependencies ----
remotes::install_github("rstudio/htmltools")
remotes::install_github("rstudio/shiny")

# optional for plot autocolor
remotes::install_github("rstudio/thematic")
devtools::install_github("RinteRface/bs4Dash")

# You might have to update shinyspring and sweetmods

devtools::install_github("shambhu112/shinyspring")
devtools::install_github("shambhu112/sweetmods")


## Step 2 : Make sure that your properties in config.yml are set as per your needs
file.edit('config.yml')

## Step 3 : Create app.R for your application
params <- config::get(file = "config.yml") # load params
shinyspring::create_shinyapp(params = params )

## Step 4 : Launch the App
shiny::runApp()

## Step 5 : Create Module for your app
shinyspring::create_module("my_custom_mod")


## Advanced Options ----
# Step :  Download Templateto make a customtemplate
download.file(url = "https://raw.githubusercontent.com/shambhu112/shinyspring/main/inst/bs4/bs4_minimal.mst" ,
              destfile = "bs4_custom.mst")

# Step  : Edit the Custom Template to suit your needs
file.edit("bs4_custom.mst")

# Step Create a new shiny app with custom template
params <- config::get(file = "config.yml") # load params
shinyspring::create_shinyapp(params , target_file = "myapp.R" , template_file = "bs4_custom.mst")

##Debug inside a mod

params <- config::get(file = "config.yml") ## @@sweetmod_config
controller <- app_master$new(params)
controller$preload_master_with_config()
registry <- sweetmods::mod_registry$new(params)
mod_names <- registry$mods_names()


control <- controller
master_params <- controller$params
params <- registry$params_for_mod("credits_mod")



registry$mod_names
registry$master_params




