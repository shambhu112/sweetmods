## Start your Shiny Spring Journey here ----

## Step 1 : Make sure that your properties in config.yml are set as per your needs
file.edit('config.yml')

## Step 2 : Create app.R for your application
params <- config::get(file = "config.yml") # load params
shinyspring::create_shinyapp(params = params )

## Step 3 : Launch the App
shiny::runApp()

## Step 4 : Create Module for your app
shinyspring::create_module("my_custom_mod")


## Advanced Options ----
# Step :  Download Templateto make a customtemplate
download.file(url = "https://raw.githubusercontent.com/shambhu112/shinyspring/main/inst/bs4/bs4_minimal.mst" ,
              destfile = "bs4_custom.mst")

# Step  : Edit the Custom Template to suit your needs
file.edit("bs4_custom.mst")

params <- config::get(file = "config.yml") # load params
# Step Create a new shiny app with custom template
shinyspring::create_shinyapp(params , target_file = "myapp.R" , template_file = "bs4_custom.mst")



