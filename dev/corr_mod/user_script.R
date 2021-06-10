## Start your Shiny Spring Journey here ----

## Install dependencies ----
remotes::install_github("rstudio/htmltools")
remotes::install_github("rstudio/shiny")
remotes::install_github("rstudio/thematic")
remotes::install_github("RinteRface/bs4Dash")

## packages used by sweetmods ----
remotes::install_github("r-link/corrmorant")
remotes::install_github("JohnCoene/tippy")

# You might have to update shinyspring and sweetmods
devtools::install_github("shambhu112/shinyspring")
devtools::install_github("shambhu112/sweetmods")


## Step 1 : Make sure that your properties in config.yml are set as per your needs ----
file.edit('config.yml')

## Step 2 : load params from config file ----
params <- config::get(file = "config.yml") # load params

## Step 3 : check to see if the config fle you have is good ----
shinyspring::test_config_file(params)

## Step 4 : Create app.R for your application ----
shinyspring::create_shinyapp(params = params )

## Step 5 : Launch the App
shiny::runApp()

## Now get a feeling of the app :)

## Step 6 : Create Module for your app
shinyspring::create_module("my_custom_mod")


## Advanced Options ----

# To customize template
# Step A:  Download Template to make a customtemplate
download.file(url = "https://raw.githubusercontent.com/shambhu112/shinyspring/main/inst/bs4/bs4_minimal.mst" ,
              destfile = "bs4_custom.mst")
# Step B: Edit the Custom Template to suit your needs
file.edit("bs4_custom.mst")

# Step C: Create a new shiny app with custom template
params <- config::get(file = "config.yml") # load params
shinyspring::create_shinyapp(params , target_file = "myapp.R" , template_file = "bs4_custom.mst")
shinyspring::test_config_file(params)



