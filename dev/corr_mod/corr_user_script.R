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
file.edit('corrconfig.yml')

## Step 3 : Create app.R for your application
params <- config::get(file = "corrconfig.yml") # load params
shinyspring::create_shinyapp(params = params )

## Step 4 : Launch the App
shiny::runApp()



