#' data_upload UI Function
#'
#' @description A File Data Upload Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @import shiny
#' @import tidyr
#' @export
file_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Dataset Uploads"),
    fluidRow(
      column(
        4,
        wellPanel(
          fileInput(
            inputId = ns("MyFiles"),
            label = "Upload files"
          ),
          shinyjs::useShinyjs(),
          textInput(ns("name_id"), label = "Enter Dataset Name"),
          shinyjs::disabled(
            actionButton(ns("add_btn"), label = "Add"),
            actionButton(ns("remove_btn"), label = "Remove"),
            actionButton(ns("preview_btn"), label = "Preview Table")
          )
        )
      ),
      column(
        8,
        mainPanel(
          titlePanel("Datasets Loaded"),
          reactable::reactableOutput(ns("table"))
        )
      )
    ),
    fluidRow(
      column(12 ,
             reactable::reactableOutput(ns("preview_table"))
      )


    )
  )
}

#' data_upload Server Functions
#' @import dplyr
#' @import shiny
#' @param id standard shiny server signature
#' @param master the \code{app_master} reference for this module
#' @export
file_upload_server <- function(id , master){
  moduleServer(id, function(input, output, session ){
    ns <- session$ns
    fileInfo <- reactive(input$MyFiles)

    observeEvent(input$MyFiles, {
      f <- fileInfo()
      cli::cli_alert_info("  name {f$name}  ")

      #Creating the default name of dataset keeping the extention out
      a <- unlist(stringr::str_split(f$name, "\\."))
      l <- length(a) - 1
      nm <- ifelse(l > 0, a[1:l], a)
      nm <- snakecase::to_any_case(nm)
      cli::cli_alert_info("  ds name {nm}  ")
      # Name the Dataset with files name without the extention
      # And enable the add button
      updateTextInput(
        session = session, inputId = "name_id",
        value = nm
      )
      shinyjs::enable("add_btn")
    })

    #reactable selection tracker
    selected <- reactive(reactable::getReactableState("table", "selected"))

    # Call Onclick of add_btn

    shinyjs::onclick("add_btn", {
      mdf <- reactive({master$rvals$mdata})
      f <- fileInfo()

      # TODO - repalce this with debug
      cli::cli_alert_info("file attributes {attributes(f)}  ")
      lapply(f, function(x) {
        cli::cli_alert_info("  item {x}  ")
      })

      # Read uploaded file
      # TODO put some checks at file load time
      # TODO remove warnings
      ds <- readr::read_csv(f$datapath )
      #Create Row : note nested dataset in tibble
      row <- create_row(
        srnum = nrow(mdf()) + 1 ,
        filename = f$name ,
        ds_name = input$name_id ,
        ds = ds ,
        format = "csv"
      )
      #Update the DataFrame with new row
      new_df <- reactive({rbind(mdf() , row)})
      cli::cli_alert_info("New DF rows {nrow(new_df())}")
      master$rvals$mdata <- new_df()
      cli::cli_alert_info(" Master dataset rows {nrow(master$rvals$mdata)} names = {master$rvals$mdata$dataset_names} " )

      #Updata UI
      updateTextInput(session = session, inputId = "name_id", value = "")
      shinyjs::disable("add_btn")
      shinyjs::reset("MyFiles")
    })

    # Render Reactable table
    output$table <- reactable::renderReactable({
      dataset_tbl(input, output, session, master$rvals$mdata )
    })

    # Remove button selection logic
    observe({
      # print(selected())
      if(is.numeric(selected())){
        shinyjs::enable("remove_btn")
        shinyjs::enable("preview_btn")
      }else{
        shinyjs::disable("remove_btn")
        shinyjs::disable("preview_btn")
      }
    })

    shinyjs::onclick("remove_btn", {
      index <- selected()
      master$rvals$mdata <- master$rvals$mdata[-c(index),]
      cli::cli_alert_info(" remove clicked new rows = {nrow(master$rvals$mdata)}")
      if(nrow(master$rvals$mdata) > 0)
        master$rvals$mdata$srnum <- seq(1:nrow(master$rvals$mdata))
      output$preview_table <- NULL
    })

    shinyjs::onclick("preview_btn", {
      index <- selected()
      ds <- master$rvals$mdata$datasets[index,]$data[[1]]
      colnames(ds) <- master$rvals$mdata$snake_cols[index]$sname
      cli::cli_alert_info(" ds for index = {index} with rows = {nrow(ds)}")
      output$preview_table <- reactable::renderReactable({
        reactable::reactable(utils::head(ds))
      })
    })
  })
}



#' Create the reactable table that shows the datasets
#'
#' The table shows the master mdata
#' @param input standard shiny module parameter
#' @param output standard shiny module parameter
#' @param session standard shiny module parameter
#' @param tbl_df the data.frame that will be drawn with reatable
#'
#' @importFrom reactable colDef
#' @export
dataset_tbl <- function(input, output, session, tbl_df) {
  reactable::reactable(tbl_df,
                       selection = "single",
                       onClick = "select",
                       defaultColDef = colDef(
                         align = "center",
                         minWidth = 70,
                         headerStyle = list(background = "#f7f7f8")
                       ),
                       columns = list(
                         srnum = colDef(name = "Num"),
                         dataset_names = colDef(name = "Dataset Names"),
                         connection = colDef(name = "Source Filename"),
                         datasets = colDef(show = FALSE),
                         original_cols = colDef(show = FALSE) ,
                         snake_cols = colDef(show = FALSE),
                         format = colDef(show = FALSE)
                       )
  )
}


