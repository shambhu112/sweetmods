

#' onLoad for corr_mod
#'
#' @description A shiny spring Module.
#'
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
corr_mod_onLoad <- function(control , params){
  library(gt)
  library(SmartEDA)
  library(tippy)
  library(corrr)
}

#' ui_function for corr_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export

corr_mod_ui <- function(id , control , params ){
  ns <- NS(id)
  fluidRow(
    shinyjs::useShinyjs(),

    bs4Dash::box(
      title = "Select Dataset and Columns",
      width = 12,
      status = "primary",
      side = "right",
      solidHeader = FALSE,
      collapsible = TRUE,
      maximizable = TRUE,
      id = "selection_box",
      fluidRow(
        column(4 ,
               bs4Dash::blockQuote(HTML("<strong> Select Columns </strong> that you want to include in your corelation analysis.Columns with Missing Values will not work hence they are not selected in the list below.
        Post selection you can binarize data. <strong> Binarize </strong> can take some time depending on the size of dataset. ") ,
                                   color = "teal")
        ),
        column(8 ,
               column_selection_mod_ui(id = ns("column_selector") , dataset_names =  control$dataset_names() )
        )
      )
    ),

    bs4Dash::tabBox(
      title = "",
      width = 12,
      status = "primary",
      side = "right",
      solidHeader = FALSE,
      collapsible = TRUE,
      maximizable = TRUE,
      id = "corr_tab",
      tabPanel(
        "About Dataset",
        h5("Dataset summary : "),
        gt::gt_output(outputId = ns("smarteda_table"))
      ),
      tabPanel(
        "Dataset Detail",
        h5("Dataset Details : "),
        gt::gt_output(outputId = ns("smarteda_table_2"))
      ),
      tabPanel(
        "Correlation Network",
        bs4Dash::actionButton(inputId = ns("corrr_btn") , label = "Show Network" , status = "primary" ) ,
        plotOutput(ns("corrr_plot"))
      ),
      tabPanel(
        "Correlation Matrix",
        bs4Dash::actionButton(inputId = ns("corrr_matrix_btn") , label = "Show Matrix" , status = "primary" ),
        plotOutput(ns("corrr_matrix_plot"))
      )
    ))
}


#' server_function for corr_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export

corr_mod_server <- function(id , control , params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #TODO : Fix situation when no datasets available
    if(nrow(control$master_data) == 0 )
      return(NULL)

    row_threshhold <- params$thresh_hold_rows

    names <- control$dataset_names()
    dq_v <- lapply(names , function(x){
      df <- control$dataset_by_name(x , row_threshhold)
      dq <- SmartEDA::ExpData(df  , type = 2)
      dq$pretty_name <-control$prettynames_for_dataset(x)
      dq
    })

    selections <- column_selection_mod_server("column_selector" , control , dq_v , names )

    output$smarteda_table <-  gt::render_gt({
      d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
      expr = gt_tbl(control$dataset_by_name(selections()$ds_name))
    },
    height = gt::px(600),
    width = gt::px(600)
    )


    output$smarteda_table_2 <- gt::render_gt(
      expr = {
        d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
        gt_tbl2(control$dataset_by_name(selections()$ds_name))
      }
    )

    observeEvent(input$binarize_btn, {
      print(" binarize clickeds")
      cli::cli_alert_info(" ds name {selections()$ds_name}  selections {selections()$col_selections}")
    #  cols <- which(selections()$col_selections)
    #  d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
    #  d <- d[,cols]

    #  b_tbl <- d %>% binarize(n_bins = 4, thresh_infreq = 0.01)

    })

    observeEvent(input$corrr_btn, {
      print(" corrr clickeds")
      cols <- which(selections()$col_selections)
      d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
      d <- d[,cols]
      pd <-  corrr::correlate(d ,use = "pairwise.complete.obs")
      output$corrr_plot <- renderPlot({
        plot(corrr::network_plot(pd , min_cor = .2))
      }
      )
    })

    observeEvent(input$corrr_matrix_btn, {
      print(" corrr matrix")
      cols <- which(selections()$col_selections)
      d <- control$dataset_by_name(selections()$ds_name , row_threshhold)

      d <- corrr::correlate(d[,cols])
      d <- corrr::rearrange(d)
      d <- corrr::shave(d)
      output$corrr_matrix_plot <- renderPlot({
        plot(corrr::rplot(d))
      })

    })

  })

}

gt_tbl <- function(data) {
  smd <- as.data.frame(SmartEDA::ExpData(data , type = 1))
  gt::gt(smd)
}

gt_tbl2 <- function(data) {
  smd <- as.data.frame(SmartEDA::ExpData(data , type = 2))
  gt::gt(smd)
}


create_pretty_checkbox <- function(id , dq , missing_threshhold = 0.0){
  x <- dq$Per_of_Missing
  missing <- dplyr::case_when(
    x > missing_threshhold ~ "high" ,
    TRUE  ~ "low")
  if("high" == missing){
    ui <- tippy::with_tippy(
      element =  shinyWidgets::prettyCheckbox( inputId = id , label = dq$pretty_name , value = FALSE , shape = "curve" , status = "danger" , inline = TRUE ) ,
      tooltip = glue::glue("<strong> missing {x * 100}% </strong> of values") ,
      allowHTML = TRUE
    )
  }else{
    ui <-tippy::with_tippy(
      element = shinyWidgets::prettyCheckbox( inputId = id , label = dq$pretty_name , value = TRUE , shape = "curve", status = "success" ,  inline = TRUE) ,
      tooltip = glue::glue("type: {dq$Variable_Type}" ) ,
      allowHTML = TRUE
    )
  }
  ui
}

