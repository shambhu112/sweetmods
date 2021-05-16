

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
  library(corrmorant)
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
               bs4Dash::blockQuote(HTML("<strong> Select Columns </strong> that you want to include in your corelation analysis.Columns with Numerics are used for Corelation analysis.
               Hence showing only numerics. selection you can then created the <strong> Network Graph</strong> or the <strong> Corelation Matrix </strong> in tabs below") ,
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
      ) ,
      tabPanel(
        "Corrmorant",
        fluidRow(
          column(3 ,
                 bs4Dash::actionButton(inputId = ns("corrrmant_btn") , label = "Show Matrix" , status = "primary" ),
                 bs4Dash::blockQuote(HTML("Note : corrmorant Charts take some additional time to plot.  Please select <strong> less that 10 columns </strong> ") ,
                            color = "warning") ,
                 bs4Dash::blockQuote(HTML('For more details on corrmorant package <a href="https://github.com/r-link/corrmorant" target="_blank"
                                 rel="noopener noreferrer">Click Here</a> ') ,
                            color = "info")

          ) ,
          column(9,
                 plotOutput(ns("corrrmant_matrix_plot") ,height = 600 )
          )
        )

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

    row_threshhold <- params$max_rows
    #cli::cli_alert_info(" Row threshhol {row_threshhold} is null : {is.null(row_threshhold)}")

    names <- control$dataset_names()
    dq_v <- lapply(names , function(x){
      dq <- control$get_dq_detail(x ,row_threshhold )
      dq$pretty_name <-control$prettynames_for_dataset(x)
      dq
    })

    selections <- column_selection_mod_server("column_selector" , control , dq_v , names )

    output$smarteda_table <-  gt::render_gt({
      gt::gt(control$get_dq_summary(selections()$ds_name , row_threshhold))
    },
    height = gt::px(600),
    width = gt::px(600)
    )


    output$smarteda_table_2 <- gt::render_gt(
      expr = {
        gt::gt(control$get_dq_detail(selections()$ds_name , row_threshhold))
      }
    )

    observeEvent(input$binarize_btn, {
      print(" binarize clickeds")
      #  cols <- which(selections()$col_selections)
      #  d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
      #  d <- d[,cols]

      #  b_tbl <- d %>% binarize(n_bins = 4, thresh_infreq = 0.01)

    })

    observeEvent(input$corrr_btn, {

      print(" corrr clickeds")
      sel <- selections()$col_selections
      sel <- sapply(sel, function(x){ifelse(length(x) >0 , as.logical(x) , FALSE) })
      #    browser()
      cols <- which(sel)
      d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
      d <- d[,cols]
      pd <-  corrr::correlate(d ,use = "pairwise.complete.obs")
      output$corrr_plot <- renderPlot({
        plot(corrr::network_plot(pd , min_cor = .2))
      }
      )
    })

    observeEvent(input$corrrmant_btn, {




      sel <- selections()$col_selections
      sel <- sapply(sel, function(x){ifelse(length(x) >0 , as.logical(x) , FALSE) })
      # browser()
      cols <- which(sel)
      d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
      d <- d[,cols]
      output$corrrmant_matrix_plot <- renderPlot( {
        p <- corrmorant::ggcorrm(d , rescale = "by_sd") +
          corrmorant::utri_heatmap(alpha = 0.5) +
          corrmorant::lotri_heatcircle(alpha = 0.5, col = 1) +
          corrmorant::utri_corrtext() +
          corrmorant::dia_names(y_pos = 0.15, size = 3) +
          corrmorant::dia_density(lower = 0.3, fill = "lightgrey", color = 1) +
          corrmorant::scale_fill_corr()
        p
      }
      )
    })

    observeEvent(input$corrr_matrix_btn, {
      print(" corrr matrix")
      sel <- selections()$col_selections
      sel <- sapply(sel, function(x){ifelse(length(x) >0 , as.logical(x) , FALSE) })
      # browser()
      cols <- which(sel)
      d <- control$dataset_by_name(selections()$ds_name , row_threshhold)
      d <- d[,cols]
      d <- corrr::correlate(d)
      d <- corrr::rearrange(d)
      d <- corrr::shave(d)

      output$corrr_matrix_plot <- renderPlot({
        plot(corrr::rplot(d , legend = TRUE , shape = 16 ,print_cor = TRUE  ))
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

