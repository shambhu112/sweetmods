
#' ui_function for column_selection_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
column_selection_mod_ui <- function(id, dataset_names) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("dataset_selection"), "Select Dataset",  choices = dataset_names,
                   multiple = FALSE, width = 400, options = NULL),
    uiOutput(ns("conditional_checkboxes"))
  )

}


#' server_function for column_selection_mod
#'
#' @description A shiny spring Module UI function
#' @param id the id
#' @param control the controller  \code{app_master}
#' @param params for the mod
#' @export
column_selection_mod_server <- function(id, control, dq_v, dataset_names , filter_types = c("everything")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    names <- dataset_names
    names(dq_v) <- names
    data_r <- reactiveValues(
      name = names[1], ds = control$dataset_by_name(names[1]),
      dq = as.data.frame(dq_v[1]),
      selections = list(ds_name = names[1], col_selections = list(rep(FALSE, nrow(dq_v[[1]]))))
    )
    observe({
      sel <- input$dataset_selection
      data_r$name <- ifelse(is.null(sel), names[1], sel)
      data_r$ds <- control$dataset_by_name(data_r$name)
      data_r$dq <- dq_v[[data_r$name]]
      cbox_states <- sapply(1:nrow(data_r$dq), function(i) {
        ids <- paste0(data_r$name, "_", i)
        #  cli::cli_alert_info("Selected {input[[ids]]}")
        input[[ids]]
      })
      data_r$selections <- list(ds_name = data_r$name, col_selections = cbox_states)
    })

    output$conditional_checkboxes <- renderUI({
      dq <- data_r$dq

      lapply(1:nrow(dq), function(i) {
        ids <- paste0(data_r$name, "_", i)

        if(identical(dq$Variable_Type[[i]] , "numeric") | identical(dq$Variable_Type[[i]] , "integer") ){
          cb <- create_pretty_checkbox_with_tippy(id = ids, dq[i, ], missing_threshhold = 0.0, ns)
        #  print(cb)
          cb
        } else{
           cb <-  shiny::helpText(id = ids, dq$pretty_name[[i]] , value = "FALSE")
        }
        cb
      })
    })

    return(reactive(data_r$selections))
  })
}

create_pretty_checkbox <- function(id, dq, missing_threshhold = 0.0, ns) {
  x <- dq$Per_of_Missing
  missing <- dplyr::case_when(
    x > missing_threshhold ~ "high",
    TRUE ~ "low"
  )

  if ("high" == missing) {
    ui <- shinyWidgets::prettyCheckbox(
      inputId = ns(id), label = dq$pretty_name, value = FALSE,
      shape = "curve", status = "danger", inline = TRUE
    )
  } else {
    ui <- shinyWidgets::prettyCheckbox(
      inputId = ns(id), label = dq$pretty_name, value = TRUE,
      shape = "curve", status = "success", inline = TRUE
    )
  }
  ui
}


create_pretty_checkbox_with_tippy <- function(id, dq, missing_threshhold = 0.0, ns) {
  x <- dq$Per_of_Missing
  missing <- dplyr::case_when(
    x > missing_threshhold ~ "high",
    TRUE ~ "low"
  )

  if ("high" == missing) {
    ui <- tippy::with_tippy(
      element = shinyWidgets::prettyCheckbox(inputId = ns(id), label = dq$pretty_name, value = FALSE, shape = "curve", status = "danger", inline = TRUE),
      tooltip = glue::glue("<strong> missing {x * 100}% </strong> of values"),
      allowHTML = TRUE
    )
  } else {
    ui <- tippy::with_tippy(
      element = shinyWidgets::prettyCheckbox(inputId = ns(id), label = dq$pretty_name, value = TRUE, shape = "curve", status = "success", inline = TRUE),
      tooltip = glue::glue("type: {dq$Variable_Type}"),
      allowHTML = TRUE
    )
  }
  ui
}
