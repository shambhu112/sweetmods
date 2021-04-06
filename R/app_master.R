#' R6 master class (Controller) for your shiny application.
#'
#' @description
#' app_master is the master class used across all modules of the the shiny app. It Stores all datasets needed in the applications
#' as tibbles. These tibbles are stored in \code{reactiveValues} \code{rvals}
#'
#' @name app_master
#' @import shiny
#' @importFrom R6 R6Class
#' @export
app_master <- R6::R6Class(
  "app_master",
  public = list(
    ### Public variables

    #' @field params initialization parameters
    params = NA,
    #' @field reactive_vals reactiveValues instance that stores all reactive values needed in app
    reactive_vals = NULL ,

    #' @field static values, non-reactive static datasets needed in the application. Note you can only pre-load these
    master_data = NULL ,

    #' @description Standard R6 Initialize function
    #'
    #' @param params the config yml driven params for initialization
    #' @return  a new `app_master` object
    initialize = function(params) {
      cli::cli_alert_info("Object app_master initialized")
      self$params <- params
      options("scipen" = 100, "digits" = 4)
      self$master_data <- tibble::tibble(srnum = numeric() , connection_str = character() , dataset_names =  character() ,
                          datasets  = tibble::tibble() , original_cols = list() , snake_cols = list() , connection_type = character())


      self$reactive_vals <- shiny::reactiveValues()
    },

    #' Preload app_master with master_data `
    #'
    #' Note the mdata value in reactiveValues (rvals) will be overwritten by what is provided
    #'
    #' @param master_data mdata in rvals will be raplced with the mmaster_data provided
    preload_master = function(master_data){
      self$master_data <- master_data
      invisible(self)
    },

    add_master_data_row = function(row){
      self$master_data <- dplyr::bind_rows(self$master_data , row)
    },


    #' Preload app_master with csv files provided in config `
    #'
    #' Note this creates new mdata overiding rvals
    #' @return self object
    preload_master_with_config = function(){
      pr_rx_file <- self$params$file_preloads

      if(!is.null(pr_rx_file)){
        files <- parse_preloads_in_config(value = self$params$file_preloads , sep = ";")
        ds_names <- parse_preloads_in_config(value = self$params$dataset_names_preloads , sep = ";")
        stopifnot(length(ds_names) == length(files))

        loaded_files <- read_files(files)
        fnames <- names(loaded_files)

        for(x in 1:length(files)){
          cli::cli_alert_info("Creating entry for  {files[x]}")
          df <- as.data.frame(loaded_files[[fnames[x]]])
          row <- create_row(x , files[x] , ds_names[x] , df , "TBD") #TODO fix file type
          self$add_master_data_row(row)
        }
        cli::cli_alert_success(" master data loaded with names = {ds_names} ")
      }

      invisible(self)
    },


    #' access dataset names as list
    #' @return the names of datasets
    dataset_names = function(){
      self$master_data$dataset_names
    },

    #' search for a tibble based on dataset_name
    #' @param dataset_name the name of the dataset to lookup
    #' @return the mapped dataset in tibble format
    dataset_by_name = function(dataset_name){
      index <- which(self$master_data$dataset_names == dataset_name)
      stopifnot(length(index) == 1) #TODO : Clean handling needed here , message
      self$master_data$datasets[[index]]
    },

    #' search for a tibble based on index in mdata
    #' @param  index the row index of the dataset
    #' @return the mapped dataset in tibble format
    data_by_index = function(index){
      self$master_data$datasets[[index]]
    }

  )
)
