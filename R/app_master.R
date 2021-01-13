#' R6 master class for storing information that is available across all modules.
#'
#' app_master is the master class used across the shiny app. It Stores all tibbles that are
#' then used by various mods.  These tibbles are stored in reactiveValues rvals
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @import shiny
#' @importFrom R6 R6Class
#' @export
#'
app_master <- R6::R6Class(
  "app_master",
  public = list(
    ### Public variables
    #' @field params initialization parameters
    params = NA,
    #' @field rvals reactiveValues instance that stores all datasets needed in app
    rvals = NULL ,

    #' Standard R6 Initialize function
    #' @noRd
    #'
    #' @param params the config yml driven params for initialization
    #' @return  a new `app_master` object
    initialize = function(params) {
      cli::cli_alert_info("Object app_master initialized")
      self$params <- params
      options("scipen" = 100, "digits" = 4)
      master_data <- tibble::tibble(srnum = numeric() , filenames = character() , dataset_names =  character() ,
                          datasets  = list() , original_cols = list() , snake_cols = list())
      self$rvals <- shiny::reactiveValues(mdata = master_data)
    },

    #' Preload app_master with master_data `
    #'
    #' Note the mdata value in reactiveValues (rvals) will be overwritten by what is provided
    #'
    #' @param master_data mdata in rvals will be raplced with the mmaster_data provided
    preload_master = function(master_data){
      self$rvals <- shiny::reactiveValues(mdata = master_data)
      invisible(self)
    },

    #' Preload app_master with csv files provided in config `
    #'
    #' Note this creates new mdata overiding rvals
    #'
    preload_master_with_config = function(){
      files <- parse_preloads_in_config(value = self$params$file_preloads , sep = ";")
      ds_names <- parse_preloads_in_config(value = self$params$dataset_names_preloads , sep = ";")
      stopifnot(length(ds_names) == length(files))

      master_data <- tibble::tibble(srnum = numeric() , filenames = character() , dataset_names =  character() ,
                                    datasets  = list() , original_cols = list() , snake_cols = list())

      for(x in 1:length(files)){
        df <- read.csv(file = files[x])
        row <- create_row(x , files[x] , ds_names[x] , df )
        master_data<-  rbind(master_data , row)

      }
      self$rvals <- shiny::reactiveValues(mdata = master_data)
      cli::cli_alert_success(" master data loaded with names = {ds_names} ")
      invisible(self)
    },

    #' access mdata easily
    #' @return the mdata object in reactiveValues
    mdata = function(){
      self$rvals$mdata
    },

    #' access dataset names as list
    #' @return the names of datasets
    dataset_names = function(){
      self$rvals$mdata$dataset_names
    },
    #' search for a tibble based on dataset_name
    #' @param dataset_name the name of the dataset to lookup
    #' @return the mapped dataset in tibble format
    data_by_name = function(dataset_name){
      index <- which(self$rvals$mdata$dataset_names == dataset_name)
      stopifnot(length(index) == 1)
      self$rvals$mdata$datasets$data[[index]]
    },

    #' search for a tibble based on index in mdata
    #' @param  index the row index of the dataset
    #' @return the mapped dataset in tibble format
    data_by_index = function(index){
      self$rvals$mdata$datasets$data[[index]]
    }

  )
)
