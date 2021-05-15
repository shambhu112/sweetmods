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
    #' @field params initialization parameters
    params = NA,
    #' @field reactive_vals reactiveValues instance that stores all reactive values needed in app
    reactive_vals = NULL ,

    #' @field master_data static values, non-reactive static datasets needed in the application. Note you can pre-load these
    master_data = NULL ,

    #' @field  dataset configuration extracted from master config
    ds_config = NULL ,

    #' @description Standard R6 Initialize function
    #'
    #' @param params the config yml driven params for initialization
    #' @return  a new `app_master` object
    initialize = function(params) {
      m_info("Object app_master initialized")
      self$params <- params
      options("scipen" = 100, "digits" = 4)
      self$master_data <- tibble::tibble(srnum = numeric() , connection_str = character() , dataset_names =  character() ,
                          datasets  = tibble::tibble() , original_cols = list() , pretty_cols = list() , connection_type = character())

      self$load_ds_config(params)
      self$reactive_vals <- shiny::reactiveValues()
    },

    #' Add a row to app_master `
    #'
    #' @param row  new row object created with create_row
    add_master_data_row = function(row){
      self$master_data <- dplyr::bind_rows(self$master_data , row)
    },

    #' removes the indexed row from app_master `
    #' Note: the indexes are reindexed from 1 to nrow(master_data) after removal
    #' @param index the rowindex
    remove_dataset = function(index){
      self$master_data <- self$master_data[-index,]
      self$master_data$srnum <- seq(1:nrow(self$master_data))
    },

    #' replaces dataset with new row at the same index
    #' @param dataset_name the ds_name
    #' @param replace_with the row created with create_row
    replace_dataset_by_name = function(dataset_name , replace_with){
      index <- which(self$master_data$dataset_names == dataset_name)
      stopifnot(length(index) == 1) #TODO : Clean handling needed here , message
      self$master_data$datasets[index,] <- tidyr::nest(replace_with , data = everything())
    },
    #' load dataset config in a consumable fashion
    #' @param params the params
   load_ds_config = function(params){
#     index <- which(stringr::str_detect(names(params) , pattern = "ds.\\D"))
      index <- which(stringr::str_detect(names(params) , pattern = "^ds."))

      if(length(index) == 0 ) return(NULL)

      prop <- names(params)[index]

      t <- stringr::str_split(prop , pattern = "[.]")

      ds_names <- sapply(1:length(t), function(x){
        t[[x]][[2]]
      })
      ds_names <- unique(ds_names)

      ds_params <- sapply(ds_names, function(x){
        pat <- paste0("ds." , x , ".\\D")
        sindex <- which(stringr::str_detect(names(params) , pattern = pat ))
        sub_config <- params[sindex]
        sub_ds_props <- sapply(names(sub_config), function(x2){
          p <- stringr::str_split(x2 , pattern = "[.]")
          param_nm <- p[[1]][3]
        })
        sub_ds_props <- as.list(sub_ds_props)
        names(sub_config) <- sub_ds_props
        sub_config
      })


       if("matrix" %in% class(ds_params)){
         ds_params <- as.list(as.data.frame(ds_params))
       }

      self$ds_config <- ds_params
    },

    #' Preload app_master with csv files provided in config `
    #'
    #' Note this creates new mdata overiding rvals
    #' @return self object
    preload_master_with_config = function(){
      config <- self$ds_config

      # TODO  : consider parallel options here
      for(x in names(config)){
        ds_nm <- x
        ds_params <- config[[ds_nm]]
        row <- load_row(name = ds_nm, ds_params = ds_params , controller = self)
        self$add_master_data_row(row)
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
    #' @param max_rows (optional) if max_rows is set then sample max_rows from dataset.
    #' @return the mapped dataset in tibble format
    dataset_by_name = function(dataset_name , max_rows = Inf){
      index <- which(self$master_data$dataset_names == dataset_name)
      stopifnot(length(index) == 1) #TODO : Clean handling needed here , message
      ret <- as.data.frame(self$master_data$datasets[index,]$data)
      v <- as.numeric(max_rows)
      if(is.finite(v) && (v < nrow(ret)) ){
        ret <- dplyr::sample_n(ret , size = max_rows )
      }
      ret
    },

    #' search for a tibble based on index in mdata
    #' @param  index the row index of the dataset
    #' @return the mapped dataset in tibble format
    data_by_index = function(index){
      self$master_data$datasets[[index]]
    },

    #' get colnames for a dataset
    #' @param dataset_name the name of the dataset to lookup
    #' @return characted list of colnames
    colnames_for_dataset = function(dataset_name){
      index <- which(self$master_data$dataset_names == dataset_name)
      stopifnot(length(index) == 1) #TODO : Clean handling needed here , message
      ret <- self$master_data$original_cols[index]$cname
      ret
    } ,

    #' get pretty colnames  for a dataset
    #' @param dataset_name the name of the dataset to lookup
    #' @return characted list of colnames
    prettynames_for_dataset = function(dataset_name){
      index <- which(self$master_data$dataset_names == dataset_name)
      stopifnot(length(index) == 1) #TODO : Clean handling needed here , message
      ret <- self$master_data$pretty_cols[index]$pnames
      ret
    }
  )
)
