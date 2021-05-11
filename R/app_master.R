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

    #' @description Standard R6 Initialize function
    #'
    #' @param params the config yml driven params for initialization
    #' @return  a new `app_master` object
    initialize = function(params) {
      cli::cli_alert_info("Object app_master initialized")
      self$params <- params
      options("scipen" = 100, "digits" = 4)
      self$master_data <- tibble::tibble(srnum = numeric() , connection_str = character() , dataset_names =  character() ,
                          datasets  = tibble::tibble() , original_cols = list() , pretty_cols = list() , connection_type = character())

      self$reactive_vals <- shiny::reactiveValues()
    },

    #' Preload app_master with master_data `
    #'
    #'
    #' @param master_data mdata in rvals will be raplced with the mmaster_data provided
    preload_master = function(master_data){
      self$master_data <- master_data
      invisible(self)
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

    #' Preload app_master with csv files provided in config `
    #'
    #' Note this creates new mdata overiding rvals
    #' @return self object
    preload_master_with_config = function(){
      pr_rx_file <- self$params$file_preloads

      if(!is.null(pr_rx_file)){
        files <- parse_preloads_in_config(value = self$params$file_preloads , sep = ";")
        ds_names <- parse_preloads_in_config(value = self$params$file_preloads_ds_name , sep = ";")
        stopifnot(length(ds_names) == length(files))

        loaded_files <- files %>% purrr::map(read_file)
        #fnames <- names(loaded_files)

        for(x in 1:length(files)){
          cli::cli_alert_info("Creating entry for  {files[x]}")
        #  df <- as.data.frame(loaded_files[[fnames[x]]])
          df <- as.data.frame(loaded_files[[x]])
          row <- create_row(x , files[x] , ds_names[x] , df , "TBD") #TODO fix file type
          self$add_master_data_row(row)
        }
        cli::cli_h3(" master data loaded with names = {ds_names} ")
      }

      ds_count <- nrow(self$master_data)
      tar_names <- self$params$tar_loads
      raw_mode <- ifelse(length(self$params$tar_load_rawmode) > 0 , as.logical(self$params$tar_load_rawmode) , FALSE)
      if(!is.null(tar_names)){
        tars <- parse_preloads_in_config(value = tar_names , sep = ";")
        for (i  in 1:length(tars)) {
          row <- create_row(
                      srnum = ds_count + i ,
                      filename = paste0("tar " , tars[i]) ,
                      ds_name = tars[i],
                      ds =  sweetmods::load_tar_as_tibble(tars[i] , raw_mode) ,
                      format = "tar"
                     )
          self$add_master_data_row(row)
        }
      }

      builtin_nms <- self$params$builtin_datasets

      if(!is.null(builtin_nms)){
          built_inds <- parse_preloads_in_config(value = builtin_nms , sep = ";")
          for(i in 1:length(built_inds)){
          if(!exists(built_inds[i])) cli::cli_alert_danger(" The dataset {built_inds[i]} does not exists on system.")
            row <- create_row(
                 srnum = ds_count + i ,
                filename = paste0("builtin " , built_inds[i]) ,
                ds_name = built_inds[i],
                ds =  sweetmods::load_built_ts_as_tibble(built_inds[i]) ,
                format = "builtin"
            )
            self$add_master_data_row(row)
          }
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
    dataset_by_name = function(dataset_name , max_rows = NULL){
      index <- which(self$master_data$dataset_names == dataset_name)
      stopifnot(length(index) == 1) #TODO : Clean handling needed here , message

      ret <- as.data.frame(self$master_data$datasets[index,]$data)
      if(!is.null(max_rows) && max_rows < nrow(ret)){
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
