
#' Loads a Row based on file type
#' @param name the name of dataset
#' @param ds_params the params specific to the ds
#' @param controller app_master
#' @return row a Row object
#' @export
load_row <- function(name , ds_params , controller){
  type <- ds_params$type
  the_ds <- NULL
   if(identical(type ,"csv")) the_ds <- csv_file(name , ds_params , controller)
   if(identical(type , "csv_dir")) the_ds <- csv_dir(name , ds_params , controller)
   if(identical(type , "rds")) the_ds <- rds_file(name , ds_params , controller)
   if(identical(type , "arrow")) the_ds <- arrow_file(name , ds_params , controller)
   if(identical(type , "feather"))the_ds <- feather_file(name , ds_params , controller)
   if(identical(type , "built_in")) the_ds <- built_in_file(name , ds_params , controller)

  if(is.null(the_ds))  cli::cli_alert_danger("Dataset load for type {type} not implemented")

  size <- nrow(controller$master_data)
  row <- new_row(sr_num = size +1 , ds = the_ds , ds_name = name , ds_params = ds_params)
  row

}


csv_file <- function(name , ds_params , controller){
   ds <- vroom::vroom(ds_params$connection)
   if(is.defined(ds_params$subset)){
     ds <- dplyr::sample_n(ds , as.integer(ds_params$subset))
   }
   cli::cli_alert_success(" csv ds {name} loaded ")
   ds
}

csv_dir <- function(name , ds_params , controller){
  cli::cli_alert_danger(" csv_dir is  not implemented ds :{name} ")
}

rds_file <- function(name , ds_params , controller){
  ds <- readr::read_rds(ds_params$connection)
  if(is.defined(ds_params$subset)){
    ds <- dplyr::sample_n(ds , as.integer(ds_params$subset))
  }
  cli::cli_alert_success(" rds ds {name} loaded ")
  ds
}


arrow_file <- function(name , ds_params , controller){
  arrow::read_arrow(ds_params$connection) #TODO : check if we can load arrow subset in arrow api
  if(is.defined(ds_params$subset)){
    ds <- dplyr::sample_n(ds , as.integer(ds_params$subset))
  }

  cli::cli_alert_success(" arrow ds {name} loaded ")
  ds

}

feather_file <-  function(name , ds_params , controller){
  ds <- arrow::read_feather(ds_params$connection) #TODO : check if we can load arrow subset in arrow api

  if(is.defined(ds_params$subset)){
    ds <- dplyr::sample_n(ds , as.integer(ds_params$subset))
  }
  cli::cli_alert_success(" feather ds {name} loaded ")
  ds
}


built_in_file <- function(name , ds_params , controller){
  ds <- load_built_ts_as_tibble(ds_name = ds_params$connection)
  if(is.defined(ds_params$subset)){
    ds <- dplyr::sample_n(ds , as.integer(ds_params$subset))
  }
  cli::cli_alert_success(" built_in ds {name} loaded ")
  ds
}

is.defined = function(x)!is.null(x)

