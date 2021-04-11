#' Parse preload files named in params (config).
#'
#' In config file the attribute file_preloads will have a value like
#' file1 ; /home/file2 . This function will parse this into the file list
#' @param value the string that needs to be parsed
#' @param sep the seperator , default is ;
#' @export
parse_preloads_in_config <- function(value , sep = ";") {
  stopifnot(!is.null(value))
  nms <- strsplit(value , sep )
  nms <-  sapply(nms , function(x) {trimws(x , "both") })
  nms
}



#' creates the standardized row for app_master mdata
#'
#' create a row that goes into mdata of reactiveValues(rvals)
#' @importFrom tibble tibble
#' @importFrom tidyselect everything
#' @param srnum the serial number for the row. Index
#' @param filename filename including path
#' @param ds_name the unique name for the dataset you wand to refer it with to access it in the application
#' @param ds the data.frame or tibble
#' @param format the type of format csv , rds , parquet , tsc
#' @export
create_row <- function(srnum , filename , ds_name , ds , format , pretty_cols = NULL){

  if(is.null(pretty_cols))
    pretty_cols <- make.names(snakecase::to_any_case(colnames(ds)))

  row <- tibble::tibble(
    "srnum" = srnum,
    "connection_str" = filename,
    "dataset_names" = ds_name,
    "datasets" = tidyr::nest(ds , data = everything()) ,
    "original_cols" = list(cname = colnames(ds)),
    "pretty_cols" = list(pnames = pretty_cols),
    "connection_type" = format
  )
  row
}



#' load a target and put it into a tibble.
#' Note : we clear up the tar_load value before reutrnignt he dataframe to free up the memory
#' @export

load_tar_as_tibble <- function(tar_name){
  targets::tar_load(tar_name)
  txt <- paste("ds <-  tibble::as_tibble(" , tar_name , ")" , sep = "")
  eval(parse(text =txt))
  txt <- paste( tar_name , " <- NULL " , sep = "")
  eval(parse(text =txt))
  ds
}


#' file reader
#'
#' read varios types of file based on extension
#' @param files list of file paths to load
#'
#' @export

read_files <- function(files){
 loaded <- sapply(files, function(x){
      nm <- tolower(x)
      if(endsWith(nm , ".csv"))
        f <- vroom::vroom(x)
      else if(end(nm , ".feather"))
        f <- arrow::read_feather(x)
      else if(endsWith(nm , ".rds"))
        f <- readRDS(x)
      else if(endsWith(nm , ".xls"))
        f <- readxl::read_xls(x)
      else if(endsWith(nm , ".xlsx"))
        f <- readxl::read_xlsx(x)
      else
        stop(glue::glue("Unknown file extension in read {x}"))

  })
  loaded
}


#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}





