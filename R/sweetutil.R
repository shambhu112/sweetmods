#' Parse preload files named in params (config).
#'
#' In config file the attribute file_preloads will have a value like
#' file1 ; /home/file2 . This function will parse this into the file list
#' @param value the string that needs to be parsed
#' @param sep the seperator , default is ;
#' @export
parse_preloads_in_config <- function(value , sep = ";") {
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
#' @export
create_row <- function(srnum , filename , ds_name , ds ){
  colnames(ds) <- make.names(snakecase::to_any_case(colnames(ds)))
  row <- tibble::tibble(
    "srnum" = srnum,
    "filenames" = filename,
    "dataset_names" = ds_name,
    "datasets" = tidyr::nest(ds , data = everything()) ,
    "original_cols" = list(cname = colnames(ds)),
    "snake_cols" = list(sname = make.names(snakecase::to_any_case(colnames(ds))))
  )
  row
}
