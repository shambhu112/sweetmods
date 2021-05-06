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
#' Push Error Message
#' @param message the error message in glue format
#' @export
m_err <- function(message){
  cli::cli_alert_danger(message)
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

#' load a builtin datasource as tibble
#' @param  ds_name
#' @return data frame for the built in dataset
#' @export
load_built_ts_as_tibble <- function(ds_name){
  txt <- paste0("a <- data.frame(" , ds_name , ")")
  eval(parse(text = txt ))
  a
}


#' load a target and put it into a tibble.
#' Note : we clear up the tar_load value before reutrnignt he dataframe to free up the memory
#' @export
load_tar_as_tibble <- function(tar_name){
  ds <- NULL
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
#' @param file name file path to load
#'
#' @export
read_file <- function(fname){
  x <- fname
  nm <- tolower(x)
  if(endsWith(nm , ".csv"))
    f <- vroom::vroom(x)
  else if(endsWith(nm , ".feather"))
    f <- arrow::read_feather(x)
  else if(endsWith(nm , ".rds"))
    f <- readr::read_rds(x)      #TODO Write Test
  else if(endsWith(nm , ".xls"))
    f <- readxl::read_xls(x)  #TODO Write Test
  else if(endsWith(nm , ".xlsx"))
    f <- readxl::read_xlsx(x)  #TODO Write Test
  else
    stop(glue::glue("Unknown file extension in read {x}"))
  f
}
#  Converts Mod References in MasterParams to mod params
#' @param master_params the master_params
#' @param registry_df the registry df
#' @param mod_names the mod names
#' @return list with new mod params
masterparams_to_mod_params <- function(master_params , registry_df , mod_names){
  params <- master_params
  mi <- sapply(mod_names, function(x){
    ymlon_to_params(x , params)
  })


  mi2 <- sapply(1:length(mi), function(x){

    mname <- mi[[x]]$mod_name
    if(is.null(mname))
      return(mi[[x]])
    mod_name <- NULL # TODO : this is done to avoid a note in r package check
    props <- dplyr::filter(registry_df , mod_name == mname)
    pnames <- unlist(list(props$property , names(mi[[x]])) )
    pnames <- unique(pnames)

    xv <- sapply(pnames, function(xx){
      v <- props[props$property == xx & props$mandatory == "TRUE",]$value
      index <- which(names(mi[[x]]) == xx)
      if(length(index) >0 )
        v <- as.character(mi[[x]][index])
      v
    })
    xv <- as.list(xv)
    xv
  })
  names(mi2) <- names(mi)
  mi2

}


#  Converts YML object notation to config. i.e converts mod_name.param: 5 to param:5 from a master yml file
#' used to get sub params for a given mod_name
#' @param obj_name the mod_name
#' @param master_params the master param
#' @return list of params
ymlon_to_params <- function(obj_name ,master_params){
  pre <- paste0(obj_name, ".\\D")
  r <- stringr::str_detect(names(master_params), pattern = pre)
  l <- names(master_params[which(r)])
  sp <- stringr::str_split(string = l, pattern = "[.]")
  sub_params <- sapply(sp, function(x) {
    unlist(x)[2]
  })
  values <- master_params[which(r)]
  ret <- vector(mode = "list", length = length(sub_params))
  ret <- values
  names(ret) <- sub_params
  ret
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





