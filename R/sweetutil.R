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

#' Push Info Message
#' @param message the error message in glue format
#' @export

m_info <- function(message){
  cli::cli_alert_info(message)
}

#' Push Success Message
#' @param message the error message in glue format
#' @export

m_success <- function(message){
  cli::cli_alert_success(message)
}


#' creates new dq_master row
#'
#' @param srnum the serial number for the row. Index
#' @param ds_name the unique name for the dataset you wand to refer it with to access it in the application
#' @param ds the dataset
#' @return row the row
#' @export
new_dqmaster_row <- function(sr_num ,ds_name , ds){
  eda1 <- SmartEDA::ExpData(ds , type = 1)
  eda2 <- SmartEDA::ExpData(ds , type = 2)
  row <-tibble::tibble(
    "srnum" = sr_num ,
    "ds_name" = ds_name ,
    "dq_summary" = tidyr::nest(eda1 , data = dplyr::everything()) ,
    "dq_detail" =  tidyr::nest(eda2 , data = dplyr::everything())
  )
  row
}


#' creates the standardized row for app_master mdata
#'
#' create a row that goes into mdata of reactiveValues(rvals)
#' @param srnum the serial number for the row. Index
#' @param ds the dataset
#' @param ds_name the unique name for the dataset you wand to refer it with to access it in the application
#' @param ds_params the config file params for file
#' @return row the row
#' @export
new_row <- function(sr_num , ds , ds_name ,ds_params){
      if(is.null(ds_params$pretty_names)){
            pretty_nms <- snakecase::to_snake_case(colnames(ds))
      } else{
          pretty_nms <- ds_params$pretty_names #TODO: this needs to tested if it works
    }
   row <- tibble::tibble(
      "srnum" = sr_num,
      "connection_str" = ds_params$connection,
      "dataset_names" = ds_name,
      "datasets" = tidyr::nest(ds , data = dplyr::everything()) ,
      "original_cols" = list(cname = colnames(ds)),
      "pretty_cols" = list(pnames = pretty_nms),
      "connection_type" = ds_params$type ,
      "row_count" = nrow(ds) ,
      "col_count" = ncol(ds) ,
   #   "memory_size" = pryr::object_size(ds),
      "dq_summary" = NULL  ,
      "dq_detail" = NULL
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
#' @param tar_name the name of the target
#' @param  raw_mode (optional) if set in params tar_raw_mode = TRUE then tar_read is used instead of tar_load
#' @export
load_tar_as_tibble <- function(tar_name , raw_mode = FALSE){
  ds <- NULL
  if(raw_mode){
    ds <- targets::tar_read_raw(tar_name )
  } else {
    targets::tar_load(tar_name)
    txt <- paste("ds <-  tibble::as_tibble(" , tar_name , ")" , sep = "")
    eval(parse(text =txt))
    txt <- paste( tar_name , " <- NULL " , sep = "")
    eval(parse(text =txt))
  }
  ds
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
  names(mi) <- mod_names

  mi2 <- sapply(1:length(mi), function(x){

    mname <- mi[[x]]$mod_name
    if(is.null(mname))
      return(mi[[x]])
    mod_name <- NULL # TODO : this is done to avoid a note in r package check
    props <- dplyr::filter(registry_df , mod_name == mname)
    pnames <- unlist(list(props$property , names(mi[[x]])) )
    pnames <- unique(pnames)

    xv <- sapply(pnames, function(xx){
      v <- props[props$property == xx & props$category == "package_defined",]$value
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

lazy_update_dq_row <- function(ds_name , control , max_rows = Inf){

  if(is.null(control$dq_master)){
    row <- new_dqmaster_row(sr_num = 1 , ds_name = ds_name , ds = control$dataset_by_name(ds_name , max_rows))
    control$dq_master <- row
    cli::cli_alert_success("dq for {ds_name} created : Index : 1")
  }
  the_row <- control$dq_master[control$dq_master$ds_name == ds_name,]
  if(nrow(the_row) == 0){
    index <- nrow(control$dq_master) + 1
    row <- new_dqmaster_row(sr_num = index ,
                            ds_name = ds_name ,
                            ds = control$dataset_by_name(ds_name , max_rows ))
    control$dq_master <- dplyr::bind_rows(control$dq_master , row)
    cli::cli_alert_success("dq for {ds_name} created : Index : {index}")
    the_row <- row
  }
  the_row
}


#TODO: Note there is a bug in this method when we have a case like
# intro_tab.mod_name: dummy_mod
# core_tab.mod_name: dummy_mod
# For this method to function properly there needs to be at-least one different type of param


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
#  ret <- vector(mode = "list", length = length(sub_params))
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





