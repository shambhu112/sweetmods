
confirm_boolean <- function(){
  x <- readline(" >>   Yes[1], No[2]: (enter 1 or 2) :  ")
  x <- as.numeric(unlist(strsplit(x, ",")))
  r <- FALSE
  if(x == 1 ){
    r <- TRUE
  }
  return(r)
}

create_ss_targets <- function(filename , overwrite = FALSE ){
  if(overwrite){
    cli::cli_alert_info(" Overwrite file {filename}")
  }else{
      if(file.exists(filename)){
        t_file <- "ss_targets_sample.txt"
      }else{
        t_file <- filename
      }
      cli::cli_alert_info(" Writing file {t_file}")
  }
}


if(file.exists("_targets.R")){
  cli::cli_alert_warning("_targets.R already exists. Overwrite (yes)? or create a sample .txt file (no)? ")
  overwrite <- confirm_boolean()
  create_ss_targets(filename ="_targets.R" , overwrite = overwrite )
}else{
  create_ss_targets("_targets.R")
}





