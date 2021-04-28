#' R6  class for Modules Management for the shinyspring app
#'
#' @description
#' mod_registry is the call you cn
#'
#' @name mod_registry
#' @importFrom R6 R6Class
#' @export
mod_registry <- R6::R6Class(
  "mod_registry",
  public = list(
    #' @field mod_names initialization parameters
    mod_names = NULL,
    #' @field master_params initialization parameters
    master_params = NULL,
    initialize = function(params) {
      r <- stringr::str_detect(names(params), "\\D[.]\\D")
      l <- names(params[which(r)])
      sp <- stringr::str_split(string = l, pattern = "[.]")
      mods <- sapply(sp, function(x) {
        unlist(x)[1]
      })
      self$mod_names <- unique(mods)
      self$master_params <- params
  },

    #' get mod names from config files
    #' @return characted list of mod_names
    mods_names = function() {
      self$mod_names
    },

    #TODO fix this to be cached .
    #' get sub params for a given mod_name
    #' @param mod_name the mod_name
    #' @return list ofparams
    params_for_mod = function(mod_name) {
      master_params <- self$master_params
      pre <- paste0(mod_name, ".\\D")
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
    },

    print = function(...) {
      cli::cli_ol("Mod Names")
      sapply(self$mod_names, function(x) {
        cli::cli_li(" {x}")
      })
      invisible(self)
    }
))
