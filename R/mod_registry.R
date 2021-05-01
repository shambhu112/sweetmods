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
    #' @field mod_params initialization parameters
    mod_params = NULL,
    #' @field registry_file
   registry_filename = NULL,
    #' @field registry
    registry = NULL,

    initialize = function(params) {
      r <- stringr::str_detect(names(params), "\\D[.]\\D")
      l <- names(params[which(r)])
      sp <- stringr::str_split(string = l, pattern = "[.]")
      mods <- sapply(sp, function(x) {
        unlist(x)[1]
      })

      self$registry_filename <- system.file("mod_registry/mod_registry.csv" , package = "sweetmods")
      self$mod_names <- unique(mods)
      self$master_params <- params
      self$mod_params <- masterprams_to_mod_params(master_params = params ,
                                              registry_file = self$registry_filename  ,
                                              mod_names = self$mod_names )
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
      index <-which(names(self$mod_params) == mod_name)
      self$mod_params[[index]]
    },

    print = function(...) {
      cli::cli_ol("Mod Names")
      sapply(self$mod_names, function(x) {
        cli::cli_li(" {x}")
      })
      invisible(self)
    }
))
