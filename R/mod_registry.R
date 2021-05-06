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
    #' @field registry
    registry = NULL,

    initialize = function(params) {
      r <- stringr::str_detect(names(params), "\\D[.]\\D")
      l <- names(params[which(r)])
      sp <- stringr::str_split(string = l, pattern = "[.]")
    #  all_mods <- sapply(sp, function(x) {
    #    unlist(x)[1]
    #  })
    # all_mods <- unique(all_mods)

    # Using only mods that have mod_name as a property
      registry_mods <- sapply(sp, function(x){
         pvalue <- x[2]
         pname <- x[1]
         ret <- NULL
         if(pvalue == "mod_name")
            ret <- pname
      })

      registry_mods <- unlist(unique(registry_mods))
      registry_filename <- system.file("mod_registry/mod_registry.csv" , package = "sweetmods")
      r <- readr::read_csv(registry_filename)
      mparams <- masterparams_to_mod_params(master_params = params ,
                                                     registry_df = r  ,
                                                     mod_names = registry_mods )

      self$registry <- r
      self$mod_names <- registry_mods
      self$master_params <- params
      self$mod_params <- mparams
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

    #`Validate the Config File
    validate_params = function(){

    },

    print = function(...) {
      cli::cli_ol("Mod Names")
      sapply(self$mod_names, function(x) {
        cli::cli_li(" {x}")
      })
      invisible(self)
    }
))
