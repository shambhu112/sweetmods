library(sweetmods)
library(shiny)
library(testthat)

params <- config::get(file = "tests/testthat/config.yml")

test_that("test masterparams_to_mod_params" , {

  params <- config::get(file = "tests/testthat/mod_params.yml")

  #registry_mods <- list("core_mod" , "explore_tab" , "intro_mod")
  registry_mods <- list("core_mod" , "explore_tab" , "intro_mod")
  registry_filename <- system.file("mod_registry/mod_registry.csv" , package = "sweetmods")
  registry_df <- suppressMessages(readr::read_csv(registry_filename ))
  master_params <- params
  mod_names <- registry_mods

  mparams <- masterparams_to_mod_params(master_params ,
                                        registry_df  ,
                                        mod_names  )


   expect_true("dummy_mod" == mparams$core_mod$mod_name)

})
