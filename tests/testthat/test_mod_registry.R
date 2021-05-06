library(sweetmods)
library(shiny)
library(testthat)

params <- config::get(file = "tests/testthat/config.yml")

test_that("initiatialization tests" , {
  reg <- mod_registry$new(params = params)
  reg$print()
  expect_true("core_mod" %in% reg$mod_names)
})

test_that("initiatialization tests" , {
  reg <- mod_registry$new(params = params)
  p <- reg$params_for_mod("core_mod")
  expect_true("ui_function" %in% names(p))
})

test_that("mod config" , {
  reg <- mod_registry$new(params = params)
  names <- reg$mod_names
  sapply(names, function(x){
    p <- reg$params_for_mod(x)
    expect_true("ui_function" %in% names(p))
  })

})

test_that("ymlon_to_params tests" , {
  params <- config::get(file = "tests/testthat/for_ymltest.yml")
  p <- ymlon_to_params(obj_name = "core_mod" ,master_params = params)
  expect_true("mod_name" %in% names(p))
})

test_that("masterparams_to_mod_params tests" , {
  params <- config::get(file = "tests/testthat/for_ymltest.yml")
  registry_filename <- system.file("mod_registry/mod_registry.csv" , package = "sweetmods")
  registry <- readr::read_csv(registry_filename)

  p <- masterparams_to_mod_params(master_params = params , registry_df = registry ,
                                  mod_names = c("core_mod" , "intro_mod" , "explore_tab"))

  expect_false("bank_mod" %in% names(p))
})


test_that("appr initial scripot " , {
  params <- config::get(file = "config.yml") ## @@sweetmod_config
  controller <- app_master$new(params)
  controller$preload_master_with_config()
  registry <- sweetmods::mod_registry$new(params)
  mod_names <- registry$mods_names()

  expect_true("core_mod" %in% mod_names)
})

