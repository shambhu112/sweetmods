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
