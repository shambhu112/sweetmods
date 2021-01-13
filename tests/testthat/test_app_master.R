library(sweetmods)
library(here)
library(shiny)

params <- config::get(file = "tests/testthat/config.yml")

test_that("initiatialization tests" , {
  master <- app_master$new(params = params)
  expect_true(is.reactivevalues(master$rvals) )
})


test_that("preload with dataset" , {
  master <- app_master$new(params = params)
  master$preload_master_with_config()
})
