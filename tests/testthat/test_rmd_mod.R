library(sweetmods)
library(testthat)
library(shiny)

params <- config::get(file = "tests/testthat/config.yml")

test_that("test rmd  ", {
  reg <- shinyspring::mod_registry$new(params)


})
