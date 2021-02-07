library(sweetmods)
library(here)
library(shiny)

params <- config::get(file = "tests/testthat/config.yml")

test_that("initiatialization tests" , {
  master <- app_master$new(params = params)
  expect_true(is.reactivevalues(master$rvals) )
})


test_that("preload reactive with dataset" , {
  master <- app_master$new(params = params)
  master$preload_master_with_config()
})


test_that("preload nrx with dataset" , {
  master <- app_master$new(params = params)
  df <- read.csv(file = "CHART4_NM.csv")
  row <- create_row(1 , "charts_file.csv" , "new_mexico" , df , "csv")
  master$preload_master_nrxdata(nrx_data = row)
  names <- master$dataset_names_nrx()
  testthat::expect_equal(length(names) , 1)
  testthat::expect_equal(names , "new_mexico")

  df <- master$data_by_name_nrx("new_mexico")
  testthat::expect_type(df , "list")

  df <- master$data_by_index_nrx(1)
  testthat::expect_equal(nrow(df) , 241)

})
