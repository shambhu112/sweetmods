library(sweetmods)
library(here)
library(shiny)

params <- config::get(file = "tests/testthat/config.yml")

test_that("initiatialization tests" , {
  master <- app_master$new(params = params)
  expect_false(is.reactivevalues(master$rvals) )
})


test_that("preloadwith dataset" , {
  master <- app_master$new(params = params)
  master$preload_master_with_config()
})

test_that("sweet util read file" , {
  params <- config::get(file = "tests/testthat/config.yml")
  files <- parse_preloads_in_config(value = params$file_preloads , sep = ";")
  f <- read_files(files)
  nm <- names(f)
  # check CSV file
  f1 <- as.data.frame(f[[nm[1]]])
  expect_equal(nrow(f1) , 241)
  #check feather file
  f3 <- as.data.frame(f[[nm[3]]])
  expect_equal(nrow(f3) , 201)
  cn <- c("branch_physical_key")

  # check is colname creation is good
  expect_true( cn %in% colnames(f3))

})

test_that("preload with reactive data" , {
  master <- app_master$new(params = params)

  df <- read.csv(file = "CHART4_NM.csv")
  row <- create_row(1 , "charts_file.csv" , "new_mexico" , df , "csv")
  master$reactive_vals$mexico_data <- mtcars

})
