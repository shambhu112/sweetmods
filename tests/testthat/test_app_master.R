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
  ds_names <- master$dataset_names()
  expect_true("mexico" %in% ds_names)

  newdata <- mtcars
  master$replace_dataset_by_name(dataset_name =  "mexico" , replace_with = newdata)
  df <- master$dataset_by_name("mexico")
  expect_true("mpg" %in% colnames(df))
})


test_that("remove dataset" , {
  master <- app_master$new(params = params)
  master$preload_master_with_config()
  ds_names <- master$dataset_names()
  expect_true("misssouri" %in% ds_names)
  master$remove_dataset(index = 2)
  expect_equal(2 , nrow(master$master_data))
})




test_that("replace dataset" , {
  master <- app_master$new(params = params)
  master$preload_master_with_config()
  ds_names <- master$dataset_names()
  expect_true("mexico" %in% ds_names)

  mexico <- master$dataset_by_name("mexico")
  expect_equal(241 , nrow(mexico))
  expect_true("Clean.Plaintiff" %in% colnames(mexico))

})


test_that("pretty names" , {
  master <- app_master$new(params = params)
  master$preload_master_with_config()
  ds_names <- master$dataset_names()
  expect_true("mexico" %in% ds_names)

  cnames <- master$colnames_for_dataset("mexico")
  expect_true("Clean Plaintiff" %in% cnames)


  pnames <- master$prettynames_for_dataset("mexico")
  expect_true("clean_plaintiff" %in% pnames)

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

test_that(" Test str_detect condition for config object notation" , {
  t <- c("abc" , "abc_t" , "abc.t" , "abc." , ".abc" , "anc.t.y")
  r <- stringr::str_detect(t , "\\D[.]\\D")
  rcheck <- c(FALSE, FALSE ,  TRUE , FALSE ,  FALSE ,  TRUE)
  testthat::expect_equal(sum(rcheck == r) , 6)
})

