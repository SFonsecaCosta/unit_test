if (!requireNamespace(c("testthat","here"), quietly=TRUE))
  install.packages(c("testthat","here"))
library(testthat)
library(here)

## call the libraries and then,
## to run the tests just make: test_dir(here("tests/testthat"), reporter=c("summary", "location"))
here()
source(here("R","popmean.R"))
rawData <- (here("data","data.tsv"))

dataInfo <- read.table(rawData, header = TRUE, sep = "\t")

test_that('Correct dimension and no missing values in the raw data', {
  expect_equal(ncol(dataInfo), 7)
  expect_identical(dataInfo, na.omit(dataInfo))
})

test_that('Check column names of the file', {
  expect_equal(colnames(dataInfo),c("zone","population","year","none","IRS","ITN","IRS.ITN"))
})

test_that('Correct data types', {
  expect_is(dataInfo,'data.frame')
  expect_is(dataInfo$zone, 'character')
  expect_is(dataInfo$population, 'integer')
  expect_is(dataInfo$year, 'integer')
})

test_that("Test error in argument list", {
  expect_that( popmean(data = rawData, intervention = c("none", "IRS", "ITN", "IRS.ITN")), throws_error() )
  
  expect_that( popmean(data = rawData, intervention = , years = 2020), throws_error() )
})

test_that("Test correct function results", {
  all.equal(popmean(data = rawData, intervention = "none", years = 2020), 0.07723)
  
  all.equal(popmean(data = rawData, intervention = c("none", "IRS", "ITN", "IRS.ITN"), years = 2020:2022), c(0.07723,0.11085,0.15447,
                                                                                                             0.07723, 0.06723,0.05723,
                                                                                                             0.06042,0.06723, 0.08404,
                                                                                                             0.06042, 0.05042, 0.04042))
})
