library("testthat")
source("data_prep.R")

test_that("CountTrue counts the TRUE values", {
  input <- c(TRUE, FALSE, TRUE, NA)
  res <- CountTrue(input)
  expect_equal(res, 2)
})

test_that("FilterByDate drops rows with NA dates", {
  input <- tibble(
    Date = as.Date(c("2018-01-03", NA, NA))
  )
  res <- FilterByDate(input, as.Date("2018-01-01"), as.Date("2018-01-05"))
  expect_equal(res, data.frame(Date = as.Date("2018-01-03")))
})

