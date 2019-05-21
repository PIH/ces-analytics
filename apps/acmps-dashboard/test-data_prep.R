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


test_that("CountNonNA doesn't count rows with NA values",{
  input <- c(NA, NA, NA, 1, 1, 0) 
  res <- CountNonNA(input)
  expect_equal(res, 3)
})


### Test Chronics Functions

# Percent Control 
# Numerator = number with information in "form.control_disease" column. 
# Denominator = number that have any information in "form.control_disease" column

test_that("PercentControl calculates the percent of patients in control", {
  input <- tibble(form.control_diabetes = c(1, 1, 0, 0, NA), form.mes = c(1, 1, 1, 1, 1), 
                  does_the_patient_have_diabetes = c(1, 1, 1, 1, 0))
  res <- PercentControl(input, form.control_diabetes, by = form.mes)
  expect_equal(res, 50)
})

 
 
 
    # NumberControl
    # NumberNotControl
    # NumberVisits
    # NumberVisits
    # PercentHojaVisita
    # PercentControlInfo



### Test ACMPS Functions 


# PercentPatientSatisfaction
# AveragePatientSatisfaction
#  PercentAttendance
# PercentMentoria
#  AverageMentoria




