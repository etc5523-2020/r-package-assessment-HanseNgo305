library(lubridate)

context("testing the function to read csv file")

test_that("test if the csv file read is correct", {
  # Expect that the read file function return values not NULL
  expect_true(!is.null(patient_link))
  expect_true(!is.null(patient_node))
  expect_true(!is.null(vietnam_daily))
  # Must return error if files are not found
  expect_error(readData("testdata"))
})
