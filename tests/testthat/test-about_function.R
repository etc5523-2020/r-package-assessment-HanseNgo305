library(shiny)

context("testing the function to import references")

test_that("The reference presented", {
  # Expect that the number of link generated match the number of input elements
  # link and packages are mapped into one data frame so link will have to duplicate to match the rows of packages
  # that's why we need unique here to avoid duplication
  expect_equal(length(unique(output_ref("link"))),length(unique(ref$link)))
  # Expect that the number of packages generated match the number of input elements
  expect_equal(length(unique(output_ref("packages"))),length(unique(ref$packages)))
  # Expect the type is supplied
  expect_error(output_ref(NULL))
  expect_error(output_ref(""))
  # Expect the types only falls in the "packages" and "link"
  expect_error(output_ref("links"))
})
