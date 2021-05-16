## ./test_clean_readxl_backslashes.R

context("clean_readxl_backslashes")

test_that("clean_readxl_backslashes cleans \\n", {
  expect_equal(clean_readxl_backslashes("\\n"), "\n")
})


test_that("clean_readxl_backslashes cleans \\u00ad", {
  expect_equal(clean_readxl_backslashes("\\u00ad"), "\u00ad")
})

test_that("clean_readxl_backslashes cleans \\u2013", {
  expect_equal(clean_readxl_backslashes("\\u2013"), "\u2013")
})


test_that("clean_readxl_backslashes cleans \\u2014", {
  expect_equal(clean_readxl_backslashes("\\u2014"), "\u2014")
})





