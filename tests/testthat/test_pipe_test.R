## c:/Dropbox/Rpackages/CLmisc/tests/testthat/test_pipe_test.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-06-17

library(CLmisc)

data(mtcars)

mtcars.DT <- as.data.table(mtcars)
mtcars.DT <- mtcars.DT[, cyl := as.character(cyl)]

test_that("pipe_test() works data.table", {
  expect_warning(mtcars.DT %>%
                   pipe_test(test = class(.[["cyl"]]), value = "character"))
})

context("Testing pipe_check() function")

test_that("Logical assertions work correctly", {
  # SUCCESS: This check should pass silently and return the data.table
  result <- mtcars.DT %>%
    pipe_check(check = all(.$mpg > 0)) %>%
    pipe_check(check = nrow(.) == 32)

  # Check that the returned object is the same as the input
  expect_identical(result, mtcars.DT)

  # FAILURE: This check should fail and throw an error
  expect_error(
    mtcars.DT %>% pipe_check(check = all(.$cyl == 4)),
    "Error: Assertion failed"
  )

  # Check that the error message contains the deparsed code
  expect_error(
    mtcars.DT %>% pipe_check(check = all(.$hp > 200))
  )
})


test_that("Object comparisons work correctly", {
  # SUCCESS: Check if the 'cyl' column is as expected
  expected_cyl <- mtcars.DT$cyl
  result <- mtcars.DT %>%
    pipe_check(check = .$cyl, expected = expected_cyl)

  expect_identical(result, mtcars.DT)

  # FAILURE: Provide an incorrect expected object
  incorrect_cyl <- c(4, 6, 8)
  expect_error(
    mtcars.DT %>% pipe_check(check = .$cyl, expected = incorrect_cyl)    
  )
})


test_that("Numerical tolerance comparisons work correctly", {
  # Create a column with a small floating point deviation
  mtcars.DT[, mpg_plus := mpg + 1e-7]

  # SUCCESS: The difference is within the default tolerance
  result <- mtcars.DT %>%
    pipe_check(check = .$mpg_plus, expected = .$mpg)

  expect_identical(result, mtcars.DT)

  # FAILURE: The difference is outside the specified (tighter) tolerance
  expect_error(
    mtcars.DT %>%
      pipe_check(check = .$mpg_plus, expected = .$mpg, tolerance = 1e-9)
  )

  # Clean up the added column
  mtcars.DT[, mpg_plus := NULL]
})


test_that("Function returns the original object unmodified", {

  data(mtcars)
  mtcars.DT <- as.data.table(mtcars)
  # Perform a simple check
  result <- mtcars.DT %>%
    pipe_check(check = is.data.table(.))

  # Test 1: The returned object is identical to the original
  expect_identical(result, mtcars.DT)

  # Test 2: The class of the object is preserved
  expect_s3_class(result, "data.table")

  # Test 3: The number of rows and columns are unchanged
  expect_equal(nrow(result), 32)
  expect_equal(ncol(result), 11)
})
