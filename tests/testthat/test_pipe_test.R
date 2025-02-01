## c:/Dropbox/Rpackages/CLmisc/tests/testthat/test_pipe_test.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-06-17

library(CLmisc)

data(mtcars)

mtcars.DT <- as.data.table(mtcars)
mtcars.DT <- mtcars.DT[, cyl := as.character(cyl)]

test_that("pipe_test() works data.table", {
  expect_equal(mtcars.DT %>%
                 pipe_test(test = class(.[["cyl"]]), value = "character"),
               mtcars.DT)
})
