## c:/Dropbox/Rpackages/CLmisc/R/pipe_test.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-06-11

#' Test in a \code{magrittr} pipe
#'
#' To run a test in \code{magrittr} pipeline
#'
#' @param obj An \code{R} object. When used in a \code{magrittr}, this
#'   will be omitted. This will also be returned
#' @param test The test
#' @param value the value that the test should be equal to
#' @param tolereance The tolerance for equality. Set to 0 exact
#'   (identical) equality
#'
#' @return \code{obj} -- the original first parameter passed to
#'   \code{pipe_test}
#'
#' @examples
#' data(mtcars)
#' mtcars.DT <- as.data.table(mtcars)
#' mtcars.DT %>%
#'  .[, cyl := as.character(cyl)] %>%
#'  pipe_test(test = class(.[["cyl"]]), value = "numeric")
#'
#' @export
pipe_test <- function(obj, test, value, tolerance = NULL) {

  if (is.null(tolerance)) {
    ##No tolerance
    out <- all.equal(value, test)
  } else {
    out <- all.equal(value, test, tolerance = tolerance)
  }

  if (out != TRUE) {
    ##print a helpful error message
    stop(sprintf("\nError: %s \nTest: %s\nvalue: %s",
                 out,
                 deparse(substitute(test)),
                 deparse(substitute(value))
                 ))
  } else {
    ##No errors, return the original object
    return(obj)
  }
}
