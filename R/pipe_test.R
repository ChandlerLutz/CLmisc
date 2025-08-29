## c:/Dropbox/Rpackages/CLmisc/R/pipe_test.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-06-11

#' Pipe-Friendly Assertion Function
#'
#' @description
#' This function is designed to be used within a `magrittr` (`%>%`) pipeline
#' to perform a check or test on data. If the check passes, it invisibly
#' returns the original object, allowing the pipeline to continue. If the
#' check fails, it throws an informative error and stops execution.
#'
#' @details
#' The function is highly efficient for large objects like `data.table`s because
#' it does not create a copy of the input object (`obj`). It provides clear
#' error messages by deparsing the expression that failed, making debugging easier.
#'
#' It operates in two modes:
#' 1.  **Logical Assertion**: If `expected` is `TRUE` (the default), the function
#'     evaluates `all(check)`. This is useful for verifying conditions like
#'     `all(my_col > 0)`.
#' 2.  **Object Comparison**: If `expected` is any other value, the function uses
#'     `all.equal(check, expected)` to compare the two objects, which is ideal
#'     for checking if a calculation produced a specific numeric vector or object.
#'
#' @param obj The object being passed through the pipe, typically a `data.frame` or `data.table`.
#' @param check The expression to test. For logical tests, this is an expression that
#'   resolves to a logical vector. For object comparisons, this is the object to be
#'   tested against `expected`.
#' @param expected The expected value for the `check`. Defaults to `TRUE`.
#' @param tolerance Numeric tolerance for `all.equal()` comparisons. Ignored when
#'   `expected = TRUE`.
#'
#' @return Invisibly returns the original input object `obj` if the check is successful.
#'
#' @seealso `all.equal()`, the `assertr` package.
#'
#' @export
#'
#' @examples
#' if (require("data.table") && require("magrittr")) {
#'
#'   DT <- data.table(x = 1:10, y = (1:10) * 2)
#'
#'   # --- Successful Checks ---
#'
#'   # 1. A successful logical check
#'   DT_new <- DT[, z := x + y] %>%
#'     pipe_check(check = all(.$z == .$x * 3)) %>%
#'     .[, z_sqrt := sqrt(z)]
#'
#'   print(DT_new)
#'
#'   # 2. A successful object comparison check
#'   DT %>%
#'     pipe_check(check = .$y, expected = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
#'
#'
#'   # --- Failing Check ---
#'
#'   # 3. A failing check, wrapped in try() to show the error message
#'   try({
#'     DT[, z := x + y] %>%
#'       pipe_check(check = all(.$z < 20))
#'   })
#' }
pipe_check <- function(obj, check, expected = TRUE,
                       tolerance = .Machine$double.eps^0.5) {
  
  # For simple logical checks where the user supplies an expression
  if (isTRUE(expected)) {
    # all() returns TRUE or FALSE, so we can check it directly
    result <- all(check)
    if (!isTRUE(result)) {
      # Provide a more specific error for this common case
      stop(sprintf("\nError: Assertion failed (result is not TRUE).\nCheck: %s",
                   deparse(substitute(check))))
    }
  } else {
    # For comparing two objects using all.equal
    result <- all.equal(check, expected, tolerance = tolerance)
    if (!isTRUE(result)) {
      stop(sprintf("\nError: %s\nCheck: %s\nExpected: %s",
                   result,
                   deparse(substitute(check)),
                   deparse(substitute(expected))))
    }
  }
  
  # Return the original object to continue the pipe
  invisible(obj)
}

#' Test in a \code{magrittr} pipe
#'
#' To run a test in \code{magrittr} pipeline
#'
#' @param obj An \code{R} object. When used in a \code{magrittr}, this
#'   will be omitted. This will also be returned
#' @param test The test
#' @param value the value that the test should be equal to
#' @param tolerance The tolerance for equality. Set to 0 exact
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
#'  pipe_test(test = class(.[["cyl"]]), value = "character")
#' ## Or
#' mtcars.DT %>%
#'  .[, cyl := as.character(cyl)] %>%
#'  pipe_test(class(.[["cyl"]]) == "character")
#' 
#'
#' @export
pipe_test <- function(obj, test, value = TRUE, tolerance = NULL) {

  warning("pipe_test() is deprecated. Please use pipe_check() instead.", call. = FALSE)

  if (value == TRUE & is.null(tolerance)) {
    out <- all(test)
  } else if (is.null(tolerance)) {
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
