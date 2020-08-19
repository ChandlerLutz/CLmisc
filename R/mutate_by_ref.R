## c:/Dropbox/Rpackages/CLmisc/R/mutate_by_ref.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2020-08-17

#' Mutate multiple cols by reference using the same function
#'
#' @param DT a \code{data.table}
#' @param cols a character vector of column names or a function that
#'   returns a logical vector to choose the column names
#' @param f a function that will be used to mutate the columns
#' @param names.extra if \code{NULL} (the default), then \code{cols}
#'   will be updated by reference in plcace. If \code{names.extra} is
#'   a character string, then new columns will be appended to
#'   \code{DT} with the names formed by \code{paste0(var,
#'   names.extra)}. If \code{names.extra} is set to \code{TRUE}, then
#'   new columns will be appended to \code{DT} with the names
#'   \code{paste0(var, ".", f)}, where \code{f} is the name of the
#'   function passed by the user and is found using
#'   \code{deparse(substitute(f))}
#' @examples
#' data(mtcars)
#'
#' DT.mtcars <- as.data.table(mtcars)
#'
#' ##Add some columns for testing
#' DT.mtcars <- DT.mtcars %>%
#'   .[, mpg.char := as.character(mpg)] %>%
#'   .[, cyl.fact := as.factor(cyl)]
#'
#' ## mutate in place by reference
#' mutate_by_ref(copy(DT.mtcars),
#'               cols = c("mpg", "hp"), f = sqrt) %>%
#'   head %>% print
#'
#' ## mutate with new column names by reference
#' mutate_by_ref(copy(DT.mtcars),
#'               cols = c("mpg", "hp"), f = sqrt, names.extra = ".sqrt") %>%
#'   head %>% print
#'
#' ## mutate with new column names by reference
#' ## setting `names.extra` to TRUE will use `deparse(substitute(f))` to
#' ## get the name of f to use as `names.extra` and sepearte with a dot (`.`)
#' mutate_by_ref(copy(DT.mtcars),
#'               cols = c("mpg", "hp"), f = sqrt, names.extra = TRUE) %>%
#'   head %>% print
#'
#' ##mutate all numeric columns in place by reference
#' mutate_by_ref(copy(DT.mtcars),
#'               cols = is.numeric, f = sqrt) %>%
#'   head %>% print
#'
#' ##mutate all numeric columns with new names by reference
#' mutate_by_ref(copy(DT.mtcars),
#'               cols = is.numeric, f = sqrt, names.extra = ".sqrt") %>%
#'   head %>% print
#'
#' ##mutate all numeric columns with new names by reference
#' ##setting `names.extra` to TRUE
#' mutate_by_ref(copy(DT.mtcars),
#'               cols = is.numeric, f = sqrt, names.extra = TRUE) %>%
#'   head %>% print
## Mutate multiple columns by reference using the same function
mutate_by_ref <- function(DT, cols, f, names.extra = NULL) {

  if (!is.data.table(DT)) stop("Error: `DT` must be a data.table")

  if (!is.character(cols) && !is.function(cols))
    stop("Error: `cols` must be a character vector or a function")

  if (!is.function(f)) stop("Error: `f` must be a function")

  ##If cols is a function and not a character vector,
  ##get cols as a character vector
  if (is.function(cols) && !is.character(cols)) {
    cols <- try({
      f_cols <- cols
      cols <- DT[, vapply(.SD, f_cols, FUN.VALUE = logical(1))]

      if (!all(vapply(cols, is.logical, FUN.VALUE = logical(1))))
        stop("Error: cols as a function needs to return a logical vector")

      cols <- cols %>% .[. == TRUE] %>% names

      cols

    }, silent = FALSE)

    if (inherits(cols, "try-error")) {
      stop("Error: cols as a function needs to return a logical vector")
    }
  }

  ##New column names if requested by the user
  if (!is.null(names.extra)) {
    if (names.extra == TRUE) {
      names.extra <- paste0(".", deparse(substitute(f)))
    }
    if (!is.character(names.extra) && length(names.extra != 1))
      stop("Error: names.extra must be a character vector of length 1")

    new.cols <- paste0(cols, names.extra)
  } else {
    new.cols <- cols
  }

  DT <- DT[, c(new.cols) := lapply(.SD, f), .SDcols = cols]

  DT
}





