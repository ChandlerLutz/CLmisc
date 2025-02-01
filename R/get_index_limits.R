#' Convenient Function to get the index limits of a \code{data.frame} or data.table
#'
#' Convenient function to get the index limits of a \code{data.table} with a variable called \code{index}. This function is especially useful to extend the axis limits of a `ggplot` using the \code{min.extend} and \code{max.extend} arguments of this function.
#'
#' @param .data a \code{data.table} with a variable \code{index}
#' @param min.extend how much to extend the limits relative to the
#'   minimum
#' @param max.extend how much to extend the limits relative to the
#'   maximum
#' @return a numeric vector of length 2, where the first element is
#'   the minimum and the second element is the maximum
#' @export
#' @examples
#' DT <- data.table(index = as.Date(c("2010-01-01", "2015-01-01", "2021-01-01")))
#' get_index_limits(DT)
#' get_index_limits(DT, max.extend = 365)
#' get_index_limits(DT, min.extend = 365)
#' get_index_limits(DT, min.extend = -365)
get_index_limits <- function(.data, min.extend = 0, max.extend = 0) {
  if (!is.data.frame(.data)) stop(".data is not a data.frame")
  if (!("index" %chin% names(.data))) stop("index is not a variable in .data")

  index.min <- .data$index %>% min
  index.max <- .data$index %>% max

  c(min = index.min + min.extend, max = index.max + max.extend)
}
