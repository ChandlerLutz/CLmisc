## c:/Dropbox/Rpackages/CLmisc/R/fntile.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-12-04

#' fast ntile
#'
#' @param x vector of data
#' @param n the numbe of quantiles desired
#' @return A vector corresponding to the ntiles. (As in \code{dplyr::ntile}.)
#' @export
#' @examples
#' fntile(c(NA, 1:20, NA), 5)
fntile <- function(x, n) {
  x.length <- sum(!is.na(x))
  return(as.integer(n * {frank(x, na.last = "keep", ties.method = "first") - 1} / x.length + 1))
}
