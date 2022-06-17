## ./integer_breaks.R

#' Create integer breaks for ggplot2 axes
#'
#' See https://stackoverflow.com/a/62321155
#'
#' @param n The number of breaks
#' @param ... extra parameters passes to \code{pretty}
#' @examples
#' data(mtcars)
#' ggplot(mtcars[mtcars$wt > 3, ], aes(x = cyl, y = wt)) +
#'  geom_point() +
#'  scale_y_continuous(breaks = integer_breaks())
#' @export 
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


