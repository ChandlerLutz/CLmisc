## c:/Dropbox/Rpackages/CLmisc/R/cut_with_bin_labels.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2020-07-15

#' \code{cut} using endpoints as bin labels bin labels
#'
#' Use endpoints as bin labels with \code{cut}. The only difference between
#' this function and \code{cut}, is that this function will label the
#' levels based on the endpoints if \code{labels} are specified as either
#' "left" or "right". For more information, see \code{cut}
#'
#' @seealso \code{cut()}, \code{ggplot2::cut_interval()},
#'   \code{ggplot2::cut_width()}, \code{ggplot2::cut_number()}
#'
#' @param x a numeric vector which is to be converted to a factor by
#'   cutting.
#' @param breaks either a numeric vector of two or more unique cut
#'   points or a single number (greater than or equal to 2) giving the
#'   number of intervals into which 'x' is to be cut.
#' @param labels labels for the levels of the resulting category.  By
#'   default, labels are constructed using '"(a,b]"' interval
#'   notation.  If 'labels = FALSE', simple integer codes are returned
#'   instead of a factor. If 'labels = "left"' then the labels will be
#'   a numeric verctor with the left endpoints. If 'labels = "right"'
#'   then the labels will be a numeric verctor with the right
#'   endpoints.
#' @param include.lowest logical, indicating if an 'x[i]' equal to the
#'   lowest (or highest, for 'right = FALSE') 'breaks' value should be
#'   included.
#' @param right logical, indicating if the intervals should be closed
#'   on the right (and open on the left) or vice versa. Default is \code{TRUE}
#' @param dig.lab integer which is used when labels are not given.  It
#'   determines the number of digits used in formatting the break
#'   numbers.
#' @param order_result logical: should the result be an ordered
#'   factor?
#' @param ... further arguments passed to or from other methods.
#' @seealso 
#' @export
#' @examples
#' data(mtcars)
#' breaks <- seq(from = 0, to = max(mtcars$hp) + 10, by = 10)
#' data.table(hp = mtcars$hp,
#'            bin.cut.default = cut(mtcars$hp, breaks = breaks),
#'            bin.left.endpoint = cut_with_bin_labels(mtcars$hp, breaks = breaks, labels = "left"),
#'            bin.right.endpoint = cut_with_bin_labels(mtcars$hp, breaks = breaks, labels = "right")
#'           ) %>% print
#'
#' #Using `right` == FALSE
#' data.table(hp = mtcars$hp,
#'            bin.cut.default = cut(mtcars$hp, breaks = breaks, right = FALSE),
#'            bin.left.endpoint = cut_with_bin_labels(mtcars$hp, breaks = breaks, labels = "left", right = FALSE),
#'            bin.right.endpoint = cut_with_bin_labels(mtcars$hp, breaks = breaks, labels = "right", right = FALSE)
#'           ) %>% print
cut_with_bin_labels <- function(x, breaks, labels = NULL,
         include.lowest = FALSE, right = TRUE, dig.lab = 3,
         ordered_result = FALSE, ...) {

  if (!is.null(labels) && labels %in% c("right", "left")) {
    out <- cut(x = x, breaks = breaks,
               include.lowest = include.lowest,
               right = right, dig.lab = dig.lab,
               ordered_result = ordered_result, ...) %>%
      as.character
    if (labels == "left" && isTRUE(right)) {
      out <- gsub("\\((.*),.*\\]$", "\\1", x = out) %>%
        as.numeric
    } else if (labels == "right" && isTRUE(right)) {
      out <- gsub("\\(.*,(.*)\\]$", "\\1", x = out) %>%
        as.numeric
    } else if (labels == "left" && isFALSE(right)) {
      out <- gsub("\\[(.*),.*\\)$", "\\1", x = out) %>%
        as.numeric
    } else if (labels == "right" && isFALSE(right)) {
      out <- gsub("\\[.*,(.*)\\)$", "\\1", x = out) %>%
        as.numeric
    }
  } else {
    out <- cut(x = x, breaks = breaks, labels = labels,
               include.lowest = include.lowest,
               right = right, dig.lab = dig.lab,
               ordered_result = ordered_result, ...)
  }
  return(out)
}
