## c:/Dropbox/Rpackages/CLmisc/R/keep_cols_by_ref.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-10-23

#' Select columns in a data.table by reference
#'
#' @param DT a \code{data.table}
#' @param keep.cols a character vector of columns in \code{DT}
#' @examples
#' data(mtcars)
#' DT <- as.data.table(mtcars)
#' DT <- select_by_ref(DT, "hp")
#' head(DT)
#'
#' @export
select_by_ref <- function(DT, keep.cols) {
  if (!is.character(keep.cols)) stop("Error: keep.cols must be a character vector")
  delete.cols <- names(DT) %>% .[!(. %chin% keep.cols)]
  DT <- DT[, c(delete.cols) := NULL]
  return(DT)
}

