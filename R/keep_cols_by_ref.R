## c:/Dropbox/Rpackages/CLmisc/R/keep_cols_by_ref.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-10-23

#' Keep columns in a data.table by reference
#'
#' @param DT a \code{data.table}
#' @param keep.cols a character vector of columns in \code{DT}
#' @export
keep_cols_by_ref <- function(DT, keep.cols) {
  if (!is.character(cols)) stop("Error: keep.cols must be a character vector")
  delete.cols <- names(DT) %>% .[!(. %chin% keep.cols)]
  DT <- DT[, c(delete.cols) := NULL]
  return(DT)
}

