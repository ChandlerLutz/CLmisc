## c:/Dropbox/Rpackages/CLmisc/R/split_blocks.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-03-12


#' To create the blocks for bootstrapping
#'
#' A function to create the blocks for block bootstrapping
#'
#' @param x vector to split
#' @param m the block length
#' @return a list of blocks, where each element of the list has length \code{m}
#' @export
#' @examples
#' x <- 1:10
#' split_blocks(x, 2)
#' split_blocks(x, 3)
#' split_blocks(x, 4)
split_blocks <- function(x, m) {
  # x -- the time series; m -- the size of the blocks
  x <- as.numeric(x) #convert ot numeric
  #The number of blocks in B_m
  num.blocks <- length(x) - m + 1
  #get B_m -- x in blocks of size m
  B_m <- vector("list", length = num.blocks)
  for (i in seq_len(num.blocks)) {
    B_m[[i]] <- x[i:(i + m - 1)]
  }
  return(B_m)
}
