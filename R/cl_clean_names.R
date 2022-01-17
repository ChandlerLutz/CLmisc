

#' Clean data.table names to be lowercase and separated by a period
#' \code{.}
#'
#' This function uses the \code{janitor::make_clean_names} function,
#' but replaces underscores with a period. Note that this function
#' updates the \code{data.table} by reference
#' 
#' @param DT a \code{data.table}
#' @examples
#' DT <- data.table(`Var A` = 1, VarB = 2, var_c = 3)
#' print(cl_clean_names(DT))
#' @export
cl_clean_names <- function(DT) {

  if (!is.data.table(DT)) stop("DT must be a data.table")
  
  old.names <- names(DT)

  new.names <- old.names %>%
    janitor::make_clean_names(.) %>%
    gsub("_", ".", x = .)

  DT <- setnames(DT, old.names, new.names)

  return(DT)
    
}
