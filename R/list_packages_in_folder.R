## c:/Dropbox/Rpackages/CLmisc/R/list_packages_in_folder.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-16

#' List all of the packages used in a folder of R scripts
#'
#' Searches through a folder, fitlers out the R scripts and then
#' returns a character vector with the R packages. Note: this function
#' will not list package dependencies
#'
#'
#' @param folder string with the path to the folder
#' @return character vector with the packages
#' @export
list_packages_in_folder <- function(folder) {

  ##get all of the files in a folder
  packages <- list.files(folder, recursive = TRUE, full.names = TRUE) %>%
    ##Make sure they are R scripts
    .[grepl(".R$|.r$", x = .)] %>%
    ##read iin the code
    lapply(readLines) %>%
    ##combine lines into a vector
    do.call("c", args = .) %>%
    ##Only keep lines with library() and require()
    .[grepl("library(.*)|require(.*)", x = .)] %>%
    paste0(collapse = " ") %>%
    ##split based on library or require
    strsplit("library|require") %>%
    ##unlist
    unlist(.) %>%
    ##regex from https://stackoverflow.com/a/8613332/1317443
    regmatches(gregexpr("(?<=\\().*?(?=\\))", text = ., perl=T)) %>%
    ##unlist
    unlist(.) %>%
    ##only unique values
    unique(.)

  return(packages)

}
