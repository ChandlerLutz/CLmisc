## c:/Dropbox/Rpackages/CLmisc/R/detach_packages.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-16


#' Detach all but base packages in R
#'
#' From https://stackoverflow.com/a/8817519/1317443
#'
#' @export
detach_all_packages <- function() {

  basic.packages <- c("package:stats", "package:graphics", "package:grDevices",
                      "package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

  return(invisible(NULL))

}


