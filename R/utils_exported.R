## c:/Dropbox/Rpackages/CLmisc/R/utils_exported.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2020-09-11


#' Convert a file path to windows format
#'
#' Convert a file path to windows format by turning forward slashes
#' `/` to backslashes `\`
#'
#' @param .file.path a unix style file path
#' @export
#' @examples
#' print(getwd())
#' convert_file_path_to_windows_format(getwd()) %>% print
convert_file_path_to_windows_format <- function(.file.path)
  gsub("/", "\\\\", .file.path)


#' Recursively make a directory if it does not exist
#'
#' Similar to the linux command \code{mkir -p}
#'
#' @param dir the directory (including parent directories) to create
#' @export
#' @examples
#' \donttest{
#' mkdir_p("test/test")
#' }
mkdir_p <- function(dir) {
  if (!dir.exists(dir))
    dir.create(dir, recursive = TRUE)
}
