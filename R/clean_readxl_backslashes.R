## ./clean_readxl_backslashes.R

#' Clean extra backslashes from readxl
#'
#' \code{clean_readxl_backslashes()} cleans extras backslashes that
#' readxl uses as escape characters when readxl reads in data. More
#' specifically, readxl converts double backslashes from readxl to the
#' actual R characters. See examples. 
#'
#' @param string a character string
#' @return A character string with the extra backslashes inserted by
#'   readxl removed.
#' @export
#' @examples
#' ## https://gist.github.com/ChandlerLutz/cca06fb35003713efae87c0322ac193e
#' ## New line 
#' clean_readxl_backslashes("\\n")
#' ## soft hyphen
#' clean_readxl_backslashes("\\u00ad")
#' ## en dash
#' clean_readxl_backslashes("\\u2013")
#' ## em dash
#' clean_readxl_backslashes("\\u2014")
clean_readxl_backslashes <- function(string) {

  ## see
  ## https://gist.github.com/ChandlerLutz/cca06fb35003713efae87c0322ac193e
  ## https://stackoverflow.com/a/27492072/1317443
  ## https://jkorpela.fi/dashes.html
  
  out <- string %>%
    ## New line
    gsub("\\n", "\n", x = ., fixed = TRUE) %>%
    ## soft hyphen
    gsub("\\u00ad", "\u00ad", x = ., fixed = TRUE) %>%
    ## en dash
    gsub("\\u2013", "\u2013", x = ., fixed = TRUE) %>%
    ## em dash
    gsub("\\u2014", "\u2014", x = ., fixed = TRUE)
  return(out)
  
}
