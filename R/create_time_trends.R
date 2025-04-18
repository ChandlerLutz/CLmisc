## c:/Dropbox/Rpackages/CLmisc/R/create_time_trends.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-05-13

#' Fast creatation of time trends
#'
#'
#' \code{create_time_trends()} quickly creates time trends based on a
#' character columns from inputted data. This function is useful for
#' statistical analysis when you want time trends. The function will
#' only operate on `data.tables` by reference.
#'
#' @family dummy functions
#' @seealso \code{\link{dummy_cols}} For creating dummy columns
#'
#' @param .data a \code{data.table}
#' @param time.col a time column; can be of any type. `time.col` will
#'   be ordered using \code{order()}
#' @param select.column a column with cross-sectional units that will
#'   be used to create the time trends. Think fixed effects such as
#'   cities, regions, counties, etc.
#' @param quadratic.time.trends a \code{logical} for whether to
#'   include quadratic (squared) time trends __IN ADDITION__ to the
#'   linear time trends for the the dummy columns. Default is
#'   \code{FALSE}
#' @param remove.first.trend removes the first time trend so that only
#'   \code{N - 1} time trends reamin. This avoids the
#'   multicollinearity in models. Default is \code{FALSE}
#' @return A data.table with the same number of rows, but the time
#'   trends included. The format of the time trend columns will be
#'   \code{tt.select.column_value} (linear trends) or
#'   \code{tt2.select.column_value} (quadratic trends) where
#'   \code{select.column} is the name of the character column
#'   requested from the user and \code{value} is the value of the of
#'   the character column for that time trend
#' @export
#' @examples
#' ## For unicode meanings, see
#' ## https://gist.github.com/ChandlerLutz/cca06fb35003713efae87c0322ac193e
#' clean_readxl_backslashes("\\n")
#' clean_readxl_backslashes("\\u00ad")
#' clean_readxl_backslashes("\\u2013")
#' clean_readxl_backslashes("\\u2013")
#' crime <- setDT(expand.grid(city = c("SF", "SF", "NYC"),
#'                year = c(1990, 2000, 2010),
#'                crime = 1:3, stringsAsFactors = FALSE))
#' crime2 <- copy(crime)
#' print(create_time_trends(crime2, time.col = "year",
#'                          select.column = "city"))
create_time_trends <- function(.data, time.col, select.column,
                               quadratic.time.trends = FALSE,
                               remove.first.trend = FALSE) {

  ##some data checks
  stopifnot(is.data.table(.data),
            is.character(time.col),
            is.character(select.column)
            )

  ##for R cmd check
  temp.time.col <- temp.time.trend <- temp.time.trend2 <- NULL

  .data <- .data[, temp.time.col := get(time.col)]

  ##the time trends
  .data.tt <- .data %>%
    .[, .(temp.time.col = unique(temp.time.col))] %>%
    .[order(temp.time.col)] %>%
    ##linear time trend
    .[, temp.time.trend := 1:.N] %>%
    ##quadratic time trend
    .[, temp.time.trend2 := temp.time.trend ^ 2]

  ##Merge in the time trends
  .data <- .data %>%
    merge(.data.tt, by = "temp.time.col") %>%
    dummy_cols(select_columns = select.column, remove_first_dummy = remove.first.trend)

  ## -- Linear time trends -- ##

  ##the names of the dummy variables
  names.dummies <- names(.data) %>% .[grepl(paste0("^", select.column, "_"), x= .)]
  ##updated names for the time trends
  names.dummies.tt <- paste0("tt.", names.dummies)

  .data <- .data %>%
    ##update the names
    data.table::setnames(names.dummies, names.dummies.tt) %>%
    ## multiply the dummies by the time trend
    .[, c(names.dummies.tt) := lapply(.SD, function(x) x * temp.time.trend),
      .SDcols = names.dummies.tt]

  print(sprintf("The regex for the linear time trend columns are `^tt.%s_`", select.column))

  ## -- Quadratic time trends if requested by the user -- ##

  if (quadratic.time.trends == TRUE) {
    ##user requested linear time trends

    .data <- .data %>%
      dummy_cols(select_columns = select.column, remove_first_dummy = remove.first.trend)
    ##the names of the dummy variables
    names.dummies <- names(.data) %>% .[grepl(paste0("^", select.column, "_"), x = .)]
    ##updated names for the time trends
    names.dummies.tt2 <- paste0("tt2.", names.dummies)

    .data <- .data %>%
      data.table::setnames(names.dummies, names.dummies.tt2) %>%
      ## multiply the dummies by the quadratic time trend
    .[, c(names.dummies.tt2) := lapply(.SD, function(x) x * temp.time.trend2),
      .SDcols = names.dummies.tt2]


    ##print the regex
    print(sprintf("The regex for the linear time trend columns are `^tt.%s_`",
                  select.column))

  }

  ##clean up
  .data <- .data[, c("temp.time.col", "temp.time.trend", "temp.time.trend2") := NULL]


  return(.data)

}






