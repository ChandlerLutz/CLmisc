## c:/Dropbox/Rpackages/CLmisc/DT_summarize.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2017-08-10

#' Convenient functions for summarizing a \code{data.table}.
#'
#' Convenient functions for summarizing a \code{data.table}. These
#' functions are convenient to use with \code{data.table}'s \code{.SD}
#' argument Shorthand functions are also available that only require a
#' \code{data.table}, a character vector of variables, \code{vars},
#' and an optional \code{names.extra} argument:
#'
#' \describe{
#'    \item{DT_mean}{\code{fun = mean} and \code{names.extra} defaults to \code{".mean"}}
#'    \item{DT_sd}{\code{fun = sd}and and \code{names.extra} defaults to \code{".sd"}}
#'    \item{DT_var}{\code{fun = var} and \code{names.extra} defaults to \code{".var"}}
#'    \item{DT_sum}{\code{fun = sum} and \code{names.extra} defaults to \code{".sum"}}
#'    \item{DT_log_diff}{\code{fun = function(x) log(x[length(x)]) -log(x[1])} and \code{names.extra} defaults to \code{".log.diff"}}
#'    \item{DT_perc_diff}{\code{fun = function(x) function(x) (x[length(x)]-x[1])/x[1]} and \code{names.extra} defaults to \code{".perc.diff"}}
#' }
#'
#' @name summarizing
#' @param DT a \code{data.table}. Can also be \code{.SD} if used
#'     inside another \code{data.table}. see examples below
#' @param fun a function used to summarize the data
#' @param vars a character vector with variable names
#' @param names.extra a string used as a suffix for new variable names
#' @param na.rm set to \code{TRUE} to remove \code{NA}s. Default is
#'     \code{FALSE}
#' @return a summarized \code{data.table}
#' @examples
#' data(mtcars)
#' setDT(mtcars) ##Convert to a data.table
#'
#' ##Base use of DT_summarize()
#' DT_summarize(mtcars, fun = mean, vars = c("mpg","hp"))
#' ##using with by and .SD
#' mtcars %>%
#'     .[, DT_summarize(.SD, fun = mean, vars = c("mpg","hp")), by = cyl]
#' ##Using the convenience function DT_mean (and leaving the names.extra
#' ##argument as the default)
#' DT_mean(mtcars, vars = c("mpg", "hp"))
#' mtcars %>%
#'     .[, DT_mean(.SD, vars = c("mpg","hp")), by = cyl]
#' ##Take the mean of of hp and mpg, and the variance of disp and wt
#' ##Note, we concatenate usign c()
#' mtcars %>%
#'     .[, c(DT_mean(.SD, vars = c("mpg", "hp")),
#'           DT_var(.SD, vars = c("disp", "wt"))
#'           )]
#' ##by cyl
#' mtcars %>%
#'     .[, c(DT_mean(.SD, vars = c("mpg", "hp")),
#'           DT_var(.SD, vars = c("disp", "wt"))
#'           ), by = cyl]
#' ##The mean, standard deviation, and variance of mpg and hp
#' summary.vars <- c("mpg", "hp")
#' mtcars %>%
#'     .[, c(DT_mean(.SD, vars = summary.vars),
#'           DT_sd(.SD, vars = summary.vars),
#'           DT_var(.SD, vars = summary.vars)), by = cyl]
#'
#' ## Other useful summary functions (may not have a
#' ##useful interpretation here)
#' summary.vars <- c("mpg", "hp")
#' mtcars %>%
#'     .[, c(DT_sum(.SD, vars = summary.vars),
#'           DT_log_diff(.SD, vars = summary.vars),
#'           DT_perc_diff(.SD, vars = summary.vars)),
#'       by = cyl]
NULL

#' @export
#' @rdname summarizing
DT_summarize <- function(DT, fun, vars, names.extra = NULL) {
    DT <- DT %>%
        .[, lapply(.SD, fun), .SDcols = vars]

    ##Set the names and return
    DT %>%
        setnames(vars, paste0(vars, names.extra)) %>%
        return

}

#' @export
#' @rdname summarizing
DT_mean <- function(DT, vars, names.extra = ".mean", na.rm = FALSE) {
    f_mean <- function(x) mean(x, na.rm = na.rm)
    DT_summarize(DT, fun = f_mean, vars = vars,
                 names.extra = names.extra)
}

#' @export
#' @rdname summarizing
DT_sd <- function(DT, vars, names.extra = ".sd", na.rm = FALSE) {
    f_sd <- function(x) sd(x, na.rm = na.rm)
    DT_summarize(DT, fun = f_sd, vars = vars,
                 names.extra = names.extra)
}

#' @export
#' @rdname summarizing
DT_var <- function(DT, vars, names.extra = ".var", na.rm = FALSE) {
    f_var <- function(x) var(x, na.rm = na.rm)
    DT_summarize(DT, fun = f_var, vars = vars,
                 names.extra = names.extra)
}

#' @export
#' @rdname summarizing
DT_sum <- function(DT, vars, names.extra = ".sum", na.rm = FALSE) {
    f_sum <- function(x) sum(x, na.rm = na.rm)
    DT_summarize(DT, fun = f_sum, vars = vars,
                 names.extra = names.extra)
}

#' @export
#' @rdname summarizing
DT_log_diff <- function(DT, vars, names.extra = ".log.diff") {
    ##Assume in descending order, not multiplied by 100
    f_log_diff <- function(x) (log(x[length(x)]) - log(x[1]))
    DT_summarize(DT, fun = f_log_diff, vars = vars,
                 names.extra = names.extra)
}

#' @export
#' @rdname summarizing
DT_perc_diff <- function(DT, vars, names.extra = ".perc.diff") {
    ##Assume in descending order, not multiplied by 100
    f_perc_diff <- function(x) (x[length(x)] - x[1]) / x[1]
    DT_summarize(DT, fun = f_perc_diff, vars = vars,
                 names.extra = names.extra)
}
