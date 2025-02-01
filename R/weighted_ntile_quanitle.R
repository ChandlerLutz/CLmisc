## c:/Dropbox/Rpackages/CLmisc/R/weighted_ntile_quanitle.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-11-03

##From https://github.com/HughParsonage/hutils

#' Weighted (ranked) quantiles
#'
#' @param vector The vector for which quantiles are desired.
#' @param weights The weights associated with the vector. None should be \code{NA} or zero.
#' @param n The number of quantiles desired.
#' @return A vector of integers corresponding to the ntiles. (As in \code{dplyr::ntile}.)
#' @examples
#' weighted_ntile(1:10, n = 5)
#' weighted_ntile(1:10, weights = c(rep(4, 5), rep(1, 5)), n = 5)
#' @export
#' @details With a short-length vector, or with weights of a high variance, the results may be unexpected.

weighted_ntile <- function(vector, weights = rep(1, times = length(vector)), n) {
  if (missing(weights) || length(weights) <= 1L) {
    v <- vector
    # This line is basically from dplyr. MIT License
    return(as.integer(n * {frank(v, ties.method = "first") - 1} / length(v) + 1))
  }

  min_w <- min(weights)
  if (min_w < 0) {
    stop("`weights` contained negative values. Ensure `weights` is non-negative.")
  }

  if (min_w == 0) {
    warning("Some weights are zero. Maximum ntile may be incorrect.")
  }

  if (length(weights) != length(vector)) {
    stop("`length(weights) = ", length(weights), "`, yet ",
         "`length(vector) = ", length(vector), "`. ",
         "`vector` and `weights` must have the same length.")
  }

  ov <- order(vector)
  out <- as.integer(n * cumsum(shift(x = weights[ov], fill = 0)) / sum(weights))

  if (last(out) >= n) {
    warning("Some ntiles greater than n = ", n)
  }
  out[order(ov)] + 1L
}


#' Weighted quantile
#' @description \code{quantile} when the values are weighted
#' @param v A vector from which sample quantiles are desired.
#' @param w Weights corresponding to each \code{v}.
#' @param p Numeric vector of probabilities. Missing values or values outside
#' \eqn{[0, 1]} raise an error.
#' @param v_is_sorted (logical, default: \code{FALSE}) If \code{TRUE}, ordering
#' \code{v} is assumed to be sorted. Only set to \code{TRUE} when it is certain
#' that \code{v} is sorted (as within groups of tables).
#' @return A vector the same length as \code{p}, the quantiles corresponding
#' to each element of \code{p}.
#' @export weighted_quantile

weighted_quantile <- function(v,
                              w = NULL,
                              p = (0:4)/4,
                              v_is_sorted = FALSE) {
  if (!length(p)) {
    stop("`p` had length zero. ",
         "Ensure `p` is a numeric vector with values in [0, 1].")
  }
  if (!is.numeric(p)) {
    stop("`p` was a ", class(p), " but must be numeric.")
  }
  if (anyNA(p)) {
    stop("`p` contained missing values. ",
         "Impute these values or remove the missing values.")
  }
  if (min(p) < 0) {
    stop("`p` contained negative values. ",
         "All values in `p` must be in [0, 1].")
  }
  if (max(p) > 1) {
    stop("`max(p) > 1`. ",
         "All values in `p` must be in [0, 1].")
  }

  if (length(w) <= 1L) {
    message("`w` is NULL or a single value, so returning unweighted quantiles.")
    return(stats::quantile(v, p, names = FALSE))
  }
  if (length(w) != length(v)) {
    stop("`length(v) = ", length(v), "`, yet ",
         "`length(w) = ", length(w), "`. ",
         "The lengths of `v` must be `w` must be equal.")
  }
  if (v_is_sorted) {
    ov <- seq_along(v)
    P <- cumsum(w) / sum(w)
  } else {
    ov <- order(v)
    P <- cumsum(w[ov]) / sum(w)
  }

  iv <- integer(length(p))
  for (i in seq_along(iv)) {
    iv[i] <- which.max(P >= p[i])
  }
  {v[ov]}[iv]
}
