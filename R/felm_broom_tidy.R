## c:/Dropbox/Rpackages/CLmisc/R/felm_broom_tidy.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-07-30

#' tidy felm output using data.table
#'
#' Fast tidying of broom felm output. Note that the function will use
#' clustered standard errors if they are available. Otherwise, the
#' function will use robust standard errors
#'
#' @param felm.mod A model of type \code{felm} from the \code{lfe} package
#' @return a data.table with the tidy estimation output
#' @export
#' @examples
#' ##From felm
#' library(lfe)
#'
#' ## create covariates
#' x <- rnorm(1000)
#' x2 <- rnorm(length(x))
#'
#' ## individual and firm
#' id <- factor(sample(20,length(x),replace=TRUE))
#' firm <- factor(sample(13,length(x),replace=TRUE))
#'
#' ## effects for them
#' id.eff <- rnorm(nlevels(id))
#' firm.eff <- rnorm(nlevels(firm))
#'
#' ## left hand side
#' u <- rnorm(length(x))
#' y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u
#'
#' ## estimate and print result
#' est <- felm(y ~ x+x2| id + firm)
#' summary(est, robust = TRUE)
#' felm_broom_tidy(est)
#'
#' ## With clustered standard errors
#' est2 <- felm(y ~ x+x2| id + firm | 0 | firm)
#' summary(est2)
#' felm_broom_tidy(est2)
#'
#'
#' # make an example with 'reverse causation'
#' # Q and W are instrumented by x3 and the factor x4. Report robust s.e.
#' x3 <- rnorm(length(x))
#' x4 <- sample(12,length(x),replace=TRUE)
#'
#' Q <- 0.3*x3 + x + 0.2*x2 + id.eff[id] + 0.3*log(x4) - 0.3*y + rnorm(length(x),sd=0.3)
#' W <- 0.7*x3 - 2*x + 0.1*x2 - 0.7*id.eff[id] + 0.8*cos(x4) - 0.2*y+ rnorm(length(x),sd=0.6)
#'
#' # add them to the outcome
#' y <- y + Q + W
#'
#' ivest <- felm(y ~ x + x2 | id+firm | (Q|W ~x3+factor(x4)))
#' summary(ivest,robust=TRUE)
#' felm_broom_tidy(ivest)
#'
#'
#' ##With clustered standard errors
#' ivest2 <- felm(y ~ x + x2 | id+firm | (Q|W ~x3+factor(x4)) | firm)
#' summary(ivest2)
#' felm_broom_tidy(ivest2)
felm_broom_tidy <- function(felm.mod) {
  DT.out <- data.table::data.table(
    term = rownames(felm.mod$coefficients),
    estimate = coefficients(felm.mod)
  )

  felm.mod.attributes <- names(felm.mod)

  if ("clustervar" %in% felm.mod.attributes) {
    ##cluster var
    DT.out <- DT.out %>%
      .[, `:=`(std.error = c(felm.mod$cse),
               statistic = c(felm.mod$ctval),
               p.value = c(felm.mod$cpval)
               )]
  } else {
    ##no clustering, just use robust
    DT.out <- DT.out %>%
      .[, `:=`(std.error = c(felm.mod$rse),
               statistic = c(felm.mod$rtval),
               p.value = c(felm.mod$rpval)
               )]

  }
  return(DT.out[])
}
