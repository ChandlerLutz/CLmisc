## ./felm_reduce_object_size.R

#' Reduce the size of an felm object
#'
#' \code{felm_reduce_object_size} reduces the size of an an felm
#' object. This function is helpful for clearing up memory after
#' running a regression or saving and felm object to the disk using
#' \code{saveRDS}. Running \code{felm_reduce_object_size}, preserves
#' using \code{lfe::summary.felm}, \code{stargazer::stargazer},
#' \code{broom::tidy}, and \code{CLmisc::felm_broom_tidy}.
#'
#' \strong{__NOTE__: The r-squared, etc, will not be preserved as this
#' function deletes residuals to save space}. Also, when calling
#' \code{broom::tidy} or \code{stargazer::stargazer}, you'll get an
#' error saying that a F-statistic can't be reported. This is extra
#' output used by \code{summary.felm}. Please ignore this as it
#' affects this rather un-useful F-statistic. You may also see other
#' warnings or errors. These can be safely ignored as they often just
#' produce non-useful summary stats
#'
#' @param felm.object The object of type \code{felm}
#'
#' @examples
#' library(lfe); library(stargazer)
#' 
#' ## Simulate data -- from ?lfe::felm
#' 
#' # Covariates
#' x <- rnorm(1000)
#' x2 <- rnorm(length(x))
#' # Individuals and firms
#' id <- factor(sample(20,length(x),replace=TRUE))
#' firm <- factor(sample(13,length(x),replace=TRUE))
#' # Effects for them
#' id.eff <- rnorm(nlevels(id))
#' firm.eff <- rnorm(nlevels(firm))
#' # Left hand side
#' u <- rnorm(length(x))
#' y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u
#' 
#' ## Estimate the model and print the results
#' est <- felm(y ~ x + x2 | id + firm)
#' 
#' ## Example with 'reverse causation' (IV regression)
#' 
#' # Q and W are instrumented by x3 and the factor x4.
#' x3 <- rnorm(length(x))
#' x4 <- sample(12,length(x),replace=TRUE)
#' Q <- 0.3*x3 + x + 0.2*x2 + id.eff[id] + 0.3*log(x4) - 0.3*y + rnorm(length(x),sd=0.3)
#' W <- 0.7*x3 - 2*x + 0.1*x2 - 0.7*id.eff[id] + 0.8*cos(x4) - 0.2*y+ rnorm(length(x),sd=0.6)
#' # Add them to the outcome variable
#' y <- y + Q + W
#' 
#' ## Estimate the IV model and report robust SEs
#' ivest <- felm(y ~ x + x2 | id + firm | (Q|W ~ x3 + factor(x4)))
#' 
#' # Example with multiway clustering
#' 
#' # Create a large cluster group (500 clusters) and a small one (20 clusters)
#' cl1 <- factor(sample(rep(1:500, length.out=length(x))))
#' cl2 <- factor(sample(rep(1:20, length.out=length(x))))
#' # Function for adding clustered noise to our outcome variable 
#' cl_noise <- function(cl) {
#'  obs_per_cluster <- length(x)/nlevels(cl)
#'  unlist(replicate(nlevels(cl), rnorm(obs_per_cluster, mean=rnorm(1), sd=runif(1)), simplify=FALSE))
#' }
#' # New outcome variable
#' y_cl <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + cl_noise(cl1) + cl_noise(cl2)
#' 
#' ## Estimate and print the model with cluster-robust SEs (default)
#' est_cl <- felm(y_cl ~ x + x2 | id + firm | 0 | cl1 + cl2)
#' 
#' est.small <- reduce_felm_object_size(est)
#' ivest.small <- reduce_felm_object_size(ivest)
#' est_cl.small <- reduce_felm_object_size(est_cl)
#'
#' identical(felm_broom_tidy(est), felm_broom_tidy(est.small))
#' identical(felm_broom_tidy(ivest), felm_broom_tidy(ivest.small))
#' identical(felm_broom_tidy(est_cl), felm_broom_tidy(est_cl.small))
#'
#' identical(stargazer(est, keep.stat = "N", se = list(est$rse)),
#'           stargazer(est.small, keep.stat = "N", se = list(est.small$rse)))
#' 
#' identical(stargazer(ivest, keep.stat = "N", se = list(ivest$rse)),
#'           stargazer(ivest.small, keep.stat = "N", se = list(ivest.small$rse)))
#' 
#' identical(stargazer(est_cl, keep.stat = "N"),
#'           stargazer(est_cl.small, keep.stat = "N"))
#'
#' ## -- Size Differences of examples
#' object.size(est) / object.size(est.small)
#' object.size(ivest) / object.size(ivest.small)
#' object.size(est_cl) / object.size(est_cl.small)
#' @export
reduce_felm_object_size <- function(felm.object) {

  if (class(felm.object) != "felm")
    stop("felm.object must be an felm object")

  ##delete these felm attributes
  attr.to.delete <- c("residuals", "response", "call", 
                      "c.fitted.values", "c.response",
                      "fitted.values",
                      "cfactor", "fe", "r.residuals",
                      "r.iv.residuals", "iv.residuals",
                      "ivx", "ivy", "centred.exo",
                      "STATS", "model", 
                      "terms", "hasicpt", "numctrl",
                      "keepX", "keepCX",
                      "vcv", "robustvcv", "clustervcv",
                      "inv", "TSS", "P.TSS"
                      )

  felm.object[attr.to.delete] <- NULL

  ##set the clustervar to NA as some other functions use
  ##it to identify the whether cluster standard errors are used
  if ("clustervar" %chin% names(felm.object))
    felm.object[["clustervar"]] <- NA


  if (!is.null(felm.object$stage1)) {
    felm.object$stage1[attr.to.delete] <- NULL
    if ("clustervar" %chin% names(felm.object$stage1))
      felm.object$stage1[["clustervar"]] <- NA
  }

  ##Delete the environment associated with the formula property
  ##https://stackoverflow.com/a/52372480
  attr(felm.object$formula, ".Environment") <- NULL

  return(felm.object)

}
