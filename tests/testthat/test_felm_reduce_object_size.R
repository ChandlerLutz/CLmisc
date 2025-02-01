## ./test_felm_reduce_object_size.R

context("reduce_felm_object_size function")

##Clear the workspace
## rm(list = ls()) 

library(lfe); library(stargazer)


## Simulate data -- from ?lfe::felm

# Covariates
x <- rnorm(1000)
x2 <- rnorm(length(x))
# Individuals and firms
id <- factor(sample(20,length(x),replace=TRUE))
firm <- factor(sample(13,length(x),replace=TRUE))
# Effects for them
id.eff <- rnorm(nlevels(id))
firm.eff <- rnorm(nlevels(firm))
# Left hand side
u <- rnorm(length(x))
y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u

## Estimate the model and print the results
est <- felm(y ~ x + x2 | id + firm)

## Example with 'reverse causation' (IV regression)

# Q and W are instrumented by x3 and the factor x4.
x3 <- rnorm(length(x))
x4 <- sample(12,length(x),replace=TRUE)
Q <- 0.3*x3 + x + 0.2*x2 + id.eff[id] + 0.3*log(x4) - 0.3*y + rnorm(length(x),sd=0.3)
W <- 0.7*x3 - 2*x + 0.1*x2 - 0.7*id.eff[id] + 0.8*cos(x4) - 0.2*y+ rnorm(length(x),sd=0.6)
# Add them to the outcome variable
y <- y + Q + W

## Estimate the IV model and report robust SEs
ivest <- felm(y ~ x + x2 | id + firm | (Q|W ~ x3 + factor(x4)))

# Example with multiway clustering

# Create a large cluster group (500 clusters) and a small one (20 clusters)
cl1 <- factor(sample(rep(1:500, length.out=length(x))))
cl2 <- factor(sample(rep(1:20, length.out=length(x))))
# Function for adding clustered noise to our outcome variable 
cl_noise <- function(cl) {
 obs_per_cluster <- length(x)/nlevels(cl)
 unlist(replicate(nlevels(cl), rnorm(obs_per_cluster, mean=rnorm(1), sd=runif(1)), simplify=FALSE))
}
# New outcome variable
y_cl <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + cl_noise(cl1) + cl_noise(cl2)

## Estimate and print the model with cluster-robust SEs (default)
est_cl <- felm(y_cl ~ x + x2 | id + firm | 0 | cl1 + cl2)

est.small <- reduce_felm_object_size(est)
ivest.small <- reduce_felm_object_size(ivest)
est_cl.small <- reduce_felm_object_size(est_cl)


test_that("running reduce_felm_object_size does not affect broom::tidy", {
  expect_equal(broom::tidy(est),
               broom::tidy(est.small))

  expect_equal(broom::tidy(est, se.type = "robust"),
               broom::tidy(est.small, se.type = "robust"))

  expect_equal(broom::tidy(ivest),
               broom::tidy(ivest.small))

  expect_equal(broom::tidy(ivest, se.type = "robust"),
               broom::tidy(ivest.small, se.type = "robust"))

  expect_equal(broom::tidy(est_cl),
               broom::tidy(est_cl.small))

  expect_equal(broom::tidy(est_cl, se.type = "cluster"),
               broom::tidy(est_cl.small, se.type = "cluster"))

})


test_that("running reduce_felm_object_size does not affect felm_broom_tidy", {
  expect_equal(felm_broom_tidy(est), felm_broom_tidy(est.small))

  expect_equal(felm_broom_tidy(ivest), felm_broom_tidy(ivest.small))

  expect_equal(felm_broom_tidy(est_cl), felm_broom_tidy(est_cl.small))

})

test_that("running reduce_felm_object_size does not affect stargazer, when the statistic is the number of observations and only robust or clustered SEs are used", {

  expect_equal(stargazer(est, keep.stat = "N", se = list(est$rse)),
               stargazer(est.small, keep.stat = "N", se = list(est.small$rse)))

  expect_equal(stargazer(ivest, keep.stat = "N", se = list(ivest$rse)),
               stargazer(ivest.small, keep.stat = "N", se = list(ivest.small$rse)))

  expect_equal(stargazer(est_cl, keep.stat = "N"),
               stargazer(est_cl.small, keep.stat = "N"))

})
