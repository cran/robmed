context("standard bootstrap test: covariance")


## load package
library("robmed", quietly = TRUE)

## control parameters
n <- 250          # number of observations
a <- c <- 0.2     # true effects
b <- 0            # true effect
R <- 1000         # number of bootstrap samples
seed <- 20150601  # seed for the random number generator

## set seed for reproducibility
set.seed(seed)

## generate data
X <- rnorm(n)
M1 <- a * X + rnorm(n)
M2 <- rnorm(n)
Y <- b * M1 + c * X + rnorm(n)
C1 <- rnorm(n)
C2 <- rnorm(n)
test_data <- data.frame(X, Y, M1, M2, C1, C2)

## run bootstrap test and compute summary
set.seed(seed)
level <- c(0.9, 0.95)
boot <- test_mediation(test_data, x = "X", y = "Y", m = "M1",
                       test = "boot", R = R, level = level[1], type = "bca",
                       method = "covariance", robust = FALSE)

## compute summary
summary_boot <- summary(boot, type = "boot")
summary_data <- summary(boot, type = "data")

## retest with different parameters
boot_less <- retest(boot, alternative = "less", level = level[2])
boot_greater <- retest(boot, alternative = "greater", level = level[2])
boot_perc <- retest(boot, type = "perc", level = level[2])

## create data for plotting
ci <- setup_ci_plot(boot)
ci_perc <- setup_ci_plot(boot_perc, p_value = TRUE)
density <- setup_density_plot(boot)
ellipse <- setup_ellipse_plot(boot)

## stuff needed to check correctness
coef_names <- c("a", "b", "Direct", "Total", "ab")


## run tests

test_that("output has correct structure", {

  # bootstrap test
  expect_s3_class(boot, "boot_test_mediation")
  expect_s3_class(boot, "test_mediation")
  # regression fit
  expect_s3_class(boot$fit, "cov_fit_mediation")
  expect_s3_class(boot$fit, "fit_mediation")
  # bootstrap replicates
  expect_s3_class(boot$reps, "boot")

})

test_that("arguments are correctly passed", {

  # alternative hypothesis
  expect_identical(boot$alternative, "twosided")
  # number of bootstrap replicates
  expect_identical(boot$R, as.integer(R))
  # confidence level
  expect_identical(boot$level, level[1])
  # type of confidence intervals
  expect_identical(boot$type, "bca")
  # variable names
  expect_identical(boot$fit$x, "X")
  expect_identical(boot$fit$y, "Y")
  expect_identical(boot$fit$m, "M1")
  expect_identical(boot$fit$covariates, character())
  # nonrobust fit and test
  expect_false(boot$fit$robust)
  expect_null(boot$fit$control)

})

test_that("dimensions are correct", {

  # effects are scalars
  expect_length(boot$ab, 1L)
  # only one indirect effect, so only one confidence interval
  expect_length(boot$ci, 2L)
  # dimensions of bootstrap replicates
  d_boot <- dim(boot$reps$t)
  expect_identical(d_boot, c(as.integer(R), 7L))

})

test_that("values of coefficients are correct", {

  expect_equivalent(boot$ab, mean(boot$reps$t[, 1]))

})

test_that("output of coef() method has correct attributes", {

  coef_boot <- coef(boot, type = "boot")
  coef_data <- coef(boot, type = "data")
  # bootstrapped effects
  expect_length(coef_boot, 5L)
  expect_named(coef_boot, coef_names)
  # effects computed on original sample
  expect_length(coef_data, 5L)
  expect_named(coef_data, coef_names)

})

test_that("coef() method returns correct values of coefficients", {

  # bootstrapped effects
  expect_equivalent(coef(boot, parm = "a", type = "boot"), mean(boot$reps$t[, 3]))
  expect_equivalent(coef(boot, parm = "b", type = "boot"), mean(boot$reps$t[, 5]))
  expect_equivalent(coef(boot, parm = "Direct", type = "boot"), mean(boot$reps$t[, 6]))
  expect_equivalent(coef(boot, parm = "Total", type = "boot"), mean(boot$reps$t[, 7]))
  expect_equivalent(coef(boot, parm = "ab", type = "boot"), boot$ab)

  # effects computed on original sample
  expect_equivalent(coef(boot, parm = "a", type = "data"), boot$fit$a)
  expect_equivalent(coef(boot, parm = "b", type = "data"), boot$fit$b)
  expect_equivalent(coef(boot, parm = "Direct", type = "data"), boot$fit$direct)
  expect_equivalent(coef(boot, parm = "Total", type = "data"), boot$fit$total)
  expect_equivalent(coef(boot, parm = "ab", type = "data"), boot$fit$a * boot$fit$b)

})

test_that("output of confint() method has correct attributes", {

  ci_boot <- confint(boot, type = "boot")
  ci_data <- confint(boot, type = "data")
  # bootstrapped confidence intervals
  expect_equal(dim(ci_boot), c(5L, 2L))
  expect_equal(rownames(ci_boot), coef_names)
  expect_equal(colnames(ci_boot), c("5 %", "95 %"))
  # confidence intervals based on theory (except for indirect effect)
  expect_equal(dim(ci_data), c(5L, 2L))
  expect_equal(rownames(ci_data), coef_names)
  expect_equal(colnames(ci_data), c("5 %", "95 %"))

})

test_that("confint() method returns correct values of confidence intervals", {

  # bootstrapped confidence intervals
  expect_equivalent(confint(boot, parm = "ab", type = "boot"), boot$ci)
  # confidence intervals based on theory (except for indirect effect)
  expect_equivalent(confint(boot, parm = "ab", type = "data"), boot$ci)

})

test_that("summary has correct structure", {

  # summary
  expect_s3_class(summary_boot, "summary_test_mediation")
  expect_s3_class(summary_data, "summary_test_mediation")
  # original output of test for indirect effect
  expect_identical(summary_boot$object, boot)
  expect_identical(summary_data$object, boot)
  # summary of the model fit
  expect_s3_class(summary_boot$summary, "summary_cov_fit_mediation")
  expect_s3_class(summary_boot$summary, "summary_fit_mediation")
  expect_s3_class(summary_data$summary, "summary_cov_fit_mediation")
  expect_s3_class(summary_data$summary, "summary_fit_mediation")
  # regression standard error for model y ~ m + x
  expect_null(summary_boot$summary$s)
  expect_null(summary_data$summary$s)
  # R-squared for model y ~ m + x
  expect_null(summary_boot$summary$R2)
  expect_null(summary_data$summary$R2)
  # F-test for model y ~ m + x
  expect_null(summary_boot$summary$F_test)
  expect_null(summary_data$summary$F_test)
  # no plot is created
  expect_null(summary_boot$plot)
  expect_null(summary_data$plot)

})

test_that("attributes are correctly passed through summary", {

  # robustness
  expect_false(summary_boot$summary$robust)
  expect_false(summary_data$summary$robust)
  # number of observations
  expect_identical(summary_boot$summary$n, as.integer(n))
  expect_identical(summary_data$summary$n, as.integer(n))
  # variable names
  expect_identical(summary_boot$summary$x, "X")
  expect_identical(summary_boot$summary$y, "Y")
  expect_identical(summary_boot$summary$m, "M1")
  expect_null(summary_boot$summary$covariates)
  expect_identical(summary_data$summary$x, "X")
  expect_identical(summary_data$summary$y, "Y")
  expect_identical(summary_data$summary$m, "M1")
  expect_null(summary_data$summary$covariates)

})

test_that("effect summaries have correct names", {

  # a path
  expect_identical(dim(summary_boot$summary$a), c(1L, 5L))
  expect_identical(rownames(summary_boot$summary$a), "X")
  expect_identical(colnames(summary_boot$summary$a)[1:2], c("Data", "Boot"))
  expect_identical(dim(summary_data$summary$a), c(1L, 4L))
  expect_identical(rownames(summary_data$summary$a), "X")
  expect_identical(colnames(summary_data$summary$a)[1], "Estimate")
  # b path
  expect_identical(dim(summary_boot$summary$b), c(1L, 5L))
  expect_identical(rownames(summary_boot$summary$b), "M1")
  expect_identical(colnames(summary_boot$summary$b)[1:2], c("Data", "Boot"))
  expect_identical(dim(summary_data$summary$b), c(1L, 4L))
  expect_identical(rownames(summary_data$summary$b), "M1")
  expect_identical(colnames(summary_data$summary$b)[1], "Estimate")
  # direct effect
  expect_identical(dim(summary_boot$summary$direct), c(1L, 5L))
  expect_identical(rownames(summary_boot$summary$direct), "X")
  expect_identical(colnames(summary_boot$summary$direct)[1:2], c("Data", "Boot"))
  expect_identical(dim(summary_data$summary$direct), c(1L, 4L))
  expect_identical(rownames(summary_data$summary$direct), "X")
  expect_identical(colnames(summary_data$summary$direct)[1], "Estimate")
  # total effect
  expect_identical(dim(summary_boot$summary$total), c(1L, 5L))
  expect_identical(rownames(summary_boot$summary$total), "X")
  expect_identical(colnames(summary_boot$summary$total)[1:2], c("Data", "Boot"))
  expect_identical(dim(summary_data$summary$total), c(1L, 4L))
  expect_identical(rownames(summary_data$summary$total), "X")
  expect_identical(colnames(summary_data$summary$total)[1], "Estimate")
  # no model fits
  expect_null(summary_boot$summary$fit_mx)
  expect_null(summary_boot$summary$fit_ymx)
  expect_null(summary_data$summary$fit_mx)
  expect_null(summary_data$summary$fit_ymx)

})

test_that("effect summaries contain correct coefficient values", {

  # effects computed on original sample
  expect_identical(summary_boot$summary$a["X", "Data"], boot$fit$a)
  expect_identical(summary_boot$summary$b["M1", "Data"], boot$fit$b)
  expect_identical(summary_boot$summary$direct["X", "Data"], boot$fit$direct)
  expect_identical(summary_boot$summary$total["X", "Data"], boot$fit$total)
  expect_identical(summary_data$summary$a["X", "Estimate"], boot$fit$a)
  expect_identical(summary_data$summary$b["M1", "Estimate"], boot$fit$b)
  expect_identical(summary_data$summary$direct["X", "Estimate"], boot$fit$direct)
  expect_identical(summary_data$summary$total["X", "Estimate"], boot$fit$total)

  # bootstrapped effects
  expect_equal(summary_boot$summary$a["X", "Boot"], mean(boot$reps$t[, 3]))
  expect_equal(summary_boot$summary$b["M1", "Boot"], mean(boot$reps$t[, 5]))
  expect_equal(summary_boot$summary$direct["X", "Boot"], mean(boot$reps$t[, 6]))
  expect_equal(summary_boot$summary$total["X", "Boot"], mean(boot$reps$t[, 7]))

})

test_that("output of retest() has correct structure", {

  # bootstrap test
  expect_identical(class(boot_less), class(boot))
  expect_identical(class(boot_greater), class(boot))
  expect_identical(class(boot_perc), class(boot))
  # regression fit
  expect_identical(boot_less$fit, boot$fit)
  expect_identical(boot_greater$fit, boot$fit)
  expect_identical(boot_perc$fit, boot$fit)
  # bootstrap replicates
  expect_identical(boot_less$reps, boot$reps)
  expect_identical(boot_greater$reps, boot$reps)
  expect_identical(boot_perc$reps, boot$reps)

})

test_that("arguments of retest() are correctly passed", {

  # alternative hypothesis
  expect_identical(boot_less$alternative, "less")
  expect_identical(boot_greater$alternative, "greater")
  expect_identical(boot_perc$alternative, "twosided")
  # confidence level
  expect_identical(boot_less$level, level[2])
  expect_identical(boot_greater$level, level[2])
  expect_identical(boot_perc$level, level[2])
  # type of confidence intervals
  expect_identical(boot_less$type, "bca")
  expect_identical(boot_greater$type, "bca")
  expect_identical(boot_perc$type, "perc")
  # indirect effect
  expect_identical(boot_less$ab, boot$ab)
  expect_identical(boot_greater$ab, boot$ab)
  expect_identical(boot_perc$ab, boot$ab)
  # confidence interval for alternative = "less"
  expect_identical(length(boot_less$ci), length(boot$ci))
  expect_equivalent(boot_less$ci[1], -Inf)
  expect_equal(boot_less$ci[2], boot$ci[2])
  expect_identical(colnames(confint(boot_less)), c("Lower", "Upper"))
  # confidence interval for alternative = "greater"
  expect_identical(length(boot_greater$ci), length(boot$ci))
  expect_equal(boot_greater$ci[1], boot$ci[1])
  expect_equivalent(boot_greater$ci[2], Inf)
  expect_identical(colnames(confint(boot_greater)), c("Lower", "Upper"))
  # confidence interval for type = "perc"
  expect_identical(length(boot_perc$ci), length(boot$ci))

})

test_that("output of p_value() method has correct attributes", {

  digits <- 3
  p_boot <- p_value(boot_perc, type = "boot", digits = digits)
  p_data <- p_value(boot_perc, type = "data", digits = digits)
  # bootstrapped effects
  expect_length(p_boot, 5L)
  expect_named(p_boot, coef_names)
  expect_equal(p_boot["ab"], round(p_boot["ab"], digits = digits))
  # effects computed on original sample
  expect_length(p_data, 5L)
  expect_named(p_data, coef_names)
  expect_equal(p_data["ab"], round(p_data["ab"], digits = digits))

})

test_that("objects returned by setup_xxx_plot() have correct structure", {

  ## ci plot without p-value
  # check data frame for confidence interval
  expect_s3_class(ci$ci, "data.frame")
  # check dimensions
  expect_identical(dim(ci$ci), c(2L, 4L))
  # check column names
  expect_named(ci$ci, c("Effect", "Estimate", "Lower", "Upper"))
  # check that direct effect and indirect effect are plotted by default
  effect_names <- c("Direct", "ab")
  effect_factor <- factor(effect_names, levels = effect_names)
  expect_identical(ci$ci$Effect, effect_factor)
  # check confidence level
  expect_identical(ci$level, level[1])
  # check logical for multiple methods
  expect_false(ci$have_methods)

  ## ci plot with p-value
  # check data frame for confidence interval and p-value
  expect_s3_class(ci_perc$ci, "data.frame")
  expect_s3_class(ci_perc$p_value, "data.frame")
  # check dimensions
  expect_identical(dim(ci_perc$ci), c(2L, 5L))
  expect_identical(dim(ci_perc$p_value), c(2L, 3L))
  # check column names
  expect_named(ci_perc$ci, c("Label", "Effect", "Estimate", "Lower", "Upper"))
  expect_named(ci_perc$p_value, c("Label", "Effect", "Value"))
  # check that labels are correct
  label_names <- c("Confidence interval", "p-Value")
  expect_identical(ci_perc$ci$Label,
                   factor(rep.int(label_names[1], 2), levels = label_names))
  expect_identical(ci_perc$p_value$Label,
                   factor(rep.int(label_names[2], 2), levels = label_names))
  # check that direct effect and indirect effect are plotted by default
  effect_names <- c("Direct", "ab")
  effect_factor <- factor(effect_names, levels = effect_names)
  expect_identical(ci_perc$ci$Effect, effect_factor)
  expect_identical(ci_perc$p_value$Effect, effect_factor)
  # check confidence level
  expect_identical(ci$level, level[1])
  # check logical for multiple methods
  expect_false(ci$have_methods)

  ## density plot
  # check data frame for confidence interval
  expect_s3_class(density$density, "data.frame")
  # check dimensions
  expect_identical(ncol(density$density), 2L)
  expect_gt(nrow(density$density), 0L)
  # check column names
  expect_named(density$density, c("ab", "Density"))
  # check data frame confidence interval
  expect_s3_class(density$ci, "data.frame")
  # check dimensions
  expect_identical(dim(density$ci), c(1L, 3L))
  # check column names
  expect_named(density$ci, c("Estimate", "Lower", "Upper"))
  # check type of test
  expect_identical(density$test, "boot")
  # check confidence level
  expect_identical(density$level, level[1])
  # check logical for multiple effects
  expect_false(density$have_effect)
  # check logical for multiple methods
  expect_false(density$have_methods)

  ## ellipse plot
  expect_identical(ellipse, setup_ellipse_plot(boot$fit))

  ## weight plot
  expect_error(setup_weight_plot(boot))

})


# ## only implemented for simple mediation without covariates
#
# test_that("covariates not implemented", {
#
#   # run test with regression method
#   set.seed(seed)
#   suppressWarnings(
#     test_reg <- test_mediation(test_data, x = "X", y = "Y", m = "M1",
#                                covariates = c("C1", "C2"), test = "boot",
#                                R = 500, level = 0.9, type = "perc",
#                                method = "regression", robust = FALSE)
#   )
#
#   # try to run test with covariates (should give warning)
#   set.seed(seed)
#   expect_warning(
#     test_cov <- test_mediation(test_data, x = "X", y = "Y", m = "M1",
#                                covariates = c("C1", "C2"), test = "boot",
#                                R = 500, level = 0.9, type = "perc",
#                                method = "covariance", robust = FALSE)
#   )
#
#   # these should be the same
#   expect_equal(test_cov, test_reg)
#
# })
#
# test_that("multiple mediators not implemented", {
#
#   # run test with regression method
#   set.seed(seed)
#   suppressWarnings(
#     test_reg <- test_mediation(test_data, x = "X", y = "Y", m = c("M1", "M2"),
#                                test = "boot", R = 500, level = 0.9,
#                                type = "perc", method = "regression",
#                                robust = FALSE)
#   )
#
#   # try to run test with multiple mediators (should give warning)
#   set.seed(seed)
#   expect_warning(
#     test_cov <- test_mediation(test_data, x = "X", y = "Y", m = c("M1", "M2"),
#                                test = "boot", R = 500, level = 0.9,
#                                type = "perc", method = "covariance",
#                                robust = FALSE)
#   )
#
#   # these should be the same
#   expect_equal(test_cov, test_reg)
#
# })


# run mediation analysis through formula interface with data argument
set.seed(seed)
boot_f1 <- test_mediation(Y ~ m(M1) + X, data = test_data,
                          test = "boot", R = R, level = 0.9, type = "bca",
                          method = "covariance", robust = FALSE)
# run mediation analysis through formula interface without data argument
set.seed(seed)
boot_f2 <- test_mediation(Y ~ m(M1) + X,
                          test = "boot", R = R, level = 0.9, type = "bca",
                          method = "covariance", robust = FALSE)
# define mediator outside formula
med <- m(M1)
set.seed(seed)
boot_f3 <- test_mediation(Y ~ med + X, data = test_data,
                          test = "boot", R = R, level = 0.9, type = "bca",
                          method = "covariance", robust = FALSE)


test_that("formula interface works correctly", {

  # check that results are the same as with default method
  expect_equal(boot_f1, boot)
  expect_equal(boot_f2, boot)
  expect_equal(boot_f3, boot)

})
