## ----include=FALSE-------------------------------------------------------
library("knitr")
options(prompt="R> ", continue = "+  ", width = 75, useFancyQuotes = FALSE)
opts_chunk$set(fig.path = "knitr-figures/figure-", fig.align = "center",
               fig.lp = "fig:", fig.pos = "t", tidy = FALSE)
render_sweave()             # use Sweave environments
set_header(highlight = "")  # do not use the Sweave.sty package

## ----eval=FALSE----------------------------------------------------------
#  install.packages("robmed")

## ----results='hide', message=FALSE, warning=FALSE------------------------
library("robmed")
data("BSG2014")

## ------------------------------------------------------------------------
keep <- c("ValueDiversity", "TaskConflict", "TeamCommitment", "TeamScore",
          "SharedLeadership", "AgeDiversity", "GenderDiversity",
          "ProceduralJustice", "InteractionalJustice", "TeamPerformance")
summary(BSG2014[, keep])

## ----eval=FALSE----------------------------------------------------------
#  TeamCommitment ~ m(TaskConflict) + ValueDiversity

## ----eval=FALSE----------------------------------------------------------
#  TeamScore ~ serial_m(TaskConflict, TeamCommitment) + ValueDiversity

## ----eval=FALSE----------------------------------------------------------
#  TeamPerformance ~ parallel_m(ProceduralJustice, InteractionalJustice) +
#    SharedLeadership + covariates(AgeDiversity, GenderDiversity)

## ------------------------------------------------------------------------
seed <- 20211117

## ------------------------------------------------------------------------
f_simple <- TeamCommitment ~ m(TaskConflict) + ValueDiversity

## ----cache=TRUE----------------------------------------------------------
set.seed(seed)
robust_boot_simple <- test_mediation(f_simple, data = BSG2014,
                                     robust = TRUE)
set.seed(seed)
ols_boot_simple <- test_mediation(f_simple, data = BSG2014,
                                  robust = FALSE)

## ----summary, fig.width=5, fig.height=4.5, out.width="0.7\\textwidth", fig.cap="Diagnostic plot of the regression weights from the robust bootstrap procedure of \\citet{alfonsXXa}.", fig.pos="b!"----
summary(robust_boot_simple)

## ----eval=FALSE----------------------------------------------------------
#  weight_plot(robust_boot_simple) +
#    scale_color_manual("", values = c("black", "#00BFC4")) +
#    theme(legend.position = "top")

## ------------------------------------------------------------------------
summary(ols_boot_simple, type = "data")

## ------------------------------------------------------------------------
coef(robust_boot_simple)
confint(robust_boot_simple)

## ------------------------------------------------------------------------
coef(ols_boot_simple, type = "data")
confint(ols_boot_simple, type = "data")

## ------------------------------------------------------------------------
coef(robust_boot_simple, parm = "Indirect")
confint(robust_boot_simple, parm = "Indirect")

## ----cache=TRUE----------------------------------------------------------
p_value(robust_boot_simple, parm = "Indirect")
p_value(ols_boot_simple, parm = "Indirect")

## ------------------------------------------------------------------------
boot_list <- list("OLS bootstrap" = ols_boot_simple,
                  "ROBMED" = robust_boot_simple)

## ----density, fig.width=5, fig.height=3.75, out.width="0.7\\textwidth", fig.cap="Density plot of the bootstrap distributions of the indirect effect, obtained via the OLS bootstrap and the robust bootstrap procedure of \\citet{alfonsXXa}.  The vertical lines indicate the the respective point estimates of the indirect effect and the shaded areas represent the confidence intervals.", fig.pos="t!"----
density_plot(boot_list)

## ----ci, fig.width=6, fig.height=4, out.width="0.85\\textwidth", fig.cap="Point estimates and 95\\% confidence intervals for selected effects in the mediation model, estimated via the OLS bootstrap and the robust bootstrap procedure of \\citet{alfonsXXa}.", fig.pos="t!"----
ci_plot(boot_list, parm = c("a", "b", "Direct", "Indirect"))

## ----ellipse, fig.width=5, fig.height=3.5, out.width="0.7\\textwidth", fig.cap="Diagnostic plot with tolerance ellipses for the OLS bootstrap and the robust bootstrap procedure of \\citet{alfonsXXa}.", fig.pos="b!"----
ellipse_plot(boot_list, horizontal = "ValueDiversity",
             vertical = "TaskConflict")

## ----ellipse-custom, fig.width=5, fig.height=3.5, out.width="0.7\\textwidth", fig.cap="Customized diagnostic plot with tolerance ellipses but without regression lines for the OLS bootstrap and the robust bootstrap procedure of \\citet{alfonsXXa}."----
setup <- setup_ellipse_plot(boot_list, horizontal = "ValueDiversity",
                            vertical = "TaskConflict")
ggplot() +
  geom_path(aes(x = x, y = y, color = Method), data = setup$ellipse) +
  geom_point(aes(x = x, y = y, fill = Weight), data = setup$data,
             shape = 21) +
  scale_fill_gradient(limits = 0:1, low = "white", high = "black") +
  labs(x = setup$horizontal, y = setup$vertical)

## ------------------------------------------------------------------------
f_serial <- TeamScore ~ serial_m(TaskConflict, TeamCommitment) +
  ValueDiversity

## ----cache=TRUE----------------------------------------------------------
set.seed(seed)
robust_boot_serial <- test_mediation(f_serial, data = BSG2014,
                                     robust = TRUE)
set.seed(seed)
ols_boot_serial <- test_mediation(f_serial, data = BSG2014,
                                  robust = FALSE)

## ------------------------------------------------------------------------
robust_boot_serial
ols_boot_serial

## ----weight, fig.width=5, fig.height=5.5, out.width="0.7\\textwidth", fig.cap="Diagnostic plot of the regression weights from the robust bootstrap procedure of \\citet{alfonsXXa} in the example for a serial multiple mediator model."----
weight_plot(robust_boot_serial) +
  scale_color_manual("", values = c("black", "#00BFC4")) +
  theme(legend.position = "top")

## ------------------------------------------------------------------------
f_parallel <-
  TeamPerformance ~ parallel_m(ProceduralJustice, InteractionalJustice) +
  SharedLeadership + covariates(AgeDiversity, GenderDiversity)

## ----cache=TRUE----------------------------------------------------------
set.seed(seed)
robust_boot_parallel <- test_mediation(f_parallel, data = BSG2014,
                                       robust = TRUE)
set.seed(seed)
ols_boot_parallel <- test_mediation(f_parallel, data = BSG2014,
                                    robust = FALSE)

## ------------------------------------------------------------------------
robust_boot_parallel
ols_boot_parallel

## ----ellipse-partial, fig.width=5, fig.height=3.5, out.width="0.7\\textwidth", fig.cap="Diagnostic plot with a tolerance ellipse for partial residuals in a multiple parallel mediator model."----
ellipse_plot(robust_boot_parallel, horizontal = "SharedLeadership",
             vertical = "TeamPerformance", partial = TRUE)

## ----cache=TRUE----------------------------------------------------------
set.seed(seed)
test_mediation(f_parallel, data = BSG2014, contrast = "absolute")

## ------------------------------------------------------------------------
retest(robust_boot_parallel, contrast = "absolute")

## ------------------------------------------------------------------------
summary(robust_boot_serial, plot = FALSE)

## ----summary-parallel, fig.width=5, fig.height=5.5, out.width="0.7\\textwidth", fig.cap="Diagnostic plot of the regression weights from the robust bootstrap procedure of \\citet{alfonsXXa} in the example for a parallel multiple mediator model.", fig.pos="t!"----
summary(robust_boot_parallel)

