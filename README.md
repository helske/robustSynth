
# robustSynth: Fast and Robust Synthetic Control Method

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/helske/robustSynth/graph/badge.svg)](https://app.codecov.io/gh/helske/robustSynth)
[![R-CMD-check](https://github.com/helske/robustSynth/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/helske/robustSynth/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Synthetic control method implementation of `robustSynth` uses multiple random 
initial values for predictor weights to improve the probability of convergence 
to the global optimum. In contrast to 
[`Synth`](https://cran.r-project.org/package=Synth) package, `robustSynth` 
uses a computationally more efficient implementation of quadratic programming 
solver of [`limSolve`](https://cran.r-project.org/package=limSolve) for donor 
weight optimization, and numerical optimization algorithms of 
[`nloptr`](https://cran.r-project.org/package=nloptr) for predictor weight 
optimization, defaulting for a derivative-free Sbplx algorithm of 
[NLOPT](http://github.com/stevengj/nlopt). 

To further improve the performance of `robustSynth`, the optimization by
multiple initial values can be parallelized using 
[`future`](https://CRAN.R-project.org/package=future), with automatic progress
reporting by [`progressr`](https://CRAN.R-project.org/package=progressr).

## Installation

You can install `robustSynth` from [CRAN](https://cran.r-project.org/) using
```{r}
install.packages("robustSynth")
```
and the development version from GitHub as
``` r
remotes::install_github("helske/robustSynth")
```

## Example

``` r
library(robustSynth)
# from ?Synth::synth
data(synth.data, package = "Synth")

d <- Synth::dataprep(
  synth.data,
  predictors = c("X1", "X2", "X3"),
  predictors.op = "mean",
  dependent = "Y",
  unit.variable = "unit.num",
  time.variable = "year",
  special.predictors = list(
    list("Y", 1991, "mean"), list("Y", 1985, "mean"), list("Y", 1980, "mean")
  ),
  treatment.identifier = 7,
  controls.identifier = c(29, 2, 13, 17, 32, 38),
  time.predictors.prior = c(1984:1989),
  time.optimize.ssr = c(1984:1990),
  unit.names.variable = "name",
  time.plot = 1984:1996
)
synth_out <- Synth::synth(d)

# for parallelization, use, e.g., future::plan(multisession, workers = 4)
if (interactive()) {
  # show progress bar
  progressr::handlers(global = TRUE)
}
set.seed(134)
robust_out <- scm(d, trials = 20)
synth_out$loss.v # 4.714688
robust_out$loss_v # 4.359649
results <- data.frame(
  time = d$tag$time.plot,
  synth = c(d$Y1plot - d$Y0plot %*% synth_out$solution.w),
  robust = robust_out$effect
)
results
   time      synth      robust
1  1984 -0.1686325  0.41177073
2  1985  1.0936597  1.06663678
3  1986  0.8502235  0.63079958
4  1987  3.3489866  3.42410376
5  1988 -1.4241479 -1.43152642
6  1989 -4.1947133 -3.86149267
7  1990 -0.4646250 -0.35705565
8  1991  0.3998366 -0.01646193
9  1992  8.1181380  4.90086789
10 1993 12.8906031 13.28067277
11 1994 15.9712216 17.42483926
12 1995 16.9174715 18.24576711
13 1996 22.9879423 24.00901200
```

