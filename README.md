# sochcontagion

<!-- badges: start -->
[![R-CMD-check](https://github.com/avishekb9/sochcontagion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/avishekb9/sochcontagion/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

**Scale-Ordered Contagion (SOCH)** — a spectral theory of financial contagion
under heterogeneous information adaptation, with the estimators and falsifiable
tests it implies.

Modelling both the source and the receiving market as exponential information
filters yields a bi-exponential transmission response whose power spectrum is
the **product of two Lorentzians**; the *slower* market supplies the binding
spectral corner. Projected onto a maximal-overlap discrete wavelet basis this
gives a closed-form transfer-entropy-by-scale profile and three predictions:

* **SOCH-A** scale ordering — slower markets peak at coarser scales;
* **SOCH-B** shape symmetry — the normalised scale profile is the same in both
  directions (the sharpest, most novel restriction);
* **SOCH-C** magnitude asymmetry — direction shows up only in the level.

## Installation

```r
# install.packages("remotes")
remotes::install_github("avishekb9/sochcontagion")
```

## Quick start

```r
library(sochcontagion)
data(g20_returns); data(market_groups)

## directed wavelet-quantile profile (the verified USA -> India signature)
wqte_profile(g20_returns, "USA", "India", tau = 0.05)
#> rises through d4: the fast-source / slow-receiver signature (SOCH-A)

## the whole empirical programme in one call
adv <- c("USA","UK","Germany","Japan"); emg <- c("China","India","Brazil","SouthAfrica")
res <- soch_pipeline(g20_returns, adv, emg, tau = 0.05)
res$test_ordering$p_value     # SOCH-A
res$market_rates              # recovered adaptation rates + fast/slow class
```

The closed-form theory is in `soch_scale_power()` / `soch_spectrum()`; the
profile-matching estimator in `soch_fit_pair()` / `soch_fit_market()`; the three
tests in `soch_test_ordering()`, `soch_test_symmetry()`, `soch_test_magnitude()`.
See `vignette("methodology")` and `vignette("replication")`.

## Citation

Bhandari, A., & Parida, I. (2026). *Scale-Ordered Contagion: A Spectral Theory
of Heterogeneous Information Adaptation in Financial Networks.* Working paper,
IIT Bhubaneswar. See `citation("sochcontagion")`.

## License

GPL-3 © Avishek Bhandari and Ipsita Parida.
