# sochcontagion 0.1.0

* First release. Implements the Scale-Ordered Contagion (SOCH) framework:
  * closed-form transmission spectrum and wavelet scale power
    (`soch_spectrum()`, `soch_scale_power()`, `soch_bands()`,
    `soch_peak_frequency()`, `soch_peak_scale()`);
  * wavelet-quantile directional-gain measure (`modwt_detail()`, `wq_gain()`,
    `wqte_profile()`);
  * profile-matching estimator and endogenous classification
    (`soch_fit_pair()`, `soch_fit_market()`, `soch_classify()`);
  * the three falsifiable tests (`soch_test_ordering()`,
    `soch_test_symmetry()`, `soch_test_magnitude()`) with phase-randomised
    surrogate and stationary block-bootstrap nulls;
  * one-call pipeline (`soch_pipeline()`, `soch_profiles()`) and plots
    (`plot_scale_profiles()`, `plot_soch_symmetry()`, `plot_market_rates()`);
  * bundled `g20_returns` and `market_groups`.
