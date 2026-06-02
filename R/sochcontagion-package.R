#' sochcontagion: Scale-Ordered Contagion in Financial Networks
#'
#' Tools for the spectral theory of financial contagion under heterogeneous
#' information adaptation (the Scale-Ordered Contagion, SOCH, framework). The
#' package implements the closed-form transmission spectrum and wavelet
#' scale-power profile, a profile-matching estimator of market adaptation
#' rates, the wavelet-quantile directional-gain measure, and the three
#' falsifiable SOCH tests (scale ordering, shape symmetry, magnitude
#' asymmetry), together with bundled G20 equity returns and a one-call
#' empirical pipeline.
#'
#' @section Main functions:
#' \describe{
#'   \item{Theory}{\code{\link{soch_scale_power}}, \code{\link{soch_spectrum}},
#'     \code{\link{soch_bands}}, \code{\link{soch_peak_frequency}},
#'     \code{\link{soch_peak_scale}}}
#'   \item{Measure}{\code{\link{modwt_detail}}, \code{\link{wq_gain}},
#'     \code{\link{wqte_profile}}}
#'   \item{Estimation}{\code{\link{soch_fit_pair}}, \code{\link{soch_fit_market}},
#'     \code{\link{soch_classify}}}
#'   \item{Tests}{\code{\link{soch_test_ordering}},
#'     \code{\link{soch_test_symmetry}}, \code{\link{soch_test_magnitude}}}
#'   \item{Pipeline and plots}{\code{\link{soch_pipeline}},
#'     \code{\link{plot_scale_profiles}}, \code{\link{plot_soch_symmetry}},
#'     \code{\link{plot_market_rates}}}
#' }
#'
#' @references
#' Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion: A Spectral
#' Theory of Heterogeneous Information Adaptation in Financial Networks.
#' Working paper, IIT Bhubaneswar.
#'
#' @keywords internal
"_PACKAGE"
