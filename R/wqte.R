#' MODWT Wavelet Detail Coefficients
#'
#' Returns the brick-walled MODWT detail coefficients of a series at every
#' scale up to \code{J}, using the Daubechies least-asymmetric filter of
#' length 8 (LA8). The maximal-overlap discrete wavelet transform is
#' shift-invariant and time-aligned, which suits financial returns; see
#' Percival and Walden (2000).
#'
#' @param x Numeric vector (e.g. a return series).
#' @param J Integer number of detail levels (default 5).
#' @param filter Character wavelet filter (default \code{"la8"}); see
#'   \code{\link[waveslim]{modwt}}.
#' @return A list of length \code{J}; element \code{k} is the numeric vector of
#'   scale-\code{k} detail coefficients (boundary coefficients removed).
#' @references Percival, D. B., & Walden, A. T. (2000). \emph{Wavelet Methods
#'   for Time Series Analysis}. Cambridge University Press.
#' @examples
#' d <- modwt_detail(rnorm(1024), J = 5)
#' length(d)
#' @export
modwt_detail <- function(x, J = 5L, filter = "la8") {
  x <- as.numeric(x); x[!is.finite(x)] <- 0
  w <- waveslim::brick.wall(waveslim::modwt(x, wf = filter, n.levels = J), filter)
  ds <- grep("^d", names(w), value = TRUE)[seq_len(J)]
  lapply(ds, function(s) as.numeric(w[[s]]))
}

#' Wavelet-Quantile Directional Gain
#'
#' Measures directed tail dependence from a source coefficient series \code{x}
#' to a target series \code{y} as a Koenker-Machado quantile pseudo-\eqn{R^1}:
#' the proportional reduction in the \eqn{\tau}-quantile check loss when the
#' source's lagged value is added to a regression of the target's value on its
#' own lag. A value in \eqn{[0,1]}; larger means stronger directed information
#' flow at quantile \eqn{\tau}. This is the loadable realisation of the
#' wavelet-quantile transfer-entropy primitive and shares the
#' conditional-quantile-regression construction of that measure.
#'
#' @param x Numeric source series (typically a MODWT detail).
#' @param y Numeric target series (typically a MODWT detail).
#' @param tau Quantile level in (0,1) (default 0.05, the lower tail).
#' @return A scalar in \eqn{[0,1]}; \code{NA} if there are too few
#'   observations or the quantile regressions fail.
#' @references Koenker, R., & Bassett, G. (1978). Regression Quantiles.
#'   \emph{Econometrica}, 46(1), 33-50. \doi{10.2307/1913643}.
#'
#'   Koenker, R., & Machado, J. A. F. (1999). Goodness of Fit and Related
#'   Inference Processes for Quantile Regression. \emph{Journal of the American
#'   Statistical Association}, 94(448), 1296-1310.
#'   \doi{10.1080/01621459.1999.10473882}.
#'
#'   Schreiber, T. (2000). Measuring Information Transfer. \emph{Physical
#'   Review Letters}, 85(2), 461-464. \doi{10.1103/PhysRevLett.85.461}.
#' @examples
#' x <- rnorm(500); y <- 0.4 * c(0, x[-500]) + rnorm(500)
#' wq_gain(x, y, tau = 0.5)   # source improves the target's quantile fit
#' @export
wq_gain <- function(x, y, tau = 0.05) {
  stopifnot(tau > 0, tau < 1)
  m <- length(y); if (m < 51L) return(NA_real_)
  idx <- 2:m
  Y <- y[idx]; Ylag <- y[idx - 1]; Xlag <- x[idx - 1]
  ok <- is.finite(Y) & is.finite(Ylag) & is.finite(Xlag)
  Y <- Y[ok]; Ylag <- Ylag[ok]; Xlag <- Xlag[ok]
  if (length(Y) < 50L) return(NA_real_)
  rho <- function(u) sum(u * (tau - (u < 0)))
  f_full <- tryCatch(suppressWarnings(quantreg::rq(Y ~ Ylag + Xlag, tau = tau)),
                     error = function(e) NULL)
  f_rest <- tryCatch(suppressWarnings(quantreg::rq(Y ~ Ylag, tau = tau)),
                     error = function(e) NULL)
  if (is.null(f_full) || is.null(f_rest)) return(NA_real_)
  V1 <- rho(stats::residuals(f_full)); V0 <- rho(stats::residuals(f_rest))
  if (V0 <= 0) return(0)
  max(0, 1 - V1 / V0)
}

#' Wavelet-Quantile Directional Scale Profile
#'
#' Computes the per-scale wavelet-quantile directional gain from one market to
#' another: the directed WQTE profile \eqn{(P_1, \ldots, P_J)} across the
#' \eqn{J} wavelet scales, whose shape, ordering, and level the SOCH theory
#' predicts.
#'
#' @param returns Numeric matrix of returns (rows = time, columns = markets,
#'   with column names); or an object coercible by \code{as.matrix}.
#' @param from,to Market names (columns of \code{returns}) for the source and
#'   target.
#' @param tau Quantile level (default 0.05).
#' @param J Integer number of scales (default 5).
#' @param filter Wavelet filter (default \code{"la8"}).
#' @return Numeric vector of length \code{J}: the directed WQTE by scale.
#' @examples
#' data(g20_returns)
#' p <- wqte_profile(g20_returns, "USA", "India", tau = 0.05)
#' round(p, 4)
#' @export
wqte_profile <- function(returns, from, to, tau = 0.05, J = 5L, filter = "la8") {
  R <- as.matrix(returns)
  if (!all(c(from, to) %in% colnames(R)))
    stop("'from' and 'to' must be column names of 'returns'")
  R <- R[stats::complete.cases(R[, c(from, to)]), c(from, to), drop = FALSE]
  dx <- modwt_detail(R[, from], J = J, filter = filter)
  dy <- modwt_detail(R[, to],   J = J, filter = filter)
  vapply(seq_len(J), function(k) wq_gain(dx[[k]], dy[[k]], tau = tau), numeric(1))
}
