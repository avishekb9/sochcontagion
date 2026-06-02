#' MODWT Scale Bands
#'
#' Returns the nominal MODWT detail pass-bands in angular frequency. With unit
#' sampling and Nyquist frequency \eqn{\pi}, detail level \eqn{k} has band
#' \eqn{[\pi 2^{-k},\, \pi 2^{-(k-1)}]}; for daily data, level 1 is the
#' two-to-four-day band and level 5 the thirty-two-to-sixty-four-day band.
#'
#' @param J Integer number of scales (default 5).
#' @return A numeric matrix with \code{J} rows and columns \code{wmin},
#'   \code{wmax} giving the lower and upper band edges in angular frequency.
#' @examples
#' soch_bands(5)
#' @export
soch_bands <- function(J = 5L) {
  J <- as.integer(J); stopifnot(J >= 1L)
  k <- seq_len(J)
  cbind(wmin = pi * 2^(-k), wmax = pi * 2^(-(k - 1)))
}

#' Product-Lorentzian Transmission Spectrum
#'
#' The power spectral density of the bi-exponential transmission response
#' implied by a source filter of rate \code{alpha_s} and a receiver filter of
#' rate \code{alpha_r}. It is the product of two Lorentzians and is symmetric
#' in the two rates; the slower rate sets the binding spectral corner.
#'
#' @param alpha_s Positive source adaptation rate.
#' @param alpha_r Positive receiver adaptation rate.
#' @param omega Numeric vector of non-negative angular frequencies.
#' @param A Level constant (default 1).
#' @return Numeric vector of spectral density values
#'   \eqn{A^2 \frac{\alpha_s^2}{\alpha_s^2+\omega^2}
#'   \frac{\alpha_r^2}{\alpha_r^2+\omega^2}}.
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' w <- seq(0, pi, length.out = 64)
#' s <- soch_spectrum(2.0, 0.2, w)
#' @export
soch_spectrum <- function(alpha_s, alpha_r, omega, A = 1) {
  stopifnot(alpha_s > 0, alpha_r > 0, all(omega >= 0))
  A^2 * (alpha_s^2 / (alpha_s^2 + omega^2)) * (alpha_r^2 / (alpha_r^2 + omega^2))
}

#' Closed-Form Wavelet Scale Power
#'
#' Integrates the product-Lorentzian spectrum over each MODWT band to give the
#' transmitted information power by scale, in closed form (partial fractions
#' plus the arctangent integral). For \eqn{\alpha_s \neq \alpha_r} the
#' antiderivative is used; the confluent case \eqn{\alpha_s = \alpha_r} is the
#' removable limit, evaluated by direct numerical integration. The profile is
#' symmetric in the two rates (the basis of the SOCH shape-symmetry
#' prediction) and strictly positive.
#'
#' @param alpha_s Positive source adaptation rate.
#' @param alpha_r Positive receiver adaptation rate.
#' @param bands A two-column matrix of band edges from \code{\link{soch_bands}};
#'   defaults to \code{soch_bands(5)}.
#' @param A Level constant (default 1).
#' @return Numeric vector of length \code{nrow(bands)}: the power \eqn{P_k} at
#'   each scale.
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' soch_scale_power(2.0, 0.2)                 # fast source, slow receiver
#' soch_scale_power(0.2, 2.0)                 # reverse: identical (symmetry)
#' @export
soch_scale_power <- function(alpha_s, alpha_r, bands = soch_bands(), A = 1) {
  stopifnot(alpha_s > 0, alpha_r > 0, ncol(bands) == 2L)
  if (abs(alpha_s - alpha_r) < 1e-8) {
    S <- function(w) soch_spectrum(alpha_s, alpha_r, w, A = A)
    return(apply(bands, 1L, function(b)
      stats::integrate(S, b[["wmin"]], b[["wmax"]])$value))
  }
  pref <- A^2 * alpha_s^2 * alpha_r^2 / (alpha_r^2 - alpha_s^2)
  br <- function(w) (1 / alpha_s) * atan(w / alpha_s) - (1 / alpha_r) * atan(w / alpha_r)
  apply(bands, 1L, function(b) pref * (br(b[["wmax"]]) - br(b[["wmin"]])))
}

#' Spectral Peak Frequency
#'
#' The angular frequency at which \eqn{\omega\, S(\omega)} (the quantity the
#' octave-band profile tracks) is maximised. It solves
#' \eqn{1 = 2\omega^2/(\alpha_s^2+\omega^2) + 2\omega^2/(\alpha_r^2+\omega^2)}
#' and lies in \eqn{[\alpha_\wedge/\sqrt 3,\ \alpha_\wedge]} where
#' \eqn{\alpha_\wedge=\min(\alpha_s,\alpha_r)}: in the symmetric case it equals
#' \eqn{\alpha/\sqrt 3}, and in the strongly asymmetric case it approaches the
#' slower rate.
#'
#' @param alpha_s Positive source adaptation rate.
#' @param alpha_r Positive receiver adaptation rate.
#' @return The peak angular frequency (scalar).
#' @examples
#' soch_peak_frequency(1, 1)        # symmetric: 1/sqrt(3)
#' soch_peak_frequency(2.0, 0.2)    # near the slow rate 0.2
#' @export
soch_peak_frequency <- function(alpha_s, alpha_r) {
  stopifnot(alpha_s > 0, alpha_r > 0)
  f <- function(w) 1 - 2 * w^2 / (alpha_s^2 + w^2) - 2 * w^2 / (alpha_r^2 + w^2)
  amin <- min(alpha_s, alpha_r)
  stats::uniroot(f, lower = amin / sqrt(3) * (1 - 1e-9),
                 upper = amin * (1 + 1e-9), extendInt = "yes")$root
}

#' Predicted Peak MODWT Scale
#'
#' The wavelet scale at which directed transfer entropy is predicted to peak,
#' \eqn{k^\star \approx \log_2(\pi/\alpha_\wedge)}, governed by the slower
#' market's adaptation rate (prediction SOCH-A). Smaller \eqn{\alpha_\wedge}
#' (a slower market) gives a larger, coarser peak scale.
#'
#' @param alpha_s Positive source adaptation rate.
#' @param alpha_r Positive receiver adaptation rate.
#' @param J Integer number of observed scales used to clip the result
#'   (default 5).
#' @return The (continuous) predicted peak scale, clipped to \code{[1, J]}.
#' @examples
#' soch_peak_scale(2.0, 0.2)
#' @export
soch_peak_scale <- function(alpha_s, alpha_r, J = 5L) {
  wstar <- soch_peak_frequency(alpha_s, alpha_r)
  k <- log2(pi / wstar)
  min(max(k, 1), J)
}
