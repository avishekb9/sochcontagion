#' Symmetric Kullback-Leibler Divergence of Normalised Profiles
#'
#' The symmetrised KL divergence between two non-negative profiles after
#' normalising each to sum to one. Used to compare the \emph{shape} of two
#' directional scale profiles in the SOCH shape-symmetry test; smaller values
#' indicate more similar shapes. A small additive constant guards against zero
#' entries.
#'
#' @param p,q Numeric non-negative vectors of equal length.
#' @param eps Small stabiliser added before normalising (default 1e-9).
#' @return A non-negative scalar.
#' @examples
#' sym_kl(c(1, 2, 3, 2, 1), c(1, 2, 3, 2, 1))   # identical shapes: 0
#' sym_kl(c(3, 2, 1), c(1, 2, 3))               # reversed shapes: > 0
#' @export
sym_kl <- function(p, q, eps = 1e-9) {
  p <- p / sum(p); q <- q / sum(q)
  p <- (p + eps) / sum(p + eps); q <- (q + eps) / sum(q + eps)
  sum(p * log(p / q)) + sum(q * log(q / p))
}

#' Phase-Randomised Surrogate Series
#'
#' Generates a surrogate of a series that preserves its power spectrum (linear
#' autocorrelation) and is real-valued, by randomising the Fourier phases. Used
#' to build the directional-significance null for the wavelet-quantile measure
#' (a source surrogate breaks directed structure while preserving the source's
#' own spectrum); see Theiler et al. (1992).
#'
#' @param x Numeric vector.
#' @return A numeric vector the same length as \code{x}.
#' @references Theiler, J., Eubank, S., Longtin, A., Galdrikian, B., & Farmer,
#'   J. D. (1992). Testing for nonlinearity in time series: the method of
#'   surrogate data. \emph{Physica D}, 58(1-4), 77-94.
#'   \doi{10.1016/0167-2789(92)90102-S}.
#' @examples
#' xs <- phase_surrogate(rnorm(256))
#' length(xs)
#' @export
phase_surrogate <- function(x) {
  x <- as.numeric(x); n <- length(x)
  X <- stats::fft(x); amp <- Mod(X); ph <- Arg(X)
  half <- floor(n / 2)
  rp <- stats::runif(half - 1, -pi, pi)
  ph[2:half] <- rp
  ph[(n - half + 2):n] <- -rev(rp)
  Re(stats::fft(amp * exp(1i * ph), inverse = TRUE)) / n
}

# Internal: circular block-bootstrap index sequence of length n, block length L.
.block_idx <- function(n, L) {
  n <- as.integer(n); L <- as.integer(L)
  nb <- as.integer(ceiling(n / L))
  starts <- sample.int(n, nb, replace = TRUE)
  idx <- as.integer(vapply(starts,
                           function(s) ((s - 1L + 0:(L - 1L)) %% n) + 1L,
                           numeric(L)))
  idx[seq_len(n)]
}
