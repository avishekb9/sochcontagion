#' Test SOCH-A: Scale Ordering by Adaptation Speed
#'
#' Tests the prediction that pairs containing slower markets peak at coarser
#' wavelet scales. For each directed profile the peak scale \eqn{k^\star} is
#' located, and \eqn{k^\star} is regressed on a slowness proxy (the number of
#' emerging markets in the pair). A positive slope supports SOCH-A.
#'
#' @param profiles A named list of directed WQTE profiles; names are
#'   \code{"from|to"} and elements are numeric scale vectors (e.g. from
#'   \code{\link{soch_profiles}}).
#' @param emerging Character vector of market names treated as the slow group.
#' @return A list with the fitted \code{model} (an \code{lm}), the
#'   \code{slope}, its \code{t} statistic and \code{p_value}, and the mean
#'   peak scale by slowness level (\code{means}).
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' ## toy profiles: emerging-inclusive pairs peak coarser
#' set.seed(1)
#' prof <- list("USA|UK" = c(5,4,3,2,1), "UK|USA" = c(5,4,3,2,1),
#'              "USA|India" = c(1,2,3,4,5), "India|USA" = c(1,2,3,4,5),
#'              "India|China" = c(1,1,2,4,6), "China|India" = c(1,1,2,4,6))
#' soch_test_ordering(prof, emerging = c("India","China"))$p_value
#' @export
soch_test_ordering <- function(profiles, emerging) {
  nm <- names(profiles)
  fr <- sub("\\|.*", "", nm); to <- sub(".*\\|", "", nm)
  kstar <- vapply(profiles, which.max, integer(1))
  slowness <- (fr %in% emerging) + (to %in% emerging)
  m <- stats::lm(kstar ~ slowness)
  cs <- summary(m)$coefficients
  list(model = m, slope = unname(cs["slowness", 1]),
       t = unname(cs["slowness", 3]), p_value = unname(cs["slowness", 4]),
       means = tapply(kstar, slowness, mean))
}

#' Test SOCH-C: Directional Magnitude Asymmetry
#'
#' Tests the prediction that advanced-to-emerging flows dominate
#' emerging-to-advanced flows in \emph{level}. For each advanced-emerging pair
#' the ratio of aggregate WQTE in the two directions is formed and a one-sided
#' sign test assesses whether the ratio exceeds one.
#'
#' @param profiles A named list of directed WQTE profiles (\code{"from|to"}).
#' @param advanced,emerging Character vectors of market names.
#' @return A list with the per-pair \code{ratios}, their \code{median},
#'   \code{mean}, the fraction exceeding one (\code{frac_gt1}), and the
#'   one-sided sign-test \code{p_value}.
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' prof <- list("USA|India" = c(1,2,3,4,5)*1.3, "India|USA" = c(1,2,3,4,5),
#'              "UK|China" = c(1,2,3,3,2)*1.1,  "China|UK" = c(1,2,3,3,2))
#' soch_test_magnitude(prof, advanced = c("USA","UK"),
#'                            emerging = c("India","China"))$frac_gt1
#' @export
soch_test_magnitude <- function(profiles, advanced, emerging) {
  agg <- vapply(profiles, mean, numeric(1))
  nm <- names(agg)
  ratios <- c()
  for (a in advanced) for (e in emerging) {
    k1 <- paste(a, e, sep = "|"); k2 <- paste(e, a, sep = "|")
    if (k1 %in% nm && k2 %in% nm && is.finite(agg[[k1]]) &&
        is.finite(agg[[k2]]) && agg[[k2]] > 0)
      ratios <- c(ratios, agg[[k1]] / agg[[k2]])
  }
  n_gt <- sum(ratios > 1)
  list(ratios = ratios, median = stats::median(ratios), mean = mean(ratios),
       frac_gt1 = mean(ratios > 1),
       p_value = stats::binom.test(n_gt, length(ratios),
                                   alternative = "greater")$p.value)
}

#' Test SOCH-B: Shape Symmetry Across Direction
#'
#' Tests the prediction---the sharpest of the three---that the \emph{shape} of
#' the directional scale profile is the same in both directions for a given
#' pair. For each unordered pair the cross-direction symmetric KL of the
#' normalised profiles is compared to a same-shape null built by
#' stationary-block-bootstrap re-estimation of a single direction (the sampling
#' variability of one shape). A large \eqn{p} means the two directions are
#' statistically indistinguishable, supporting SOCH-B.
#'
#' @param returns Numeric returns matrix (columns named by market).
#' @param markets Character vector of markets; all unordered pairs are tested.
#' @param tau Quantile level (default 0.05).
#' @param J Integer scales (default 5).
#' @param B Integer bootstrap replications per direction (default 200).
#' @param L Integer block length for the stationary bootstrap (default 22).
#' @param filter Wavelet filter (default \code{"la8"}).
#' @return A \code{data.frame} with one row per unordered pair: \code{pair},
#'   \code{D_obs}, \code{null_q95}, \code{p_value}, and \code{holds}
#'   (\code{p_value > 0.05}).
#' @references Politis, D. N., & Romano, J. P. (1994). The Stationary
#'   Bootstrap. \emph{Journal of the American Statistical Association},
#'   89(428), 1303-1313. \doi{10.1080/01621459.1994.10476870}.
#' @examples
#' \donttest{
#' data(g20_returns)
#' soch_test_symmetry(g20_returns, c("USA","India","Germany"),
#'                    tau = 0.05, B = 50)
#' }
#' @export
soch_test_symmetry <- function(returns, markets, tau = 0.05, J = 5L,
                               B = 200L, L = 22L, filter = "la8") {
  R <- as.matrix(returns)
  R <- R[stats::complete.cases(R[, markets, drop = FALSE]), markets, drop = FALSE]
  n <- nrow(R)
  pdir <- function(xi, xj) {
    dx <- modwt_detail(xi, J = J, filter = filter)
    dy <- modwt_detail(xj, J = J, filter = filter)
    vapply(seq_len(J), function(k) wq_gain(dx[[k]], dy[[k]], tau = tau), numeric(1))
  }
  cmb <- utils::combn(markets, 2)
  out <- vector("list", ncol(cmb))
  for (c in seq_len(ncol(cmb))) {
    i <- cmb[1, c]; j <- cmb[2, c]; xi <- R[, i]; xj <- R[, j]
    Dobs <- sym_kl(pdir(xi, xj), pdir(xj, xi))
    Pij <- matrix(NA_real_, B, J); Pji <- matrix(NA_real_, B, J)
    for (b in seq_len(B)) {
      ix <- .block_idx(n, L)
      Pij[b, ] <- pdir(xi[ix], xj[ix]); Pji[b, ] <- pdir(xj[ix], xi[ix])
    }
    nullKL <- c(vapply(seq_len(B - 1), function(b) sym_kl(Pij[b, ], Pij[b + 1, ]), numeric(1)),
                vapply(seq_len(B - 1), function(b) sym_kl(Pji[b, ], Pji[b + 1, ]), numeric(1)))
    p <- mean(nullKL >= Dobs)
    out[[c]] <- data.frame(pair = paste(i, j, sep = "-"), D_obs = Dobs,
                           null_q95 = stats::quantile(nullKL, 0.95, names = FALSE),
                           p_value = p, holds = p > 0.05)
  }
  do.call(rbind, out)
}
