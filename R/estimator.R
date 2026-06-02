#' Profile-Matching Estimator (single pair)
#'
#' Recovers a pair's adaptation rates by fitting an observed wavelet-quantile
#' profile to the closed-form scale power \code{\link{soch_scale_power}} by
#' nonlinear least squares, concentrating out the level. Because the scale
#' power is symmetric in the two rates, a single directional profile identifies
#' only the \emph{unordered} pair \eqn{\{\alpha_\wedge,\alpha_\vee\}}; the
#' ordered assignment comes from the pooled fit
#' (\code{\link{soch_fit_market}}). Rates faster than the Nyquist frequency are
#' not recoverable from sampled data, so the search is confined to
#' \code{[LO, HI]} with \code{HI = pi}.
#'
#' @param wqte Numeric vector: an observed directed WQTE scale profile.
#' @param bands Band matrix from \code{\link{soch_bands}} (default matches the
#'   length of \code{wqte}).
#' @param LO,HI Lower and upper bounds on the searched rates (defaults
#'   \code{0.02} and \code{pi}, the resolvable range).
#' @param starts Optional matrix of log-rate start values (two columns); a
#'   sensible multi-start grid is used by default.
#' @return A list with \code{amin}, \code{amax} (recovered unordered rates),
#'   \code{theta} (level), \code{ssr}, \code{fitted}, \code{resid}, \code{R2},
#'   \code{kstar} (peak scale of \code{wqte}).
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' truth <- 3 * soch_scale_power(2.0, 0.2)
#' fit <- soch_fit_pair(truth)
#' c(amin = fit$amin, amax = fit$amax)
#' @export
soch_fit_pair <- function(wqte, bands = soch_bands(length(wqte)),
                          LO = 0.02, HI = pi, starts = NULL) {
  J <- length(wqte); stopifnot(J == nrow(bands), all(is.finite(wqte)))
  obj <- function(lr) {
    a_s <- exp(lr[1]); a_r <- exp(lr[2])
    pen <- 1e6 * sum(pmax(0, lr - log(HI))^2 + pmax(0, log(LO) - lr)^2)
    Q <- soch_scale_power(min(max(a_s, LO), HI), min(max(a_r, LO), HI), bands)
    denom <- sum(Q * Q)
    if (!is.finite(denom) || denom <= 0) return(1e18)
    theta <- sum(wqte * Q) / denom
    sum((wqte - theta * Q)^2) + pen
  }
  if (is.null(starts)) {
    g <- log(c(0.1, 0.3, 0.7, 1.5, 3.0))
    starts <- as.matrix(expand.grid(g, g))
  }
  best <- NULL
  for (i in seq_len(nrow(starts))) {
    fit <- tryCatch(stats::optim(starts[i, ], obj, method = "Nelder-Mead",
                                 control = list(reltol = 1e-10, maxit = 2000)),
                    error = function(e) NULL)
    if (!is.null(fit) && (is.null(best) || fit$value < best$value)) best <- fit
  }
  a1 <- min(max(exp(best$par[1]), LO), HI); a2 <- min(max(exp(best$par[2]), LO), HI)
  amin <- min(a1, a2); amax <- max(a1, a2)
  Q <- soch_scale_power(amin, amax, bands); theta <- sum(wqte * Q) / sum(Q * Q)
  fitted <- theta * Q
  list(amin = amin, amax = amax, theta = theta, ssr = best$value,
       fitted = fitted, resid = wqte - fitted,
       R2 = 1 - sum((wqte - fitted)^2) / sum((wqte - mean(wqte))^2),
       kstar = which.max(wqte), J = J)
}

#' Pooled Market-Level Adaptation Rates
#'
#' Estimates one adaptation rate per market by constraining every directed
#' pair to use its two markets' rates, with a per-pair level concentrated out.
#' Sharing each rate across all pairs containing that market resolves the
#' per-pair ordering ambiguity and yields a stable market-level estimate.
#'
#' @param profiles A list of directed profiles; each element is a list with
#'   \code{i} (source name), \code{j} (target name), and \code{wqte} (the
#'   numeric scale profile).
#' @param markets Character vector of market names (the rate vector order).
#' @param bands Band matrix (default \code{soch_bands(J)} for the profile
#'   length \code{J}).
#' @param start Optional numeric vector of log-rate start values
#'   (length \code{length(markets)}).
#' @return A \code{data.frame} with columns \code{market}, \code{alpha}
#'   (estimated rate), and \code{class} ("fast"/"slow" by the cross-sectional
#'   median).
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' \donttest{
#' data(g20_returns); data(market_groups)
#' mk <- c(market_groups$advanced[1:2], market_groups$emerging[1:2])
#' prof <- list()
#' for (a in mk) for (b in mk) if (a != b)
#'   prof[[length(prof)+1]] <- list(i = a, j = b,
#'     wqte = wqte_profile(g20_returns, a, b, tau = 0.05))
#' soch_fit_market(prof, mk)
#' }
#' @export
soch_fit_market <- function(profiles, markets, bands = NULL, start = NULL) {
  if (is.null(bands)) bands <- soch_bands(length(profiles[[1]]$wqte))
  M <- length(markets); idx <- stats::setNames(seq_len(M), markets)
  ssr_all <- function(lr) {
    a <- exp(lr); tot <- 0
    for (p in profiles) {
      Q <- soch_scale_power(a[idx[[p$i]]], a[idx[[p$j]]], bands)
      d <- sum(Q * Q); if (!is.finite(d) || d <= 0) return(1e18)
      th <- sum(p$wqte * Q) / d
      tot <- tot + sum((p$wqte - th * Q)^2)
    }
    tot
  }
  if (is.null(start)) start <- rep(log(0.7), M)
  fit <- stats::optim(start, ssr_all, method = "L-BFGS-B",
                      lower = log(0.02), upper = log(pi),
                      control = list(maxit = 5000, factr = 1e7))
  alpha <- stats::setNames(exp(fit$par), markets)
  data.frame(market = markets, alpha = as.numeric(alpha),
             class = soch_classify(as.numeric(alpha)), row.names = NULL)
}

#' Endogenous Fast/Slow Classification
#'
#' Classifies markets as fast or slow adapters by a cross-sectional median
#' split of estimated adaptation rates, replacing the exogenous
#' advanced/emerging partition with a data-determined one.
#'
#' @param alpha Numeric vector of estimated adaptation rates.
#' @return Character vector ("fast" if above the median, else "slow").
#' @examples
#' soch_classify(c(USA = 2.4, India = 0.29, China = 0.08, UK = 2.4))
#' @export
soch_classify <- function(alpha) {
  med <- stats::median(alpha)
  ifelse(alpha > med, "fast", "slow")
}
