#' Build All Directed WQTE Profiles
#'
#' Computes the wavelet-quantile directional scale profile for every ordered
#' pair among the chosen markets, returning the named list used by the SOCH
#' tests and the pooled estimator.
#'
#' @param returns Numeric returns matrix (columns named by market).
#' @param markets Character vector of markets.
#' @param tau Quantile level (default 0.05).
#' @param J Integer scales (default 5).
#' @param filter Wavelet filter (default \code{"la8"}).
#' @return A named list; element \code{"from|to"} is the numeric scale profile.
#' @examples
#' \donttest{
#' data(g20_returns)
#' P <- soch_profiles(g20_returns, c("USA","India","Germany"), tau = 0.05)
#' names(P)
#' }
#' @export
soch_profiles <- function(returns, markets, tau = 0.05, J = 5L, filter = "la8") {
  R <- as.matrix(returns)
  R <- R[stats::complete.cases(R[, markets, drop = FALSE]), markets, drop = FALSE]
  det <- lapply(markets, function(mk) modwt_detail(R[, mk], J = J, filter = filter))
  names(det) <- markets
  out <- list()
  for (s in markets) for (r in markets) if (s != r)
    out[[paste(s, r, sep = "|")]] <-
      vapply(seq_len(J), function(k) wq_gain(det[[s]][[k]], det[[r]][[k]], tau = tau),
             numeric(1))
  out
}

#' Full SOCH Empirical Pipeline
#'
#' Runs the complete Scale-Ordered Contagion empirical programme on a returns
#' panel: builds all directed WQTE profiles, runs the three SOCH tests
#' (ordering, magnitude, and---optionally---shape symmetry), and recovers
#' market-level adaptation rates with the pooled profile-matching estimator,
#' classifying each market as a fast or slow adapter.
#'
#' @param returns Numeric returns matrix (columns named by market).
#' @param advanced,emerging Character vectors partitioning the markets a priori
#'   (used only to build the slowness proxy and the magnitude comparison; under
#'   the theory the partition is itself a hypothesis tested by the recovered
#'   classification).
#' @param tau Quantile level (default 0.05).
#' @param J Integer scales (default 5).
#' @param symmetry Logical; run the (heavier) SOCH-B block-bootstrap test
#'   (default \code{FALSE}).
#' @param B Bootstrap replications for the symmetry test (default 200).
#' @return A list with \code{profiles}, \code{kstar}, \code{aggregate},
#'   \code{test_ordering}, \code{test_magnitude}, optionally
#'   \code{test_symmetry}, and \code{market_rates} (a data.frame of recovered
#'   rates and classification).
#' @references Bhandari, A., & Parida, I. (2026). Scale-Ordered Contagion.
#'   Working paper, IIT Bhubaneswar.
#' @examples
#' \donttest{
#' data(g20_returns); data(market_groups)
#' adv <- market_groups$advanced[1:3]; emg <- market_groups$emerging[1:3]
#' res <- soch_pipeline(g20_returns, adv, emg, tau = 0.05)
#' res$test_ordering$p_value
#' res$market_rates
#' }
#' @export
soch_pipeline <- function(returns, advanced, emerging, tau = 0.05, J = 5L,
                          symmetry = FALSE, B = 200L) {
  markets <- c(advanced, emerging)
  P <- soch_profiles(returns, markets, tau = tau, J = J)
  kstar <- vapply(P, which.max, integer(1))
  aggregate <- vapply(P, mean, numeric(1))
  prof_list <- lapply(names(P), function(k) {
    fr <- sub("\\|.*", "", k); to <- sub(".*\\|", "", k)
    list(i = fr, j = to, wqte = P[[k]])
  })
  res <- list(
    profiles = P, kstar = kstar, aggregate = aggregate,
    test_ordering = soch_test_ordering(P, emerging),
    test_magnitude = soch_test_magnitude(P, advanced, emerging),
    market_rates = soch_fit_market(prof_list, markets, bands = soch_bands(J))
  )
  if (symmetry)
    res$test_symmetry <- soch_test_symmetry(returns, markets, tau = tau, J = J, B = B)
  res
}
