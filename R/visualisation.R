#' Plot Directed WQTE Scale Profiles
#'
#' Draws one or more directed wavelet-quantile scale profiles against MODWT
#' scale, for visual inspection of the rising/peaked shapes that the SOCH
#' predictions concern.
#'
#' @param profiles A named list of numeric scale profiles (\code{"from|to"}),
#'   e.g. a subset of \code{\link{soch_profiles}} output.
#' @param normalise Logical; plot profiles normalised to sum one (default
#'   \code{FALSE}).
#' @param col Optional vector of line colours (recycled).
#' @return Invisibly \code{NULL}; called for its plot.
#' @examples
#' P <- list("USA|India" = c(0.016,0.043,0.049,0.049,0.057),
#'           "India|USA" = c(0.006,0.027,0.045,0.022,0.042))
#' plot_scale_profiles(P)
#' @export
plot_scale_profiles <- function(profiles, normalise = FALSE, col = NULL) {
  if (normalise) profiles <- lapply(profiles, function(p) p / sum(p))
  J <- length(profiles[[1]])
  if (is.null(col)) col <- grDevices::hcl.colors(length(profiles), "Dark 3")
  col <- rep(col, length.out = length(profiles))
  ymax <- max(vapply(profiles, max, numeric(1))) * 1.05
  graphics::plot(NA, xlim = c(1, J), ylim = c(0, ymax), xaxt = "n",
                 xlab = "MODWT scale", ylab = if (normalise) "normalised power" else "WQTE gain",
                 bty = "l")
  graphics::axis(1, at = seq_len(J), labels = paste0("d", seq_len(J)))
  for (i in seq_along(profiles)) {
    graphics::lines(seq_len(J), profiles[[i]], col = col[i], lwd = 2)
    graphics::points(seq_len(J), profiles[[i]], col = col[i], pch = 19)
  }
  graphics::legend("topleft", bty = "n", lwd = 2, col = col, legend = names(profiles))
  invisible(NULL)
}

#' Plot the SOCH-B Shape-Symmetry Test
#'
#' Forest plot of each unordered pair's observed cross-direction divergence
#' against its same-shape bootstrap null, as returned by
#' \code{\link{soch_test_symmetry}}. Points inside the null bars do not reject
#' shape symmetry.
#'
#' @param test A \code{data.frame} from \code{\link{soch_test_symmetry}}.
#' @return Invisibly \code{NULL}; called for its plot.
#' @examples
#' tt <- data.frame(pair = c("USA-India","Germany-India"),
#'                  D_obs = c(0.076, 0.010), null_q95 = c(1.48, 1.88),
#'                  p_value = c(0.92, 1.00), holds = c(TRUE, TRUE))
#' plot_soch_symmetry(tt)
#' @export
plot_soch_symmetry <- function(test) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)
  d <- test[order(test$D_obs), , drop = FALSE]
  graphics::par(mar = c(4, 8, 1, 1))
  graphics::plot(NA, xlim = c(0, max(d$null_q95) * 1.02), ylim = c(1, nrow(d)),
                 yaxt = "n", xlab = "symmetric KL (normalised profiles)", ylab = "", bty = "l")
  graphics::segments(0, seq_len(nrow(d)), d$null_q95, seq_len(nrow(d)),
                     col = "grey60", lwd = 4, lend = 1)
  graphics::points(d$D_obs, seq_len(nrow(d)), pch = 19,
                   col = ifelse(d$holds, "#1f3b73", "#c0392b"))
  graphics::axis(2, at = seq_len(nrow(d)), labels = d$pair, las = 1, cex.axis = 0.7, tick = FALSE)
  invisible(NULL)
}

#' Plot Recovered Market Adaptation Rates
#'
#' Horizontal bar chart of estimated market adaptation rates with the
#' cross-sectional median split, as returned by \code{\link{soch_fit_market}}.
#'
#' @param rates A \code{data.frame} with columns \code{market}, \code{alpha},
#'   and optionally \code{class}, from \code{\link{soch_fit_market}}.
#' @param col Two colours for the fast/slow (or supplied) classes.
#' @return Invisibly \code{NULL}; called for its plot.
#' @examples
#' r <- data.frame(market = c("USA","UK","India","China"),
#'                 alpha = c(2.4, 2.4, 0.29, 0.08),
#'                 class = c("fast","fast","slow","slow"))
#' plot_market_rates(r)
#' @export
plot_market_rates <- function(rates, col = c("#1f3b73", "#c0392b")) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)
  d <- rates[order(rates$alpha), , drop = FALSE]
  cl <- if (!is.null(d$class)) d$class else soch_classify(d$alpha)
  bcol <- ifelse(cl == cl[which.max(d$alpha)], col[1], col[2])
  graphics::par(mar = c(4, 7, 1, 1))
  graphics::barplot(d$alpha, horiz = TRUE, names.arg = d$market, col = bcol,
                    border = NA, las = 1, xlab = expression(hat(alpha)[i]))
  graphics::abline(v = stats::median(d$alpha), lty = 2, col = "grey30")
  invisible(NULL)
}
