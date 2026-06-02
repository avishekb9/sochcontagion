#' Daily Log-Returns of G20 Equity Markets
#'
#' Daily log-returns for eighteen G20 equity-market indices from
#' 12 January 2006 through 18 March 2026, used for the empirical tests of the
#' Scale-Ordered Contagion framework. Price levels are integrated of order one
#' and returns are stationary, so all transfer-entropy computation is on
#' returns.
#'
#' @format A numeric matrix with 5036 rows (trading days) and 18 columns
#'   (markets), the row names being ISO dates. Columns are
#'   Argentina, Australia, Brazil, Canada, China, France, Germany, India,
#'   Indonesia, Italy, Japan, Mexico, Russia, SouthAfrica, SouthKorea, Turkey,
#'   UK, USA.
#' @source Compiled from public sources (e.g. Yahoo Finance) for the G20
#'   equity indices.
#' @examples
#' data(g20_returns)
#' dim(g20_returns)
#' head(colnames(g20_returns))
"g20_returns"

#' Advanced / Emerging Market Grouping
#'
#' The a-priori advanced/emerging partition of the bundled markets, used to
#' build the slowness proxy and the directional magnitude comparison. Under the
#' Scale-Ordered Contagion theory this partition is a hypothesis to be tested
#' against the data-determined classification, not a maintained assumption.
#'
#' @format A list with two character vectors, \code{advanced} and
#'   \code{emerging}.
#' @examples
#' data(market_groups)
#' market_groups$advanced
"market_groups"
