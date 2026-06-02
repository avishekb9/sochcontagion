# =============================================================================
#  DATASET.R - build the bundled .rda files in /data from the source XLSX in
#  /inst/extdata. Run once from the package root:  Rscript data-raw/DATASET.R
# =============================================================================
suppressMessages(library(readxl))

PKG_ROOT <- if (file.exists(file.path(getwd(), "DESCRIPTION"))) getwd() else
            normalizePath(file.path(getwd(), ".."))

# --- 1. G20 returns (numeric matrix, ISO-date row names) ---------------------
g20_path <- file.path(PKG_ROOT, "inst", "extdata", "G20.xlsx")
g20 <- suppressMessages(read_excel(g20_path))
dts <- as.Date(g20[[1]], format = "%d/%m/%Y")
M <- as.matrix(g20[, -1]); storage.mode(M) <- "double"
ok <- !is.na(dts) & stats::complete.cases(M)
g20_returns <- M[ok, , drop = FALSE]
rownames(g20_returns) <- as.character(dts[ok])

# --- 2. Advanced / emerging grouping ----------------------------------------
market_groups <- list(
  advanced = c("USA", "UK", "Germany", "Japan", "Canada", "France", "Italy", "Australia"),
  emerging = c("China", "India", "Brazil", "Russia", "SouthAfrica",
               "Mexico", "Indonesia", "Turkey", "SouthKorea", "Argentina")
)
# keep only groups present as columns
market_groups$advanced <- intersect(market_groups$advanced, colnames(g20_returns))
market_groups$emerging <- intersect(market_groups$emerging, colnames(g20_returns))

dir.create(file.path(PKG_ROOT, "data"), showWarnings = FALSE)
save(g20_returns, file = file.path(PKG_ROOT, "data", "g20_returns.rda"),
     compress = "xz")
save(market_groups, file = file.path(PKG_ROOT, "data", "market_groups.rda"),
     compress = "xz")
cat(sprintf("g20_returns: %d x %d ; market_groups: %d advanced + %d emerging\n",
            nrow(g20_returns), ncol(g20_returns),
            length(market_groups$advanced), length(market_groups$emerging)))
