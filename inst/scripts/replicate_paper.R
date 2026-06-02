## replicate_paper.R - headline Scale-Ordered Contagion results on g20_returns.
library(sochcontagion)
data(g20_returns); data(market_groups)

adv <- c("USA", "UK", "Germany", "Japan")
emg <- c("China", "India", "Brazil", "SouthAfrica")

cat("USA->India:", round(wqte_profile(g20_returns, "USA", "India", 0.05), 4), "\n")
cat("India->USA:", round(wqte_profile(g20_returns, "India", "USA", 0.05), 4), "\n\n")

out <- soch_pipeline(g20_returns, adv, emg, tau = 0.05, symmetry = TRUE, B = 200)
cat("SOCH-A slope =", round(out$test_ordering$slope, 3),
    " p =", round(out$test_ordering$p_value, 3), "\n")
cat("SOCH-C median ratio =", round(out$test_magnitude$median, 3),
    " p =", round(out$test_magnitude$p_value, 3), "\n")
cat("SOCH-B holds:", sum(out$test_symmetry$holds), "/", nrow(out$test_symmetry), "\n\n")
print(out$market_rates[order(-out$market_rates$alpha), ])
