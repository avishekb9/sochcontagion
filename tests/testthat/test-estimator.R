test_that("soch_fit_pair recovers known rates from a clean profile", {
  truth <- 3 * soch_scale_power(2.0, 0.2)
  fit <- soch_fit_pair(truth)
  expect_equal(fit$amin, 0.2, tolerance = 0.05)
  expect_equal(fit$amax, 2.0, tolerance = 0.10)
  expect_gt(fit$R2, 0.99)
})

test_that("soch_fit_pair returns the unordered pair (direction-invariant)", {
  f1 <- soch_fit_pair(3 * soch_scale_power(2.0, 0.2))
  f2 <- soch_fit_pair(3 * soch_scale_power(0.2, 2.0))
  expect_equal(f1$amin, f2$amin, tolerance = 1e-3)
  expect_equal(f1$amax, f2$amax, tolerance = 1e-3)
})

test_that("soch_classify splits at the median", {
  cl <- soch_classify(c(a = 3, b = 2, c = 0.5, d = 0.1))
  expect_equal(unname(cl), c("fast", "fast", "slow", "slow"))
})

test_that("soch_fit_market recovers a fast/slow ordering", {
  mk <- c("F1", "F2", "S1", "S2")
  rates <- c(F1 = 2.0, F2 = 1.8, S1 = 0.3, S2 = 0.15)
  prof <- list()
  for (a in mk) for (b in mk) if (a != b)
    prof[[length(prof) + 1]] <- list(i = a, j = b,
      wqte = 3 * soch_scale_power(rates[[a]], rates[[b]]))
  fm <- soch_fit_market(prof, mk)
  expect_true(all(fm$class[fm$market %in% c("S1", "S2")] == "slow"))
})
