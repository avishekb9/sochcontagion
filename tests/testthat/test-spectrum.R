test_that("soch_bands has the right shape and edges", {
  b <- soch_bands(5)
  expect_equal(dim(b), c(5L, 2L))
  expect_equal(unname(b[1, ]), c(pi / 2, pi))
  expect_true(all(b[, "wmin"] < b[, "wmax"]))
})

test_that("scale power is symmetric in the two rates and positive", {
  expect_equal(soch_scale_power(2.0, 0.2), soch_scale_power(0.2, 2.0))
  expect_true(all(soch_scale_power(1.5, 0.4) > 0))
})

test_that("closed-form scale power matches numerical integration", {
  b <- soch_bands(5)
  cf <- soch_scale_power(2.0, 0.3, b)
  nm <- apply(b, 1, function(bb)
    stats::integrate(function(w) soch_spectrum(2.0, 0.3, w), bb[1], bb[2])$value)
  expect_equal(cf, nm, tolerance = 1e-6)
})

test_that("confluent case is handled (alpha_s == alpha_r)", {
  expect_true(all(is.finite(soch_scale_power(1.0, 1.0))))
})

test_that("peak frequency obeys the [amin/sqrt3, amin] bound", {
  w_sym <- soch_peak_frequency(1, 1)
  expect_equal(w_sym, 1 / sqrt(3), tolerance = 1e-4)
  for (a in c(0.3, 1.0)) for (b in c(0.3, 1.0, 2.0)) {
    amin <- min(a, b); w <- soch_peak_frequency(a, b)
    expect_gte(w, amin / sqrt(3) - 1e-6)
    expect_lte(w, amin + 1e-6)
  }
})

test_that("predicted peak scale is coarser for slower markets", {
  expect_gt(soch_peak_scale(2.0, 0.2), soch_peak_scale(2.0, 2.0))
})
