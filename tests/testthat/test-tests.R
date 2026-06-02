test_that("sym_kl is zero for identical shapes and positive otherwise", {
  expect_equal(sym_kl(c(1, 2, 3, 2, 1), c(1, 2, 3, 2, 1)), 0, tolerance = 1e-9)
  expect_gt(sym_kl(c(3, 2, 1), c(1, 2, 3)), 0)
})

test_that("phase_surrogate preserves length and returns a real series", {
  xs <- phase_surrogate(rnorm(256))
  expect_length(xs, 256L)
  expect_true(all(is.finite(xs)))
})

test_that("soch_test_ordering finds a positive slowness slope", {
  prof <- list("USA|UK" = c(5, 4, 3, 2, 1), "UK|USA" = c(5, 4, 3, 2, 1),
               "USA|India" = c(1, 2, 3, 4, 5), "India|USA" = c(1, 2, 3, 4, 5),
               "India|China" = c(1, 1, 2, 4, 6), "China|India" = c(1, 1, 2, 4, 6))
  r <- soch_test_ordering(prof, emerging = c("India", "China"))
  expect_gt(r$slope, 0)
  expect_true(is.finite(r$p_value))
})

test_that("soch_test_magnitude computes a directional ratio", {
  prof <- list("USA|India" = c(1, 2, 3, 4, 5) * 1.3, "India|USA" = c(1, 2, 3, 4, 5),
               "UK|China" = c(1, 2, 3, 3, 2) * 1.1,  "China|UK" = c(1, 2, 3, 3, 2))
  r <- soch_test_magnitude(prof, advanced = c("USA", "UK"),
                           emerging = c("India", "China"))
  expect_equal(r$frac_gt1, 1)
  expect_true(is.finite(r$median))
})
