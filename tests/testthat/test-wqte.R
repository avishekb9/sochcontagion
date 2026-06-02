test_that("modwt_detail returns J scales", {
  d <- modwt_detail(rnorm(1024), J = 5)
  expect_length(d, 5L)
  expect_true(all(vapply(d, is.numeric, logical(1))))
})

test_that("wq_gain is in [0,1] and detects a planted directed link", {
  set.seed(1)
  x <- rnorm(800); y <- 0.5 * c(0, x[-800]) + rnorm(800)
  g <- wq_gain(x, y, tau = 0.5)
  expect_true(is.finite(g) && g >= 0 && g <= 1)
  expect_gt(g, wq_gain(rnorm(800), rnorm(800), tau = 0.5))
})

test_that("wq_gain returns NA on too-few observations", {
  expect_true(is.na(wq_gain(rnorm(10), rnorm(10))))
})

test_that("wqte_profile reproduces the USA->India signature", {
  skip_if_not(exists("g20_returns"))
  data(g20_returns, envir = environment())
  p <- wqte_profile(g20_returns, "USA", "India", tau = 0.05)
  expect_length(p, 5L)
  expect_true(all(diff(p[1:4]) > 0))            # rises d1 -> d4
  expect_gt(mean(p), mean(wqte_profile(g20_returns, "India", "USA", tau = 0.05)))
})
