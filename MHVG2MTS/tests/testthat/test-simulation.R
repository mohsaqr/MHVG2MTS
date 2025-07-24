library(testthat)
library(MHVG2MTS)

test_that("simulate_garch works", {
  omega <- c(0.1, 0.2)
  alpha <- matrix(c(0.1, 0.05, 0.05, 0.1), nrow = 2)
  beta <- matrix(c(0.8, 0.1, 0.1, 0.8), nrow = 2)
  varcov <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

  garch_data <- simulate_garch(t = 100, omega, alpha, beta, varcov)
  expect_equal(nrow(garch_data), 100)
  expect_equal(ncol(garch_data), 2)

  garch_data_burnin <- simulate_garch(t = 100, omega, alpha, beta, varcov, burnin = 50)
  expect_equal(nrow(garch_data_burnin), 100)
  expect_equal(ncol(garch_data_burnin), 2)
})

test_that("simulate_var works", {
  const <- c(0.5, 0.5)
  phi <- matrix(c(0.5, 0.2, 0.1, 0.6), nrow = 2)
  cov_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

  var_data <- simulate_var(t = 100, const, phi, cov_mat)
  expect_equal(nrow(var_data), 100)
  expect_equal(ncol(var_data), 2)

  var_data_burnin <- simulate_var(t = 100, const, phi, cov_mat, burnin = 100)
  expect_equal(nrow(var_data_burnin), 100)
  expect_equal(ncol(var_data_burnin), 2)
})

test_that("simulate_wn works", {
  cov_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  wn_data <- simulate_wn(t = 100, cov_mat)
  expect_equal(nrow(wn_data), 100)
  expect_equal(ncol(wn_data), 2)
})
