library(testthat)
library(MHVG2MTS)

test_that("order_level works", {
  data <- data.frame(k = factor(c(3, 1, 2)), value = rnorm(3))
  ordered_data <- order_level(data)
  expect_equal(as.character(ordered_data$k), c("1", "2", "3"))
})

test_that("comp_distribution works", {
  seq_data <- c(1, 2, 2, 3, 3, 3)
  dist <- comp_distribution(seq_data)
  expect_equal(nrow(dist), 2)
  expect_equal(ncol(dist), 2)
  expect_equal(sum(dist$Freq), 1)
})

test_that("dist_degree works", {
  seq_data <- list(
    "A" = list(data.frame(V1 = c(1, 2, 2))),
    "B" = list(data.frame(V1 = c(1, 1, 2)))
  )
  model_names <- c("A", "B")
  distributions <- dist_degree(seq_data, model_names, n_inst = 1, col_idx = 1)
  expect_type(distributions, "list")
  expect_equal(length(distributions), 1)
  expect_equal(nrow(distributions[[1]]), 2)
  expect_equal(ncol(distributions[[1]]), 3)
})

test_that("mean_dist_degree works", {
  dist_data <- list(
    data.frame(k = factor(1:2), A = c(0.5, 0.5), B = c(0.2, 0.8)),
    data.frame(k = factor(1:2), A = c(0.6, 0.4), B = c(0.3, 0.7))
  )
  mean_dist <- mean_dist_degree(dist_data, n_inst = 2)
  expect_equal(nrow(mean_dist), 2)
  expect_equal(ncol(mean_dist), 3)
  expect_equal(mean_dist$A, c(0.55, 0.45))
})

test_that("sd_dist_degree works", {
  dist_data <- list(
    data.frame(k = factor(1:2), A = c(0.5, 0.5), B = c(0.2, 0.8)),
    data.frame(k = factor(1:2), A = c(0.6, 0.4), B = c(0.3, 0.7))
  )
  sd_dist <- sd_dist_degree(dist_data, n_inst = 2)
  expect_equal(nrow(sd_dist), 2)
  expect_equal(ncol(sd_dist), 3)
})
