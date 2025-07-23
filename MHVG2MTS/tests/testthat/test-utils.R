library(testthat)
library(MHVG2MTS)

test_that("melt_variables works", {
  df1 <- data.frame(k = 1:2, Y_1 = c(0.5, 0.5))
  df2 <- data.frame(k = 1:2, Y_2 = c(0.2, 0.8))
  melted_df <- melt_variables(df1, df2)
  expect_equal(nrow(melted_df), 4)
  expect_equal(ncol(melted_df), 3)
})

test_that("df_dists works", {
  list_data <- list(data.frame(k = 1:2, A = c(0.5, 0.5)), data.frame(k = 1:2, A = c(0.6, 0.4)))
  model_names <- c("A")
  dists <- df_dists(list_data, model_names, n_inst = 2, k_f = 1, k_l = 2)
  expect_type(dists, "list")
  expect_equal(length(dists), 1)
  expect_equal(nrow(dists[[1]]), 2)
  expect_equal(ncol(dists[[1]]), 2)
})

test_that("freq_df works", {
  df_data <- data.frame("2" = c(0.5, 0.6), "3" = c(0.5, 0.4))
  freq <- freq_df(df_data)
  expect_equal(nrow(freq), 4)
  expect_equal(ncol(freq), 2)
})

test_that("comp_pca works", {
  data <- iris[,1:4]
  pca_results <- comp_pca(data)
  expect_type(pca_results, "list")
  expect_s3_class(pca_results$pca, "prcomp")
})

test_that("comp_clusters works", {
  data_redim <- iris[,1:4]
  k <- 3
  true_classes <- iris$Species
  clusters <- comp_clusters(data_redim, k, true_classes)
  expect_type(clusters, "list")
  expect_equal(length(clusters$cluster_fit$size), 3)
})
