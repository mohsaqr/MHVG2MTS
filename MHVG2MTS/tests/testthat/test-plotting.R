library(testthat)
library(MHVG2MTS)
library(ggplot2)

test_that("plot_mts works", {
  mts_data <- data.frame(Time = 1:10, Series1 = rnorm(10), Series2 = rnorm(10))
  p <- plot_mts(mts_data, cols = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_acf works", {
  ts_data <- rnorm(100)
  p <- plot_acf(ts_data)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ccf works", {
  ts_a <- rnorm(100)
  ts_b <- rnorm(100)
  p <- plot_ccf(ts_a, ts_b)
  expect_s3_class(p, "ggplot")
})

test_that("plot_dist works", {
  dist_data <- data.frame(k = 1:10, value = rnorm(10), Variable = "A")
  p <- plot_dist(dist_data, cols = "red")
  expect_s3_class(p, "ggplot")
})

test_that("plot_boxplot_dists works", {
  freq_data <- data.frame(k = rep(1:5, 2), Freq = rnorm(10), Variable = rep(c("A", "B"), each = 5))
  p <- plot_boxplot_dists(freq_data, cols = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_pointplot_dists works", {
  freq_data <- data.frame(k = rep(1:5, 2), Freq = rnorm(10), Variable = rep(c("A", "B"), each = 5))
  p <- plot_pointplot_dists(freq_data, cols = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_ggplot_models works", {
  data <- data.frame(feature = rnorm(10), models = rep(c("A", "B"), each = 5))
  p <- plot_ggplot_models(data, models = data$models, level_models = c("A", "B"), feature = "feature", title = "My Plot", cols = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_pca works", {
  pca_data <- prcomp(iris[,1:4])
  p <- plot_pca(pca_data, true_classes = iris$Species, ncp = 2, col = c("red", "blue", "green"))
  expect_type(p, "list")
})

test_that("plot_boxplot_clust works", {
  df_data <- data.frame(k = rep(2:4, each = 5), Freq = rnorm(15))
  p <- plot_boxplot_clust(df_data)
  expect_s3_class(p, "ggplot")
})

test_that("plot_clusters works", {
  data <- iris[,1:4]
  true_classes <- iris$Species
  cluster_fit <- kmeans(data, 3)
  p <- plot_clusters(data, true_classes, cluster_fit, col = c("red", "blue", "green"))
  expect_s3_class(p, "ggplot")
})

test_that("multiplot works", {
  p1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
  p2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point()
  expect_error(multiplot(p1, p2, cols = 2), NA)
})
