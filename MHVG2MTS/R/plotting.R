#' Plot a multivariate time series
#'
#' @param mts A mts object to plot.
#' @param cols A vector of colors for the series.
#' @param y_title The title for the y-axis.
#' @param x_title The title for the x-axis.
#' @param main_title The main title for the plot.
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
plot_mts <- function(mts, cols, y_title = "", x_title = "", main_title = "") {
  melt_mts <- melt(mts, id.vars = "Time")

  ggplot(melt_mts, aes(x = Time, y = value, group = variable, colour = variable)) +
    geom_line() +
    scale_color_manual(values = cols) +
    geom_point(size = 0.7) +
    theme_minimal() +
    labs(x = x_title, y = y_title, title = main_title)
}

#' Plot the ACF of a time series
#'
#' @param ts A time series object.
#' @param y_title The title for the y-axis.
#' @param x_title The title for the x-axis.
#' @param main_title The main title for the plot.
#' @param linf Lower limit for the y-axis.
#' @param lsup Upper limit for the y-axis.
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom forecast ggAcf
#' @export
plot_acf <- function(ts, y_title = "", x_title = "", main_title = "", linf = -1, lsup = -1) {
  g <- ggAcf(ts, lag.max = 15) +
    ggtitle(main_title) +
    theme_minimal() +
    labs(x = x_title, y = y_title)

  if (linf != lsup) {
    g <- g + ylim(linf, lsup)
  }

  g
}

#' Plot the CCF of two time series
#'
#' @param ts_a The first time series object.
#' @param ts_b The second time series object.
#' @param y_title The title for the y-axis.
#' @param x_title The title for the x-axis.
#' @param main_title The main title for the plot.
#' @param linf Lower limit for the y-axis.
#' @param lsup Upper limit for the y-axis.
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom forecast ggCcf
#' @export
plot_ccf <- function(ts_a, ts_b, y_title = "", x_title = "", main_title = "", linf = -1, lsup = -1) {
  g <- ggCcf(ts_a, ts_b, lag.max = 15) +
    theme_minimal() +
    labs(x = x_title, y = y_title, title = main_title)

  if (linf != lsup) {
    g <- g + ylim(linf, lsup)
  }

  g
}

#' Plot a distribution
#'
#' @param data A data frame.
#' @param type The type of plot, either "line" or "point".
#' @param cols A vector of colors.
#' @param y_title The title for the y-axis.
#' @param x_title The title for the x-axis.
#' @param main_title The main title for the plot.
#' @param is_smooth A boolean indicating whether to smooth the curve.
#' @param scale A string indicating the scale of the axes, can be "identity", "semilog-y", or "log-log".
#' @return A ggplot object.
#' @import ggplot2
#' @export
plot_distribution <- function(data, type = "line", cols = NULL, y_title = "", x_title = "", main_title = "",
                              is_smooth = FALSE, scale = "identity") {

  p <- ggplot(data, aes(x = k, y = value, group = Variable, colour = Variable)) +
    labs(x = x_title, y = y_title, title = main_title) +
    theme_minimal()

  if (type == "line") {
    p <- p + geom_line() + geom_point()
  } else if (type == "point") {
    p <- p + geom_point()
  }

  if (!is.null(cols)) {
    p <- p + scale_color_manual(values = cols)
  }

  if (is_smooth) {
    p <- p + geom_smooth(se = FALSE)
  }

  if (scale == "semilog-y") {
    p <- p + scale_y_log10()
  } else if (scale == "log-log") {
    p <- p + scale_y_log10() + scale_x_log10()
  }

  p
}

#' Boxplot of a dataframe of distributions
#'
#' @param freq_data A data frame of frequencies.
#' @param cols A vector of colors.
#' @param y_title The title for the y-axis.
#' @param x_title The title for the x-axis.
#' @param main_title The main title for the plot.
#' @return A ggplot object.
#' @import ggplot2
#' @export
plot_boxplot_dists <- function(freq_data, cols, y_title = "", x_title = "", main_title = "") {
  ggplot(data = freq_data, aes(y = Freq, x = k, fill = Variable)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = cols) +
    theme_minimal() +
    labs(x = x_title, y = y_title, title = main_title)
}


#' Plot boxplots by class models
#'
#' @param data A data frame with features and class.
#' @param models An array with class models.
#' @param level_models An array with the level/order of unique class models.
#' @param feature The column name of the feature to draw in the boxplot.
#' @param title The plot title.
#' @param cols An array of colors by unique class models.
#' @return A ggplot object.
#' @import ggplot2
#' @export
plot_ggplot_models <- function(data, models, level_models, feature, title, cols) {
  data$models2 <- factor(models, levels = level_models)

  ggplot(data, aes(x = models2, y = .data[[feature]], fill = models2)) +
    geom_boxplot(alpha = 0.7) +
    scale_x_discrete(name = "Model") +
    scale_y_continuous(name = title) +
    scale_fill_manual(values = cols) +
    theme_minimal()
}

#' Plot PCA
#'
#' @param pca A PCA object.
#' @param true_classes The true classes of the data.
#' @param ncp The number of components to use.
#' @param col A vector of colors.
#' @param title The plot title.
#' @return A list of ggplot objects.
#' @import ggplot2
#' @importFrom factoextra fviz_pca_biplot fviz_contrib
#' @importFrom corrplot corrplot
#' @export
plot_pca <- function(pca, true_classes, ncp, col, title = "MNet Features") {
  biplot <- fviz_pca_biplot(pca,
    palette = col,
    geom.ind = "point",
    fill.ind = true_classes,
    col.ind = "black",
    pointshape = 21,
    pointsize = 2,
    addEllipses = TRUE,
    labelsize = 5,
    title = paste0("PCA: ", title),
    col.var = "contrib",
    gradient.cols = c("#F0E009", "#DC5F96", "#604B70"),
    repel = TRUE,
    legend.title = list(fill = "Models", color = "Contrib")
  ) +
    theme_minimal()

  var <- get_pca_var(pca)
  corrplot <- corrplot(var$contrib[, 1:as.integer(ncp)],
    is.corr = FALSE, tl.srt = 45
  )

  barplot <- fviz_contrib(pca,
    choice = "var",
    axes = 1:3, top = 10,
    fill = "#48D1CC", color = "black"
  ) +
    ggtitle("Contribution of Variables to All Dimensions") +
    xlab(title) +
    theme_minimal()

  list(
    "pca_plot" = biplot,
    "corrplot" = corrplot,
    "bar_plot" = barplot
  )
}

#' Boxplot of clustering evaluation measures results
#'
#' @param df_data A data frame with clustering evaluation measures results.
#' @param y_title The title for the y-axis.
#' @param main_title The main title for the plot.
#' @return A ggplot object.
#' @import ggplot2
#' @export
plot_boxplot_clust <- function(df_data, y_title = "", main_title = "") {
  ggplot(data = df_data, aes(y = Freq, x = k)) +
    geom_boxplot() +
    scale_y_continuous(name = y_title) +
    scale_x_discrete(name = "Number of Clusters (k)") +
    ggtitle(main_title) +
    theme_minimal()
}

#' Jitterplot of the clusters
#'
#' @param data The data used for clustering.
#' @param true_classes The true classes of the data.
#' @param cluster_fit The cluster fit object.
#' @param col A vector of colors.
#' @param main_title The main title for the plot.
#' @return A ggplot object.
#' @import ggplot2
#' @export
plot_clusters <- function(data, true_classes, cluster_fit, col, main_title = "Cluster Analysis") {
  ggplot(data, aes(x = true_classes, y = cluster_fit$cluster, fill = true_classes)) +
    geom_boxplot(fill = "white") +
    geom_jitter(aes(color = true_classes), alpha = 0.4) +
    scale_x_discrete(name = "Model") +
    scale_y_continuous(name = "Cluster") +
    ggtitle(main_title) +
    scale_color_manual(values = col) +
    theme_minimal()
}

#' Draw multi-ggplots
#'
#' @param ... A list of ggplot objects.
#' @param plotlist A list of ggplot objects.
#' @param cols The number of columns in the layout.
#' @param layout A matrix specifying the layout.
#' @return A grid of plots.
#' @importFrom gridExtra grid.arrange
#' @export
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  plots <- c(list(...), plotlist)

  if (!is.null(layout)) {
    grid.arrange(grobs = plots, layout_matrix = layout)
  } else {
    grid.arrange(grobs = plots, ncol = cols)
  }
}
