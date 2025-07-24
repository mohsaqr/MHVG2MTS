#' Reorder a data frame by level
#'
#' @param data A data frame.
#' @return The reordered data frame.
#' @export
order_level <- function(data) {
  data <- data[order(as.integer(levels(data$k))), ]
  data$k <- factor(data$k, levels = sort(as.integer(levels(data$k))))
  data
}

#' Compute the distribution of a sequence
#'
#' @param seq_data A sequence.
#' @return A data frame with the distribution.
#' @export
comp_distribution <- function(seq_data) {
  dist <- prop.table(table(seq_data))

  if (names(dist)[1] == "1") {
    dist <- dist[-1]
  }

  as.data.frame(dist)
}

#' Compute the distribution of a given sample sequence
#'
#' @param seq_data A list of sequences.
#' @param model_names A vector of model names.
#' @param n_inst The number of instances.
#' @param col_idx The column index.
#' @return A list of data frames with the distributions.
#' @export
dist_degree <- function(seq_data, model_names, col_idx) {

  dists <- lapply(seq_data, function(model_data) {
    lapply(model_data, function(inst_data) {
      comp_distribution(inst_data[, col_idx])
    })
  })

  n_inst <- length(dists[[1]])

  join_dist <- lapply(1:n_inst, function(i) {

    inst_dists <- lapply(dists, function(model_dists) {
      model_dists[[i]]
    })

    merged_dist <- Reduce(function(x, y) merge(x, y, by = "seq_data", all = TRUE), inst_dists)
    colnames(merged_dist) <- c("k", model_names)
    order_level(merged_dist)
  })

  join_dist
}

#' Compute the mean of a distribution set
#'
#' @param dist_data A list of distribution data frames.
#' @param n_inst The number of instances.
#' @return A data frame with the mean distribution.
#' @export
summarise_dist_degree <- function(dist_data, summary_fun = "mean") {

  all_data <- do.call(rbind, dist_data)

  summary_fun <- match.fun(summary_fun)

  agg_data <- aggregate(. ~ k, data = all_data, FUN = summary_fun, na.rm = TRUE)

  order_level(agg_data)
}
