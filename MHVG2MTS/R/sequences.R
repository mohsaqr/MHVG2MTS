#' Reorder a data frame by level
#'
#' @param data A data frame.
#' @return The reordered data frame.
#' @importFrom dplyr %>%
#' @export
order_level <- function(data) {
  data$k <- as.integer(levels(data$k))
  data <- data[order(data$k), ]
  data$k <- as.factor(data$k)
  data
}

#' Compute the distribution of a sequence
#'
#' @param seq_data A sequence.
#' @return A data frame with the distribution.
#' @importFrom dplyr %>%
#' @export
comp_distribution <- function(seq_data) {
  dist <- table(seq_data) / length(seq_data)

  if (names(dist)[1] == "1") {
    dist <- dist[2:length(dist)]
  }

  dist %>% as.data.frame.table()
}

#' Compute the distribution of a given sample sequence
#'
#' @param seq_data A list of sequences.
#' @param model_names A vector of model names.
#' @param n_inst The number of instances.
#' @param col_idx The column index.
#' @return A list of data frames with the distributions.
#' @importFrom dplyr full_join
#' @export
dist_degree <- function(seq_data, model_names, n_inst, col_idx) {
  dist <- list()
  for (m in 1:length(model_names)) {
    dist[[m]] <- list()
    for (i in 1:n_inst) {
      dist[[m]][[i]] <- comp_distribution(seq_data[[m]][[i]][, col_idx])
    }
  }
  names(dist) <- model_names

  join_dist <- list()
  for (i in 1:n_inst) {
    join_dist[[i]] <- dist[[1]][[i]]
    for (m in 2:length(model_names)) {
      aux <- dist[[m]][[i]]
      join_dist[[i]] <- full_join(join_dist[[i]], aux, by = c("seq_data"))
    }
  }

  for (i in 1:n_inst) {
    colnames(join_dist[[i]]) <- c("k", model_names)
    join_dist[[i]] <- order_level(join_dist[[i]])
  }

  join_dist
}

#' Compute the mean of a distribution set
#'
#' @param dist_data A list of distribution data frames.
#' @param n_inst The number of instances.
#' @return A data frame with the mean distribution.
#' @importFrom dplyr %>% group_by summarise_all
#' @export
mean_dist_degree <- function(dist_data, n_inst) {
  melt_dist <- dist_data[[1]]

  for (i in 2:n_inst) {
    melt_dist <- rbind(melt_dist, dist_data[[i]])
  }

  mean_dist <- melt_dist %>%
    group_by(k) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    as.data.frame()

  order_level(mean_dist)
}

#' Compute the standard deviation of a distribution set
#'
#' @param dist_data A list of distribution data frames.
#' @param n_inst The number of instances.
#' @return A data frame with the standard deviation of the distribution.
#' @importFrom dplyr %>% group_by summarise_all
#' @export
sd_dist_degree <- function(dist_data, n_inst) {
  melt_dist <- dist_data[[1]]

  for (i in 2:n_inst) {
    melt_dist <- rbind(melt_dist, dist_data[[i]])
  }

  sd_dist <- melt_dist %>%
    group_by(k) %>%
    summarise_all(sd, na.rm = TRUE) %>%
    as.data.frame()

  order_level(sd_dist)
}
