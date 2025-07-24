#' Read measures from a file or a set of files
#'
#' @param dir The directory where the file is located.
#' @param class_name The name of the class.
#' @param measure_name The name of the measure.
#' @param layer_A The first layer.
#' @param layer_B The second layer (optional).
#' @param i The index of the file (optional).
#' @return A data frame with the measures.
#' @export
read_measures <- function(dir, class_name, measure_name, layer_A, layer_B = 0, i = NULL) {
  layer_str <- ifelse(layer_B == 0, layer_A, paste0(layer_A, "-", layer_B))

  if (is.null(i)) {
    file_name <- file.path(dir, paste0(class_name, "__", layer_str, "_", measure_name, ".txt"))
  } else {
    file_name <- file.path(dir, paste0(class_name, "_", i, "__", layer_str, "_", measure_name, ".txt"))
  }

  data <- read.csv(file_name, header = FALSE)
  data <- as.data.frame(t(data))
  colnames(data) <- as.character(unlist(data[1, ]))
  data <- data[-1, ]

  data[] <- lapply(data, function(x) as.numeric(as.character(x)))

  if (!is.null(i)) {
    if (is.character(data)) {
      data <- data.frame(AvgDegree = as.numeric(data))
    } else {
      data$Class <- class_name
    }
  }

  data
}
