#' Read measures from a file
#'
#' @param dir The directory where the file is located.
#' @param class_name The name of the class.
#' @param tmeasure_name The name of the measure.
#' @param layer_A The first layer.
#' @param layer_B The second layer (optional).
#' @return A data frame with the measures.
#' @export
read_measures <- function(dir, class_name, tmeasure_name, layer_A, layer_B = 0) {
  if (layer_B == 0) {
    file_name <- file.path(dir, paste0(class_name, "__", layer_A, "_", tmeasure_name, ".txt"))
  } else {
    file_name <- file.path(dir, paste0(class_name, "__", layer_A, "-", layer_B, "_", tmeasure_name, ".txt"))
  }

  data <- read.table(file_name, header = FALSE, sep = ",")
  data <- as.data.frame(t(data))
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]

  for (i in 1:ncol(data)) {
    data[, i] <- as.numeric(data[, i])
  }

  data
}

#' Read measures from a set of files
#'
#' @param dir The directory where the files are located.
#' @param class_name The name of the class.
#' @param measure_name The name of the measure.
#' @param i The index of the file.
#' @param layer_A The first layer.
#' @param layer_B The second layer (optional).
#' @return A data frame with the measures.
#' @export
read_measure_set <- function(dir, class_name, measure_name, i, layer_A, layer_B = 0) {
  if (layer_B == 0) {
    file_name <- paste0(layer_A, "_", measure_name, ".txt")
  } else {
    file_name <- paste0(layer_A, "-", layer_B, "_", measure_name, ".txt")
  }
  file_name <- file.path(dir, paste0(class_name, "_", i, "__", file_name))

  data <- read.table(file_name, header = FALSE, sep = ",")
  data <- as.data.frame(t(data))
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]

  if (class(data) == "character") {
    data <- as.numeric(data)
    data <- data.frame("AvgDegree" = data)
  } else {
    for (i in 1:ncol(data)) {
      data[, i] <- as.numeric(data[, i])
    }
    data$Class <- class_name
  }

  data
}
