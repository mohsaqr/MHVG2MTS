library(testthat)
library(MHVG2MTS)

test_that("read_measures works", {
  # Create a dummy file
  dir.create("tmp_data")
  write.table(data.frame(a = 1:2, b = 3:4), "tmp_data/my_class__layer1_my_measure.txt", sep = ",", row.names = FALSE, col.names = TRUE)

  # Test reading the file
  measures <- read_measures(dir = "tmp_data", class_name = "my_class", tmeasure_name = "my_measure", layer_A = "layer1")
  expect_equal(nrow(measures), 2)
  expect_equal(ncol(measures), 2)

  # Clean up
  unlink("tmp_data", recursive = TRUE)
})

test_that("read_measure_set works", {
  # Create a dummy file
  dir.create("tmp_data")
  write.table(data.frame(a = 1:2, b = 3:4), "tmp_data/my_class_1__layer1_my_measure.txt", sep = ",", row.names = FALSE, col.names = TRUE)

  # Test reading the file
  measure_set <- read_measure_set(dir = "tmp_data", class_name = "my_class", measure_name = "my_measure", i = 1, layer_A = "layer1")
  expect_equal(nrow(measure_set), 2)
  expect_equal(ncol(measure_set), 3) # Adds a "Class" column

  # Clean up
  unlink("tmp_data", recursive = TRUE)
})
