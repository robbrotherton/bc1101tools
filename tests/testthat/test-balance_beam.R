test_that("balance beam works", {

  test_data <- c(1, 2, 6, 6, 10)

  output <- balance_beam(test_data)

  expected_primary_data_layer <- tibble::tibble(x = test_data, group = factor(seq_along(test_data)))

  expect_equal(length(output$layers), 5)
  expect_equal(output$layers[[1]]$data, expected_primary_data_layer)

  # check that a new layer is added if deviation labels are included
  output_with_labels <- balance_beam(test_data, label_boxes = TRUE)
  expect_equal(length(output_with_labels$layers), 6)

  # check with different data
  test_data <- c(4, 6)
  output <- balance_beam(test_data)

  expected_primary_data_layer <- tibble::tibble(x = test_data, group = factor(seq_along(test_data)))

  expect_equal(length(output$layers), 5)
  expect_equal(output$layers[[1]]$data, expected_primary_data_layer)

})
