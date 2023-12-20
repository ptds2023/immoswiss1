library(testthat)

# Test that an error is thrown for invalid number of rooms
test_that("Invalid number of rooms throws an error", {
  expect_error(immoswiss::find_nearest_neighbors(3.6, 100, 1000, data),
               "Invalid number of rooms. Please enter a number of rooms in increments of 0.5.")
})

# Test that an error is thrown for invalid location
test_that("Invalid location throws an error", {
  expect_error(immoswiss::find_nearest_neighbors(2, 100, 999, data),
               "Invalid location. Please enter a valid location from the list: 1000, 1003, 1004, 1005,1006,1007,1010,1012,1018")
})

# Additional test cases can be added based on your specific requirements

# Test that correct input works without errors
test_that("Correct input does not throw an error", {
  expect_no_error(immoswiss::find_nearest_neighbors(2, 100, 1000, lausanne))
})
