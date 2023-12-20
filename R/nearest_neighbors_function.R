#' Find Nearest Neighbors based on rooms and square meters
#' @param rooms Number of rooms specified by the user
#' @param meter_square Number of square meters specified by the user
#' @param location Location (postal code) specified by the user
#' @param data The dataset containing information on rooms, meter_square, price, and location
#' @param k Number of nearest neighbors to retrieve (default is 5)
#' @return A data frame with the k nearest neighbors matching the query
#' @export
#' @importFrom stats sd
#' @examples find_nearest_neighbors(5, 210, 1004, lausanne, k = 10)
find_nearest_neighbors <- function(rooms, meter_square, location, data, k = 5) {
  valid_locations <- c(1000, 1003, 1004, 1005,1006,1007,1010,1012,1018)
  if (!(location %in% valid_locations)) {
    stop("Invalid location. Please enter a valid location from the list: 1000, 1003, 1004, 1005,1006,1007,1010,1012,1018")
  }
  if (rooms %% 0.5 != 0) {
    stop("Invalid number of rooms. Please enter a number of rooms in increments of 0.5.")
  }
  # Scale the numerical features (rooms and meter_square)
  scaled_data <- scale(data[, c("rooms", "meter_square")])

  # Scale the query numerical parameters using mean and standard deviation of dataset columns
  scaled_query <- c((rooms - mean(data$rooms)) / sd(data$rooms),
                    (meter_square - mean(data$meter_square)) / sd(data$meter_square))

  # One-hot encode the location column for the query
  unique_locations <- unique(data$location)
  encoded_location <- ifelse(unique_locations == location, 1, 0)

  # Prepare encoded location for comparison with dataset
  scaled_query <- c(scaled_query, encoded_location)

  # One-hot encode the location column for the dataset
  encoded_locations <- t(sapply(data$location, function(x) ifelse(unique_locations == x, 1, 0)))

  # Calculate distances based on scaled features and encoded location
  distances <- apply(cbind(scaled_data, encoded_locations), 1, function(x) sqrt(sum((x - scaled_query)^2)))

  # Combine distances with the original dataset
  data_with_distances <- cbind(data, distance = round(distances,2))

  # Sort the dataset by distance in ascending order
  sorted_data <- data_with_distances[order(data_with_distances$distance), ]
  colnames(sorted_data) <- c("Rooms", "Surface", "Price", "Location", "Distance")
  # Return the top k nearest neighbors
  return(sorted_data[1:k, ])
}
