# CHIEHYANG CHIEN 131037

library(pracma)

# Define the function for Monte Carlo integration
integration3d <- function(f, region, n_samples = 10000) {
  # Extract the limits of the rectangular region
  x_limits <- region[[1]]
  y_limits <- region[[2]]
  
  # Generate random points within the specified region
  x_samples <- runif(n_samples, min = x_limits[1], max = x_limits[2])
  y_samples <- runif(n_samples, min = y_limits[1], max = y_limits[2])
  
  # Evaluate the function at the random points
  f_values <- f(x_samples, y_samples)
  
  # Calculate the area of the rectangular region
  area <- (x_limits[2] - x_limits[1]) * (y_limits[2] - y_limits[1])
  
  # Estimate the integral using Monte Carlo method
  integral_estimate <- area * mean(f_values)
  
  return(integral_estimate)
}


# Example 1 (low n)
integration3d(
  f = function(x, y) {cos(x) * y},
  region = list(c(0, pi / 2), c(0, 1)),
  n_samples = 10^2
)
# Example 1 (high n)
integration3d(
  f = function(x, y) {cos(x) * y},
  region = list(c(0, pi / 2), c(0, 1)),
  n_samples = 10^5
)

# Example 2 (low n)
integration3d(
  f = function(x, y) { (cos(x) + 2) * (sin(y) + 1)},
  region = list(x = c(0, pi), y = c(0, pi)),
  n = 10^2)

# Example 2 (high n)
integration3d(
  f = function(x, y) { (cos(x) + 2) * (sin(y) + 1)},
  region = list(x = c(0, pi), y = c(0, pi)),
  n = 10^5)
