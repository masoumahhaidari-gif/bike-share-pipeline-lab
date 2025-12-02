library(testthat)
source("../utils.R")
source("../estimation.R")
source("../simulation.R")
source("../placement.R")

test_that("compute_initial_inventory sums correctly", {
  demand_df <- data.frame(
    station = c("A", "B", "C"),
    demand  = c(10, 20, 30)
  )
  
  inv <- compute_initial_inventory(demand_df, total_bikes = 60)
  
  expect_equal(sum(inv), 60)
})

test_that("simulate_arrivals returns correct rows", {
  rates_df <- data.frame(
    start_station = c("A", "A", "B"),
    end_station   = c("B", "C", "A"),
    hour          = c(8, 9, 10),
    mu_hat        = c(1, 2, 3)
  )
  
  sim <- simulate_arrivals(rates_df, seed = 1)
  expect_equal(nrow(sim), 3)
  expect_true("arrivals" %in% names(sim))
})

