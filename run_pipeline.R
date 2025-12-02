library(dplyr)
library(lubridate)

# Load project functions
source("estimation.R")
source("simulation.R")
source("placement.R")
source("utils.R")

#' Run the full bike-share pipeline for one fleet size
#'
#' @param data Historical trip data with columns:
#'   start_station, end_station, start_time, end_time, ...
#' @param total_bikes Total number of bikes available in the fleet.
#' @param seed Base random seed for reproducibility.
#' @param capacities Optional named numeric vector of per-station
#'   capacities. If NULL, all stations are given capacity 50.
#'
#' @return A list containing:
#'   \itemize{
#'     \item mu_hat: hourly arrival-rate estimates
#'     \item arrivals: simulated arrivals
#'     \item trips: simulated completed trips
#'     \item initial_inventory: starting bikes per station
#'     \item inventory: inventory over time
#'     \item rebalance: summary of where bikes should be added/removed
#'   }
#' @export
run_pipeline <- function(data,
                         total_bikes,
                         seed = 123,
                         capacities = NULL) {
  
  set.seed(seed)
  
  # Ensure time columns are POSIXct (if they aren't already)
  if (!inherits(data$start_time, "POSIXct")) {
    data <- data %>%
      mutate(
        start_time = ymd_hms(start_time),
        end_time   = ymd_hms(end_time)
      )
  }
  
  # 1. Estimate arrival rates
  mu_hat <- estimate_arrival_rates(data)
  
  # 2. Simulate arrivals and trips
  arrivals <- simulate_arrivals(mu_hat, seed = seed)
  
  # If no capacities provided, assume 50 bikes per station
  if (is.null(capacities)) {
    stations   <- sort(unique(data$start_station))
    capacities <- setNames(rep(50, length(stations)), stations)
  }
  
  trips <- simulate_trips(arrivals, capacities)
  
  # 3. Determine initial placement based on demand
  demand_df <- mu_hat %>%
    group_by(start_station) %>%
    summarise(demand = sum(mu_hat, na.rm = TRUE), .groups = "drop") %>%
    rename(station = start_station)
  
  init_inv <- compute_initial_inventory(demand_df, total_bikes)
  
  # 4. Simulate inventory over the day
  inv <- simulate_inventory(trips, init_inv)
  
  # 5. Simple rebalance suggestions
  reb <- rebalance_inventory(inv)
  
  list(
    mu_hat            = mu_hat,
    arrivals          = arrivals,
    trips             = trips,
    initial_inventory = init_inv,
    inventory         = inv,
    rebalance         = reb
  )
}

