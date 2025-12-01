#' Simulates Poisson Arrivals at Each Station
#'
#' @description Generates hourly arrivals for all station pairs based on
#'   arrival rate estimates.
#'
#' @param mu_hat A data frame containing columns:
#'   \itemize{
#'     \item start_station
#'     \item end_station
#'     \item hour
#'     \item mu_hat (arrival rate)
#'   }
#' @param seed Optional integer for reproducibility.
#'
#' @return A data frame of simulated arrivals with columns:
#'   start_station, end_station, hour, arrivals
#'
simulate_arrivals <- function(mu_hat, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  mu_hat %>%
    mutate(arrivals = rpois(n(), lambda = mu_hat))
}


#' Simulate Trip Completions
#'
#' @description Converts simulated arrivals into completed trips,
#'   optionally applying capacity limits.
#'
#' @param arrivals_df Output of `simulate_arrivals()`.
#' @param capacities Named numeric vector giving bike capacity at each station.
#'
#' @return A data frame of simulated completed trips.
#'
simulate_trips <- function(arrivals_df, capacities) {
  arrivals_df %>%
    group_by(start_station) %>%
    mutate(capacity = capacities[start_station],
           completed = pmin(arrivals, capacity)) %>%
    ungroup()
}


#' Simulate Station Inventory Over Time
#'
#' @description Tracks inventory changes across a 24h period using simulated trip flows.
#'
#' @param trips_df Output of `simulate_trips()`.
#' @param initial_inventory Named numeric vector of starting bikes per station.
#'
#' @return A data frame with station, hour, and simulated inventory.
#'
simulate_inventory <- function(trips_df, initial_inventory) {
  
  all_stations <- unique(c(trips_df$start_station, trips_df$end_station))
  
  inventory_df <- expand.grid(
    station = all_stations,
    hour = 0:23
  ) %>%
    arrange(station, hour) %>%
    mutate(inventory = NA_real_)
  
  for (s in all_stations) {
    inv <- initial_inventory[s]
    
    for (h in 0:23) {
      outgoing <- trips_df %>%
        filter(start_station == s, hour == h) %>%
        summarise(sum_out = sum(completed, na.rm = TRUE)) %>% pull()
      
      incoming <- trips_df %>%
        filter(end_station == s, hour == h) %>%
        summarise(sum_in = sum(completed, na.rm = TRUE)) %>% pull()
      
      inv <- inv + incoming - outgoing
      inventory_df$inventory[inventory_df$station == s & inventory_df$hour == h] <- inv
    }
  }
  
  inventory_df
}
