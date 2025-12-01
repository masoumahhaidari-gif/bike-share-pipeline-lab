library(dplyr)

#' Simulates Poisson arrivals at each station–hour
#'
#' @description Generates hourly arrivals for all station pairs based on
#'   arrival rate estimates.
#'
#' @param rates_df A data frame containing columns:
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
simulate_arrivals <- function(rates_df, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  rates_df %>%
    mutate(
      arrivals = rpois(dplyr::n(), lambda = mu_hat)
    )
}


#' Simulate trip completions (with simple capacity limits)
#'
#' @description Converts simulated arrivals into completed trips,
#'   applying a per-station capacity cap.
#'
#' @param arrivals_df Output of `simulate_arrivals()`.
#' @param capacities Named numeric vector giving bike capacity at each
#'   start station (names = station IDs).
#'
#' @return A data frame of simulated completed trips.
simulate_trips <- function(arrivals_df, capacities) {
  arrivals_df %>%
    group_by(start_station) %>%
    mutate(
      capacity  = capacities[start_station],
      completed = pmin(arrivals, capacity)
    ) %>%
    ungroup()
}


#' Simulate station inventory over a 24h period
#'
#' @description Tracks inventory changes across 24 hours using simulated
#'   trip flows and an initial inventory vector.
#'
#' @param trips_df Output of `simulate_trips()`. Must contain:
#'   start_station, end_station, hour, completed.
#' @param initial_inventory Named numeric vector of starting bikes per station.
#'
#' @return A data frame with columns: station, hour, inventory.
simulate_inventory <- function(trips_df, initial_inventory) {
  
  all_stations <- unique(c(trips_df$start_station, trips_df$end_station))
  
  inventory_df <- expand.grid(
    station = all_stations,
    hour    = 0:23,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    arrange(station, hour) %>%
    mutate(inventory = NA_real_)
  
  # helper: safe sum, returns 0 if no rows
  safe_sum <- function(x) {
    if (length(x) == 0) return(0)
    sum(x, na.rm = TRUE)
  }
  
  for (s in all_stations) {
    inv <- initial_inventory[s]
    
    for (h in 0:23) {
      outgoing <- trips_df %>%
        filter(start_station == s, hour == h) %>%
        summarise(sum_out = safe_sum(completed), .groups = "drop") %>%
        pull(sum_out)
      
      incoming <- trips_df %>%
        filter(end_station == s, hour == h) %>%
        summarise(sum_in = safe_sum(completed), .groups = "drop") %>%
        pull(sum_in)
      
      inv <- inv + incoming - outgoing
      
      inventory_df$inventory[
        inventory_df$station == s &
          inventory_df$hour == h
      ] <- inv
    }
  }
  
  inventory_df
}

