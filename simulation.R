# Simulate arrivals, completed trips, and station inventory

library(dplyr)

#' Simulate Poisson arrivals at each station–hour
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
#'   start_station, end_station, hour, mu_hat, arrivals
#' @export
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
#'   applying a per-station capacity cap (per hour).
#'
#' @param arrivals_df Output of \code{simulate_arrivals()}.
#' @param capacities Named numeric vector giving bike capacity at each
#'   start station (names = station IDs).
#'
#' @return A data frame of simulated completed trips with columns:
#'   start_station, end_station, hour, arrivals, capacity, completed, unmet.
#' @export
simulate_trips <- function(arrivals_df, capacities) {
  
  arrivals_df %>%
    group_by(start_station) %>%
    mutate(
      # look up capacity for each start_station; NA -> 0
      capacity  = ifelse(is.na(capacities[start_station]),
                         0,
                         capacities[start_station]),
      completed = pmin(arrivals, capacity),
      unmet     = pmax(arrivals - completed, 0)
    ) %>%
    ungroup()
}


#' Simulate station inventory over a 24-hour period
#'
#' @description Tracks inventory changes across 24 hours using simulated
#'   trip flows and an initial inventory vector.
#'
#' @param trips_df Output of \code{simulate_trips()}.
#'   Must contain: start_station, end_station, hour, completed.
#' @param initial_inventory Named numeric vector of starting bikes per station
#'   (names = station IDs).
#'
#' @return A data frame with columns: station, hour, inventory.
#' @export
simulate_inventory <- function(trips_df, initial_inventory) {
  
  # All stations observed in the simulated trips
  all_stations <- unique(c(trips_df$start_station, trips_df$end_station))
  
  # Create full grid of station x hour
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
  
  # Loop over stations and hours to update inventory
  for (s in all_stations) {
    
    # if station not in initial_inventory, start at 0
    inv <- ifelse(is.na(initial_inventory[s]), 0, initial_inventory[s])
    
    for (h in 0:23) {
      
      outgoing <- trips_df %>%
        filter(start_station == s, hour == h) %>%
        summarise(sum_out = safe_sum(completed), .groups = "drop") %>%
        pull(sum_out)
      
      incoming <- trips_df %>%
        filter(end_station == s, hour == h) %>%
        summarise(sum_in = safe_sum(completed), .groups = "drop") %>%
        pull(sum_in)
      
      # update inventory: previous + incoming - outgoing
      inv <- inv + incoming - outgoing
      
      # prevent negative inventory (optional but safer)
      inv <- max(inv, 0)
      
      inventory_df$inventory[
        inventory_df$station == s &
          inventory_df$hour == h
      ] <- inv
    }
  }
  
  inventory_df
}

