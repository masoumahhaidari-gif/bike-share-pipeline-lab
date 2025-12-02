# Functions for computing starting inventory and scoring placements

library(dplyr)

#' Compute initial starting inventory
#'
#' @param demand_df Data frame with columns:
#'   \itemize{
#'     \item station: station ID
#'     \item demand: expected demand for that station (e.g., sum of mu_hat)
#'   }
#' @param total_bikes Total number of bikes available.
#' @param method One of "proportional" (default) or "equal".
#'
#' @return Named numeric vector of initial inventory by station.
#' @export
compute_initial_inventory <- function(demand_df, total_bikes,
                                      method = c("proportional", "equal")) {
  method   <- match.arg(method)
  stations <- demand_df$station
  
  if (method == "equal") {
    alloc_raw <- rep(total_bikes / length(stations), length(stations))
  } else {
    # proportional to expected demand
    w <- demand_df$demand / sum(demand_df$demand)
    alloc_raw <- w * total_bikes
  }
  
  alloc <- round(alloc_raw)
  
  # Fix rounding so total exactly matches total_bikes
  diff <- total_bikes - sum(alloc)
  if (diff != 0) {
    # tweak the stations with highest demand
    idx <- order(demand_df$demand, decreasing = TRUE)[seq_len(abs(diff))]
    alloc[idx] <- alloc[idx] + sign(diff)
  }
  
  names(alloc) <- stations
  alloc
}


#' Rebalance inventory after simulation
#'
#' @param inventory_df Output of \code{simulate_inventory()}.
#' @param low_thresh Lower bound for acceptable inventory.
#' @param high_thresh Upper bound for acceptable inventory.
#'
#' @return A data frame indicating where bikes need to be moved.
#' @export
rebalance_inventory <- function(inventory_df,
                                low_thresh = 2,
                                high_thresh = 20) {
  
  inventory_df %>%
    group_by(station) %>%
    summarise(
      min_inventory = min(inventory, na.rm = TRUE),
      max_inventory = max(inventory, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      status = case_when(
        min_inventory < low_thresh  ~ "needs_bikes",
        max_inventory > high_thresh ~ "remove_bikes",
        TRUE                        ~ "ok"
      )
    )
}


#' Score a placement using simulated trips and inventory
#'
#' @description Uses the simulated trips and resulting inventory to compute
#'   a simple placement score. Here, the score is the total unmet demand
#'   (number of trips that could not be served). Lower is better.
#'
#' @param trips_df Output of \code{simulate_trips()} (must contain 'unmet').
#' @param initial_inventory Named numeric vector of starting bikes per station.
#'
#' @return A list with:
#'   \itemize{
#'     \item score: total unmet demand over the day (lower is better)
#'     \item inventory: data frame of station-hour inventory
#'   }
#' @export
score_placement <- function(trips_df, initial_inventory) {
  inv_df <- simulate_inventory(trips_df, initial_inventory)
  
  # total unmet trips across all stations and hours
  total_unmet <- sum(trips_df$unmet, na.rm = TRUE)
  
  list(
    score     = total_unmet,
    inventory = inv_df
  )
}


#' Generate recommendations and save results for several fleet sizes
#'
#' @param mu_hat Output of \code{estimate_arrival_rates()}.
#' @param fleet_sizes Numeric vector of total bikes (e.g., c(50, 100, 150)).
#' @param capacities Named numeric vector of per-station capacities
#'   (names = station IDs used as start_station in mu_hat).
#' @param seed Base seed for reproducibility.
#' @param results_dir Folder where CSV outputs will be saved.
#'
#' @return Invisibly, a data frame summarising scores by fleet size.
#' @export
generate_recommendations <- function(mu_hat,
                                     fleet_sizes,
                                     capacities,
                                     seed = 123,
                                     results_dir = "results") {
  
  # ensure results directory exists
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  scores <- list()
  
  for (B in fleet_sizes) {
    set.seed(seed + B)
    
    # 1. Simulate arrivals and trips
    arrivals <- simulate_arrivals(mu_hat, seed = seed + B)
    trips    <- simulate_trips(arrivals, capacities)
    
    # 2. Build demand summary for placement (expected demand per station)
    demand_df <- mu_hat %>%
      group_by(start_station) %>%
      summarise(demand = sum(mu_hat, na.rm = TRUE), .groups = "drop") %>%
      rename(station = start_station)
    
    # 3. Compute initial inventory for this fleet size
    init_inv <- compute_initial_inventory(demand_df, total_bikes = B)
    
    # 4. Score the placement (using unmet demand)
    placement_result <- score_placement(trips, init_inv)
    inv_df <- placement_result$inventory
    sc     <- placement_result$score
    
    # 5. Save placement table and inventory time series
    placement_tbl <- tibble(
      station = names(init_inv),
      bikes   = as.numeric(init_inv)
    )
    
    write.csv(
      placement_tbl,
      file = file.path(results_dir, paste0("placement_", B, ".csv")),
      row.names = FALSE
    )
    
    write.csv(
      inv_df,
      file = file.path(results_dir, paste0("inventory_", B, ".csv")),
      row.names = FALSE
    )
    
    # store score
    scores[[as.character(B)]] <- sc
    message("Fleet size ", B, ": total unmet demand = ", round(sc, 2))
  }
  
  score_df <- tibble(
    fleet_size = as.numeric(names(scores)),
    score      = unlist(scores)
  )
  
  write.csv(
    score_df,
    file = file.path(results_dir, "scores_by_fleet_size.csv"),
    row.names = FALSE
  )
  
  invisible(score_df)
}

