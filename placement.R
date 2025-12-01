#' Compute Optimal Starting Inventory
#'
#' @description Computes initial inventory allocation using proportional demand,
#'   equal distribution, or custom weights.
#'
#' @param demand_df A data frame with station and expected demand.
#' @param total_bikes Total number of bikes available.
#' @param method One of "proportional", "equal".
#'
#' @return Named numeric vector of initial inventory by station.
#'
compute_initial_inventory <- function(demand_df, total_bikes, 
                                      method = c("proportional", "equal")) {
  method <- match.arg(method)
  stations <- demand_df$station
 
  if (method == "equal") {
    alloc <- rep(total_bikes / length(stations), length(stations))
  } else {
    w <- demand_df$demand / sum(demand_df$demand)
    alloc <- w * total_bikes
  }
  
  names(alloc) <- stations
  round(alloc)
}


#' Rebalance Inventory After Simulation
#'
#' @description Applies a simple threshold-based rebalancing rule.
#'
#' @param inventory_df Output of simulate_inventory().
#' @param low_thresh Lower bound for acceptable inventory.
#' @param high_thresh Upper bound for acceptable inventory.
#'
#' @return A data frame indicating where bikes need to be moved.
#'
rebalance_inventory <- function(inventory_df, low_thresh = 2, high_thresh = 20) {
  inventory_df %>%
    group_by(station) %>%
    summarise(
      min_inventory = min(inventory),
      max_inventory = max(inventory)
    ) %>%
    mutate(
      status = case_when(
        min_inventory < low_thresh ~ "needs_bikes",
        max_inventory > high_thresh ~ "remove_bikes",
        TRUE ~ "ok"
      )
    )
}
