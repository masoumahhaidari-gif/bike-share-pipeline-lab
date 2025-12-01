
# Executes the full estimation → simulation → placement workflow
source("estimation.R")
source("simulation.R")
source("placement.R")
source("utils.R")

run_pipeline <- function(data, total_bikes, seed = 123) {
  
  set.seed(seed)
  
  # 1. Estimate arrival rates
  mu_hat <- estimate_arrival_rates(data)
  
  # 2. Convert arrival rates → simulation
  arrivals <- simulate_arrivals(mu_hat, seed = seed)
  capacities <- rep(50, length(unique(data$start_station)))
  names(capacities) <- unique(data$start_station)
  
  trips <- simulate_trips(arrivals, capacities)
  
  # 3. Determine initial placement
  demand_df <- mu_hat %>%
    group_by(start_station) %>%
    summarise(demand = sum(mu_hat, na.rm = TRUE)) %>%
    rename(station = start_station)
  
  init_inv <- compute_initial_inventory(demand_df, total_bikes)
  
  # 4. Simulate inventory
  inv <- simulate_inventory(trips, init_inv)
  
  # 5. Rebalance suggestions
  reb <- rebalance_inventory(inv)
  
  list(
    mu_hat = mu_hat,
    arrivals = arrivals,
    trips = trips,
    initial_inventory = init_inv,
    inventory = inv,
    rebalance = reb
  )
}