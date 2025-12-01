# **Bike-Share NHPP Pipeline**

This pipeline is designed so that a transportation or city manager can estimate 
bike demand, simulate daily usage, and compare different fleet sizes to decide 
the best number of bikes and how to place them at the start of the day. The 
method uses non-homogeneous Poisson processes (NHPPs) to model demand over time 
and simulate system performance.

The project includes:

-   NHPP arrival-rate estimation
-   Network-wide demand simulation
-   Heuristic bike placement optimization
-   Visualizations and output tables
-   Example results for multiple fleet sizes

## **Input Data**

The pipeline expects historical bike-share data in the same format as previous 
labs. A small example file is included:

**`sample_bike.csv`**

It contains:

-   `start_station`
-   `end_station`
-   `start_time`
-   `end_time`
-   `customer_type` (ignored in the pipeline)

Timestamps must be in `YYYY-MM-DD HH:MM:SS` format.

## **How to Run the Pipeline**

```{r}
source("run_pipeline.R")

data <- read.csv("sample_bike.csv")
data$start_time <- as.POSIXct(data$start_time)
data$end_time   <- as.POSIXct(data$end_time)

# Run the full pipeline
results <- run_pipeline(
  data        = data,
  total_bikes = 200,   # choose fleet size
  seed        = 123
)

This will automatically: 
- estimate NHPP hourly arrival rates
- simulate a full day of bike system usage 
- compute recommended initial inventory per station 
- generate summary outputs and plots

### Compare Multiple Fleet Sizes (manager scenario)

```{r}
fleet_sizes <- c(100, 200, 300)

results_list <- lapply(fleet_sizes, function(b) {
  run_pipeline(data, total_bikes = b, seed = 123)
})

names(results_list) <- paste0("bikes_", fleet_sizes)
```
Each element contains recommended starting inventory, simulation results, 
and summary metrics.

### Visualize results
# Inventory over time for a specific station
```{r}
plot_inventory(results$inventory, station = "10")
```
# Heatmap of flows
```{r}
plot_flow_heatmap(results$mu_hat)
```

## **Script Descriptions**

### **run_pipeline.R**

Main entry point that coordinates:
1. data preprocessing
2. NHPP arrival-rate estimation
3. demand simulation
4. bike placement recommendations
5. summary output

### **estimation.R**

Contains functions for preprocessing data and estimating hourly arrival rates.

Main function:

-   **`estimate_arrival_rates(data)`** Computes μ̂ for each hour and station pair using:

    -   average trips per hour
    -   average availability per hour
    -   μ̂ = avg_trips / avg_avail

These rates feed into the NHPP intensity functions used in simulation.

### **simulation.R**

Functions for generating realistic daily demand using NHPPs.

-   **`simulate_arrivals()`** — generate Poisson arrivals per station-hour
-   **`simulate_trips()`** — compute completed trips given capacity
-   **`simulate_inventory()`** — track bike inventory across 24 hours

These run automatically through run_pipeline().

### **placement.R**

Functions for scoring placements and searching for good solutions.

-   **`compute_initial_inventory()`**— heuristic allocation of bikes
-   **`rebalance_inventory()`**— flag stations needing more/fewer bikes

### **utils.R**

Utility functions for plotting:

-   **`plot_inventory()`**— inventory over time per station
-   **`plot_flow_heatmap()`**— heatmap of station-to-station flows

## **Tests**

Basic unit tests (using **testthat**) are located in:

```         
tests/testthat.R
```

These verify core functionality such as:

-   arrival-rate calculations
-   simulation output structure

## **Example Outputs**

The **results/** folder contains:

-   NHPP intensity plots
-   recommended placement tables
-   simulation-based performance summaries

for at least three fleet sizes.

## **Requirements**

Packages needed:

```{r}
tidyverse
lubridate
ggplot2
dplyr
tidyr
testthat
```
If missing:
```{r}
install.packages(c("tidyverse", "lubridate", "ggplot2", "dplyr", "tidyr", "testthat"))
```