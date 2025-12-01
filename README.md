# **Bike-Share NHPP Pipeline**

This repository contains an R-based pipeline for analyzing historical bike-share usage data and recommending an initial placement of bikes at the start of each day. The method uses **non-homogeneous Poisson processes (NHPPs)** to model time-varying demand and simulates system performance under different fleet sizes.

The project includes:

-   NHPP arrival-rate estimation
-   Network-wide demand simulation
-   Heuristic bike placement optimization
-   Visualizations and output tables
-   Example results for multiple fleet sizes

## **Input Data**

The pipeline expects historical bike-share data in the same format as previous labs. A small example file is included:

**`sample_bike.csv`**

It contains:

-   `start_station`
-   `end_station`
-   `start_time`
-   `end_time`
-   `customer_type` (ignored in the pipeline)

Timestamps must be in `YYYY-MM-DD HH:MM:SS` format.

## **How to Run the Pipeline**

### **Step 1: Load scripts**

```{r}
source("run_pipeline.R")
```

### **Step 2: Load data**

```{r}
data <- read.csv("sample_bike.csv")
data$start_time <- as.POSIXct(data$start_time)
data$end_time <- as.POSIXct(data$end_time)
```

### Step 3: Run the full pipeline

```{r}
results <- run_pipeline(
  data = data,
  total_bikes = 200,   # specify fleet size
  seed = 123           # optional for reproducibility
)
```

### Step 4: Run for multiple fleet sizes

```{r}
fleet_sizes <- c(100, 200, 300)

results_list <- lapply(fleet_sizes, function(b) {
  run_pipeline(data, total_bikes = b, seed = 123)
})
```

### Step 5: Visualize results

```{r}
plot_inventory(results$inventory, station = "A")
plot_flow_heatmap(results$mu_hat)
```

## **Script Descriptions**

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

You will need the following R packages:

```         
tidyverse
lubridate
ggplot2
dplyr
tidyr
```
