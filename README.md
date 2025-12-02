# **Bike-Share NHPP Pipeline**

This project implements a full pipeline for analyzing and simulating bike-share
demand using non-homogeneous Poisson processes (NHPPs). The pipeline estimates 
hourly arrival rates, simulates daily usage, and recommends initial bike 
placement for different fleet sizes.

It is designed for a transportation or city manager deciding:

- how many bikes are needed in the system
- where to place them at the start of the day
- how different fleet sizes affect unmet demand

Outputs include tables, simulated inventories, and performance summaries for 
multiple fleet sizes.

## **Input Data**

The pipeline expects a CSV file with the following columns:

- start_station
- end_station
- start_time (timestamp: "YYYY-MM-DD HH:MM:SS")
- end_time
- customer_type (ignored)

An example file is provided:
**`sample_bike.csv`**

## **How to Run the Pipeline**

1. Load project functions

```{r}
source("utils.R")
source("estimation.R")
source("simulation.R")
source("placement.R")
```

2. Load the data

```{r}
df <- load_bike_data("sample_bike.csv")
```

3. Estimate hourly NHPP arrival rates

```{r}
mu_hat <- estimate_arrival_rates(df)
```

4. Define station capacities
(Here we assign 50 bikes per station as an example.)

```{r}
stations   <- sort(unique(mu_hat$start_station))
capacities <- setNames(rep(50, length(stations)), stations)
```

5. Generate placement recommendations for multiple fleet sizes

```{r}
generate_recommendations(
  mu_hat      = mu_hat,
  fleet_sizes = c(50, 80, 100),   # choose any 3+
  capacities  = capacities,
  seed        = 123,
  results_dir = "results"
)
```

This will automatically:
- simulate arrivals
- simulate completed trips
- compute a heuristic starting inventory per station
- score unmet demand for each fleet size
- save CSV outputs into results/

Optional: Run the full pipeline for a single fleet size

```{r}
source("run_pipeline.R")

result_100 <- run_pipeline(
  data        = df,
  total_bikes = 100,
  seed        = 123
)
```

This returns a list containing:
- NHPP arrival rates
- simulated arrivals
- simulated trips
- initial inventory
- full 24-hour inventory
- rebalance flags

## Visualization Examples

# Plot a station’s inventory over time:

```{r}
plot_inventory(result_100$inventory, station = "10")
```

# Plot estimated flows between stations:

```{r}
plot_flow_heatmap(mu_hat)
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