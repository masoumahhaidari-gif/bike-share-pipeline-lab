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

``` r
source("run_pipeline.R")
```

### **Step 2: Load data**

```{r}
data <- read.csv("sample_bike.csv")
```

### **Step 3: Simulation (NHPP-based demand generation)**

```{r}
intensity_list <- build_intensity_functions(mu_hat)   # if implemented internally
sim_results <- simulate_network(intensity_list, n_sim = 100)
```

### **Step 4: Search for a good placement**

```{r}
placement <- search_best_placement(intensity_list, n_bikes = 200)
```

### **Step 5: Produce final outputs**

```{r}
generate_recommendations(intensity_list, fleet_sizes = c(100, 150, 200))
```

Outputs are saved automatically to the **results/** folder.

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

-   **`simulate_station_day()`** — simulates pickup/return times for a station
-   **`simulate_network()`** — simulates across all stations for many runs

### **placement.R**

Functions for scoring placements and searching for good solutions.

-   **`score_placement()`** — evaluates shortages/overflow
-   **`search_best_placement()`** — heuristic search for allocations
-   **`generate_recommendations()`** — exports tables + plots for fleet sizes

### **utils.R**

Utility functions:

-   Plotting intensity functions
-   Formatting recommendation tables
-   Additional helper functions

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
