# Helper functions for loading, preprocessing, and plotting

library(dplyr)
library(ggplot2)
library(lubridate)

# LOAD & PREPROCESS FUNCTIONS

#' Load and clean bike-share data
#'
#' @param path Character, path to the CSV file.
#'
#' @return A data.frame with parsed timestamps and required columns.
#' @export
load_bike_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  required_cols <- c("start_station", "end_station", "start_time", "end_time")
  missing <- setdiff(required_cols, names(df))
  
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  df$start_time <- ymd_hms(df$start_time, quiet = TRUE)
  df$end_time   <- ymd_hms(df$end_time, quiet = TRUE)
  
  return(df)
}

#' Preprocess bike data by adding date/hour columns
#'
#' @param df The data frame returned by load_bike_data()
#'
#' @return Data frame with date and hour columns added.
#' @export
preprocess_bike_data <- function(df) {
  df %>%
    mutate(
      date = as.Date(start_time),
      hour = hour(start_time)
    )
}

# PLOTTING FUNCTIONS

#' Plot inventory over time for a single station
#'
#' @param inventory_df Output of simulate_inventory().
#' @param station A station ID (character).
#'
#' @return A ggplot object.
#' @export
plot_inventory <- function(inventory_df, station) {
  
  inventory_df %>%
    filter(station == station) %>%
    ggplot(aes(x = hour, y = inventory)) +
    geom_line(color = "steelblue") +
    geom_point(color = "steelblue") +
    labs(
      title = paste("Inventory Over Time:", station),
      x = "Hour of Day",
      y = "Number of Bikes"
    ) +
    theme_minimal()
}

#' Plot heatmap of estimated flow (arrival rates)
#'
#' @param mu_hat A data frame containing:
#'   start_station, end_station, mu_hat
#'
#' @return A ggplot heatmap.
#' @export
plot_flow_heatmap <- function(mu_hat) {
  
  mu_hat %>%
    filter(!is.na(mu_hat)) %>% 
    ggplot(aes(x = start_station, y = end_station, fill = mu_hat)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", trans = "log") +
    labs(
      title = "Estimated Flow Heatmap (μ̂)",
      x = "Start Station",
      y = "End Station",
      fill = "log(μ̂)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

