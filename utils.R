library(dplyr)
library(ggplot2)


#' Plot inventory over time for a single station
#'
#' @param inventory_df Output of simulate_inventory().
#' @param station A station ID (character).
#'
#' @return A ggplot object.
plot_inventory <- function(inventory_df, station) {
  
  inventory_df %>%
    filter(station == !!station) %>%
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
#'   start_station, end_station, mu_hat (arrival rate)
#'
#' @return A ggplot heatmap.
plot_flow_heatmap <- function(mu_hat) {
  
  mu_hat %>%
    ggplot(aes(x = start_station, y = end_station, fill = mu_hat)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    labs(
      title = "Estimated Flow Heatmap (μ̂)",
      x = "Start Station",
      y = "End Station",
      fill = "μ̂"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
