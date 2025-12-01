#' Plot Inventory Over Time for a Single Station
#'
#' @param inventory_df Output of simulate_inventory().
#' @param station A station name.
#'
#' @return A ggplot object.
#'
plot_inventory <- function(inventory_df, station) {
  inventory_df %>%
    filter(station == !!station) %>%
    ggplot(aes(x = hour, y = inventory)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("Inventory Over Time:", station),
      x = "Hour",
      y = "Number of Bikes"
    ) +
    theme_minimal()
}


#' Plot Heatmap of Average Flow
#'
#' @param mu_hat Arrival rate estimates.
#'
#' @return ggplot heatmap.
#'
plot_flow_heatmap <- function(mu_hat) {
  mu_hat %>%
    ggplot(aes(x = start_station, y = end_station, fill = mu_hat)) +
    geom_tile() +
    labs(
      title = "Estimated Flow Heatmap",
      x = "Start Station",
      y = "End Station"
    ) +
    theme_minimal()
}
