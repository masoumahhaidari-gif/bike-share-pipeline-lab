#' Estimate hourly arrival rates for a bike-share system
#'
#' This function computes hourly arrival rates \eqn{\hat{\mu}} for each
#' origin–destination station pair using historical trip data. The
#' arrival rate estimate is based on:
#'
#' \itemize{
#'   \item the average number of trips per hour between each station pair, and
#'   \item the average amount of time each station has at least one bike
#'     available (estimated using cumulative inventory over time).
#' }
#'
#' The final rate estimate is:
#' \deqn{\hat{\mu} = \frac{\text{avg_trips}}{\text{avg_avail}}}
#' whenever stations are available during that hour.
#'
#' @param data A data frame containing at least the following columns:
#'   \describe{
#'     \item{start_station}{Character or factor: station ID where the trip starts.}
#'     \item{end_station}{Character or factor: station ID where the trip ends.}
#'     \item{start_time}{POSIXct: trip start timestamp.}
#'     \item{end_time}{POSIXct: trip end timestamp.}
#'   }
#'
#' @return A tibble containing hourly arrival‐rate estimates with columns:
#'   \describe{
#'     \item{start_station}{Origin station.}
#'     \item{end_station}{Destination station.}
#'     \item{hour}{Hour of the day (0–23).}
#'     \item{avg_trips}{Average hourly number of trips from origin to destination.}
#'     \item{avg_avail}{Average time (hours) station had bikes available.}
#'     \item{mu_hat}{Estimated hourly arrival rate \eqn{\hat{\mu}}.}
#'   }
estimate_arrival_rates <- function(data) {
  
  # Compute average number of trips per hour between each station pair
  x_hat <- data %>%
    mutate(hour = lubridate::hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(
      avg_trips = n() / n_distinct(lubridate::as_date(start_time)),
      .groups = "drop"
    )
  
  # Convert to long format to track station inventory changes
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(
      cols = c("start_station", "start_time", "end_station", "end_time"),
      names_to = c("type", ".value"),
      names_pattern = "(start|end)_(.*)"
    ) %>%
    mutate(
      change = ifelse(type == "start", -1, 1),
      hour   = lubridate::hour(time)
    ) %>%
    select(station, time, hour, change)
  
  # Add hour markers for each station/date combination
  dates    <- unique(lubridate::as_date(trips_long$time))
  hours    <- c(seq(0, 23), seq(0, 23) + 0.9999999)
  stations <- unique(trips_long$station)
  
  hr_pts <- expand.grid(time = dates, hour = hours, station = stations) %>%
    mutate(
      time   = as.POSIXct(time) + hour * 60 * 60,
      hour   = lubridate::hour(time),
      change = 0
    )
  
  # Combine raw and padded data
  trips_long <- rbind(trips_long, hr_pts)
  
  # Estimate average availability (time with count > 0 bikes)
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>%
    mutate(
      count = cumsum(change),
      date  = lubridate::as_date(time)
    ) %>%
    group_by(station, hour, date) %>%
    summarise(
      time_avail = sum(as.numeric(diff(time),
                                  units = "hours") * (head(count, -1) > 0)),
      .groups = "drop_last"
    ) %>%
    summarise(
      avg_avail = round(mean(time_avail), 4),
      .groups   = "drop"
    )
  
  # Merge trip counts with availability to compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA_real_))
  
  return(mu_hat)
}
