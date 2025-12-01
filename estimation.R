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
  
  # Make sure station IDs are characters (not factors)
  data <- data %>%
    dplyr::mutate(
      start_station = as.character(start_station),
      end_station   = as.character(end_station)
    )
  
  # 1. Average number of trips per hour between each station pair
  x_hat <- data %>%
    dplyr::filter(start_station != "R", end_station != "R") %>%
    dplyr::mutate(hour = lubridate::hour(start_time)) %>%
    dplyr::group_by(start_station, end_station, hour) %>%
    dplyr::summarise(
      avg_trips = n() / dplyr::n_distinct(lubridate::as_date(start_time)),
      .groups   = "drop"
    )
  
  # 2. Long format to track station inventory changes over time
  trips_long <- data %>%
    dplyr::filter(start_station != "R", end_station != "R") %>%
    tidyr::pivot_longer(
      cols = c(start_station, start_time, end_station, end_time),
      names_to     = c("type", ".value"),
      names_pattern = "(start|end)_(.*)"
    ) %>%
    dplyr::mutate(
      change = ifelse(type == "start", -1L, 1L),
      time   = as.POSIXct(time),
      hour   = lubridate::hour(time)
    ) %>%
    dplyr::select(station, time, hour, change)
  
  # 3. Add hour boundary points for each station and date
  dates    <- unique(lubridate::as_date(trips_long$time))
  stations <- unique(trips_long$station)
  hours    <- c(seq(0, 23), seq(0, 23) + 0.9999999)
  
  hr_pts <- expand.grid(
    time    = dates,
    hour    = hours,
    station = stations,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      time   = as.POSIXct(time) + hour * 60 * 60,
      hour   = lubridate::hour(time),
      change = 0L
    ) %>%
    dplyr::select(station, time, hour, change)
  
  # Combine raw and padded data
  trips_long <- dplyr::bind_rows(trips_long, hr_pts)
  
  # 4. Estimate average availability (time with count > 0 bikes)
  alpha_hat <- trips_long %>%
    dplyr::filter(station != "R") %>%
    dplyr::arrange(station, time) %>%
    dplyr::group_by(station) %>%
    dplyr::mutate(
      count = cumsum(change),
      date  = lubridate::as_date(time)
    ) %>%
    dplyr::group_by(station, hour, date) %>%
    dplyr::summarise(
      time_avail = sum(
        as.numeric(diff(time), units = "hours") *
          (head(count, -1) > 0)
      ),
      .groups = "drop_last"
    ) %>%
    dplyr::summarise(
      avg_avail = round(mean(time_avail), 4),
      .groups   = "drop"
    )
  
  # 5. Merge trip counts with availability to compute arrival rates
  mu_hat <- x_hat %>%
    dplyr::left_join(alpha_hat,
                     by = c("start_station" = "station", "hour" = "hour")) %>%
    dplyr::mutate(
      mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA_real_)
    )
  
  return(mu_hat)
}
