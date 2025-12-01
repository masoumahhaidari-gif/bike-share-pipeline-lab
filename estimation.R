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
#'
#' @examples
#' \dontrun{
#' data <- read.csv("sample_bike.csv")
#' data$start_time <- as.POSIXct(data$start_time)
#' data$end_time <- as.POSIXct(data$end_time)
#' estimate_arrival_rates(data)
#' }
#'
#' @export
estimate_arrival_rates <- function(data) {
  
  # compute the average number of trips per hour between each pair
  x_hat <- data %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop") 
  
  # pivot longer to get change in count 
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    select(station, time, hour, change)
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1),seq(0,23,1)+0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    mutate(time = as.POSIXct(time) + hour*60*60,
           hour = hour(time))
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(count = cumsum(change),
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    summarize(time_avail = 
                sum(difftime(time, lag(time), units="hours")*(count > 0), 
                    na.rm = TRUE)) %>%
    summarize(avg_avail = mean(time_avail)) %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # join the data and compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat)
}