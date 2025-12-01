test_that("estimate_arrival_rates returns expected columns", {
  sample <- data.frame(
    start_station = c("A"),
    end_station = c("B"),
    start_time = as.POSIXct("2022-01-01 01:00:00"),
    end_time   = as.POSIXct("2022-01-01 01:10:00")
  )
  
  res <- estimate_arrival_rates(sample)
  
  expect_true(all(c("start_station", "end_station", "hour",
                    "avg_trips", "avg_avail", "mu_hat") %in% names(res)))
})
