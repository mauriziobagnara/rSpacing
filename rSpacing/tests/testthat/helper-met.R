make_test_met_data <- function(n = 10L, start = as.Date("2000-01-01")) {
  dates <- seq(start, by = "day", length.out = n)
  data.frame(
    date = dates,
    radn = seq(12, 21, length.out = n),
    maxt = seq(20, 29, length.out = n),
    mint = seq(10, 19, length.out = n),
    rain = c(0, 1, rep(0, max(0, n - 2L)))
  )
}
