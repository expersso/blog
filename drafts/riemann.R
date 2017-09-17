library(tidyverse)
library(gridExtra)

get_interval <- function(lower, upper, delta = 1) {
  tibble(xmin = seq(lower, upper - delta, by = delta),
         xmax = seq(lower + delta, upper, by = delta))
}

add_integral_sums <- function(df, f, ...) {
  df$low <- map2_dbl(df$xmin, df$xmax, function(x, y)
    optimise(f, c(x, y), ...)$objective)
  df$high <- map2_dbl(df$xmin, df$xmax, function(x, y)
    optimise(f, c(x, y), maximum = TRUE, ...)$objective)
  df
}

as_riemann <- function(x) {
  structure(x, class = c("riemann", class(x)))
}

riemann <- function(f, lower, upper, delta = 1, ...) {
  df <- get_interval(lower, upper, delta) %>%
    add_integral_sums(f, ...) %>%
    as_riemann()

  attr(df, "lower_sum") <- sum(df$low) * delta
  attr(df, "upper_sum") <- sum(df$high) * delta
  attr(df, "delta") <- delta
  attr(df, "n_intervals") <- nrow(df)
  attr(df, "f") <- f
  df
}

plot.riemann <- function(df) {
  ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0)) +
    geom_rect(aes(ymax = high), fill = "blue",
              alpha = 0.5, color = "white", size = 0.1) +
    geom_rect(aes(ymax = low), fill = "red",
              alpha = 0.5, color = "white", size = 0.1) +
    stat_function(fun = attr(df, "f")) +
    theme_minimal() +
    labs(y = NULL,
         subtitle = sprintf("Lower sum: %0.1f, Upper sum: %0.1f",
                            attr(df, "lower_sum"), attr(df, "upper_sum")),
         caption = sprintf("\nDelta: %s\nintervals: %s",
                           attr(df, "delta"), attr(df, "n_intervals")))
}

plot_multiple_deltas <- function(f, lower, upper, deltas, ...) {
  plots <- map(deltas, ~plot(riemann(f, lower, upper, .)))
  lift(grid.arrange)(plots, ...)
}

f <- function(x) x^2

df <- riemann(f, -10, 10, 0.1)
test <- integrate(f, -10, 10)

plot_multiple_deltas(cos, -pi, pi, c(1, 0.5, 0.1, 0.01))
plot(riemann(cos, -pi, pi, pi/20))
