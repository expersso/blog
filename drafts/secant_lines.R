library(tidyverse)

line_params <- function(x0, x1, y0, y1) {
  m <- (y1 - y0) / (x1 - x0)
  c <- y1 - m * x1
  list("slope" = m, "intercept" = c)
}

secant <- function(f, x0, x1, ...) {
  line_params(x0, x1, f(x0, ...), f(x1, ...))
}

geom_secant <- function(fun, x0, x1, ...) {
  params <- secant(fun, x0, x1)
  list(geom_abline(slope = params$slope, intercept = params$intercept),
       geom_point(aes_(c(x0, x1), fun(c(x0, x1), ...))))
}

x <- -3:3
cube <- function(x) x^2

ggplot() +
  stat_function(aes(x), fun = cube) +
  geom_secant(cube, seq(-5, 4, 0.1), seq(-4, 5, 0.1)) +
  coord_cartesian(ylim = c(-25, 25)) +
  theme_void()
