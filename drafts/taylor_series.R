library(tidyverse)

dom <- seq(-pi/2, pi/2, length.out = 20)
dom <- seq(0, 2, 0.1)

make_poly <- function(f, x, n) {
  derivaties   <- iterate(d, n, f)
  factorials   <- factorial(seq(0, n))
  coefficients <- invoke_map_dbl(derivaties, x)

  substitute_and_sum <- function(a) {
    xs <- a ^ seq(0, n)
    sum(coefficients * xs / factorials)
  }
  function(as) map_dbl(as, substitute_and_sum)
}

F <- function(x) exp(sin(x))

ggplot() +
  stat_function(aes(dom), fun = F, color = "red") +
  stat_function(aes(dom), fun = make_poly(F, 0, 4), color = "orange")
