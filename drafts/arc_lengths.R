library(tidyverse)
library(stringr)

dd <- function(f, name = "x") {
  F <- deriv(as.expression(body(f)), name, function.arg = TRUE)
  function(x) {
    out <- attr(F(x), "gradient")
    attributes(out) <- NULL
    out
  }
}

arc_length <- function(f, lower, upper) {
  F <- function(x) sqrt(1 + dd(f)(x)^2)
  integrate(F, lower, upper)
}

g <- function(x) x^2/8 - log(x)
arc_length(g, 1, 2)

x <- seq(1, 3)

segment <- function(f, domain) {
  tibble(
    id = seq_along(domain),
    x = domain,
    xend = lead(x),
    y = f(x),
    yend = f(xend)
  ) %>% drop_na()
}

make_domain <- function(length.out) {
  seq(-2*pi, 2*pi, length.out = length.out)
}

dfs <- map(2:13, ~segment(cos, make_domain(.))) %>%
  bind_rows(.id = "n_segments") %>%
  mutate(n_segments = str_pad(n_segments, 2, "left", "0"))

ggplot(dfs) +
  stat_function(aes(x), data = tibble(x = make_domain(10)),
                fun = cos, color = "grey70") +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend, color = factor(id)),
               show.legend = FALSE) +
  facet_wrap(~n_segments)

autoplot.integrate <- function(i, domain = seq(-5, 5, length.out = 100),
                               ..., fun.args = NULL) {
  f <- eval(i$call[["f"]])
  l <- eval(i$call[["lower"]])
  u <- eval(i$call[["upper"]])
  x <- seq(l, u, length.out = 100)
  ymin <- 0
  ymax <- f(x)
  ggplot() +
    stat_function(aes(domain), fun = f) +
    geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), ...) +
    labs(title = paste("Definite integral of", as.character(i$call[["f"]])),
         subtitle = sprintf("Lower: %0.2f, Upper: %0.2f, Integral: %0.2f",
                            l, u, i$value))
}

autoplot.function <- function(f, domain = seq(-5, 5, length.out = 100),
                              ..., args = NULL) {
  ggplot() +
    stat_function(aes(domain), fun = f, args = args, ...)
}
