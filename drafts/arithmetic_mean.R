library(purrr)
glue  <- partial(paste0, collapse = "")
repg  <- function(k, x = " ") glue(rep(x, k))
`%+%` <- glue

draw_weights <- function(n, r) {
  weights <- rep("*", r)                             # weight symbols

  x      <- sort(sample(n, r))                       # random weights
  mu     <- round(mean(x))                           # fulcrum position

  spaces <- map(diff(x) - 1, repg)                   # spaces b/w weights
  lspace <- repg(x[1] - 1)                           # space before 1st weight

  top     <- lspace %+% (weights %+% spaces)         # weights spaced out
  ruler   <- repg(n, "-")                            # ruler symbols
  fulcrum <- repg(mu - 1) %+% "^" %+% repg(n - mu)   # fulcrum below ruler

  cat(top, ruler, fulcrum, sep = "\n")               # print
}

walk(1:5, ~draw_weights(50, 5))

library(ggplot2)
library(gridExtra)

draw_weights_gg <- function(n, r) {
  x <- runif(r, 1, n)

  ggplot() +
    geom_point(aes(x, 1), shape = 21, color = "grey10",
               fill = "steelblue", size = 3) +
    geom_point(aes(mean(x), 0), color = "firebrick",
               shape = "^", size = 7) +
    geom_segment(aes(x = 1, xend = n, y = 0.5, yend = 0.5)) +
    theme_void() +
    ylim(0, 5)
}

lift(grid.arrange)(rerun(20, draw_weights_gg(50, 5)))
