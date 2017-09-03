library(tidyverse)

seq_lim <- function(x, dir = "left", h = 0.01, n = 20) {
  op <- if(dir == "left") `-` else `+`
  seq(op(x, h), x, length.out = n)
}

limit <- function(f, x, n = 20) {
  left <- seq_lim(x, n = n)
  right <- seq_lim(x, "right", n = n)
  left_fx <- map_dbl(left, f)
  right_fx <- map_dbl(right, f)
  tibble(
    "i" = rep(seq_len(n), 2),
    "x" = c(left, right),
    "y" = c(left_fx, right_fx),
    "dir" = rep(c("left", "right"), each = n)
  )
}

f <- function(x) (cos(3*x) - 1) / x^2
y <- sqrt(.Machine$double.eps)
x <- pi/2
limit(tan, pi/2)
square <- function(x) x^2
cube <- function(x) x^3
xs <- limit(cube, 2)
xs2 <- limit(square, 0)
g <- function(x) {
  x <- as.double(x)
  case_when(x < 0 ~ -x,
            x >= 0 ~ x^2 + 1)
}

ggplot(limit(g, 0), aes(x = i, y = y, color = dir)) +
  geom_point()

ggplot() +
  stat_function(geom = "point", fun = g, aes(x = -10:5))


ggplot() +
  stat_function(geom = "point", fun = f, aes(x = -10:5))
