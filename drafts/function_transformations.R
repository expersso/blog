library(tidyverse)

polar_to_cart <- function(r, theta) {
  list(x = cos(theta) * r, y = sin(theta) * r)
}

f <- function(theta) exp(cos(theta)) - 2*cos(4*theta) + sin(theta/12)^5

df <- tibble(
  theta = seq(0, 2*pi, 0.01),
  r     = sin(3*theta),
  x     = polar_to_cart(r, theta)$x,
  y     = polar_to_cart(r, theta)$y
)

ggplot(df, aes(x, y)) +
  geom_path() +
  coord_cartesian()

polar <- function(r, theta) {
  x <- list("r" = r, "theta" = theta)
  structure(x, class = c("polar", class(x)))
}

x <- polar(1, pi/2)

print.polar <- function(x) {
  if(length(x$r) == 1) {
  cat(sprintf("r = %0.2f, θ = %0.2f", x$r, x$theta))
  } else {
    print(as.data.frame(x))
  }
}

as.double.polar <- function(x) {
  list(
    "x" = x$r * cos(x$theta),
    "y" = x$r * sin(x$theta)
  )
}

as.polar <- function(x, y) {
  r <- sqrt(x^2 + y^2)
  theta <- acos(x/r)
  polar(r, theta)
}

plot.polar <- function(x, ...) {
  num <- as.numeric(x)
  plot(num$x, num$y, ...)
}

f <- function(n) sin(10*n) / (10*n)
g <- function(n) 1/ (10*n)
ggplot() +
  stat_function(aes(seq(2, 50, 0.01)), fun = f, n = 10000) +
  stat_function(aes(seq(2, 50, 0.01)), fun = g, n = 10000) +
  stat_function(aes(seq(2, 50, 0.01)), fun = neg(g), n = 10000)

neg <- function(f) {
  function(...) -f(...)
}

neg <- function(x) UseMethod("neg")
neg.default <- function(x) .NotYetImplemented()
neg.numeric <- function(x) -x
neg.function <- function(f) function(...) -f(...)

shift_horizonally <- function(f, p) {
  function(x, ...) {
    f(x + p, ...)
  }
}

shift_vertically <- function(f, p) {
  function(x, ...) {
    f(x, ...) + p
  }
}

dilate_horizontally <- function(f, p) {
  function(x, ...) {
    f(x * p, ...)
  }
}

dilate_vertically <- function(f, p) {
  function(x, ...) {
    f(x, ...) * p
  }
}

make_trans_func <- function(op, inside = TRUE) {
  function(f, p) {
    function(x, ...) {
      if(inside) f(op(x, p), ...) else op(f(x, ...), p)
    }
  }
}

shift_horizonally   <- make_trans_func(`+`)
dilate_horizontally <- make_trans_func(`*`)
shift_vertically    <- make_trans_func(`+`, FALSE)
dilate_vertically   <- make_trans_func(`*`, FALSE)

g <- function(x) sin(x)
x <- -10:10

ggplot() +
  stat_function(aes(x), fun = g) +
  stat_function(aes(x), fun = dilate_vertically(g, 8), color = "blue")
