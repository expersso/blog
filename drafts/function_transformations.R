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
  force(op); force(inside)
  function(f, p) {
    force(f); force(p)
    function(x, ...) {
      if(inside) f(op(x, p), ...) else op(f(x, ...), p)
    }
  }
}

shift_h  <- make_trans_func(`+`)
shift_v  <- make_trans_func(`+`, FALSE)
dilate_h <- make_trans_func(`*`)
dilate_v <- make_trans_func(`*`, FALSE)
flip_h   <- partial(make_trans_func(`*`), p = -1)
flip_v   <- partial(make_trans_func(`*`, FALSE), p = -1)

g <- function(x) x^3
x <- -10:10

df <- data_frame(x = x,
           cube = g(x),
           `cube flipped vertically` = flip_v(g)(x),
           `cube flipped horizontally` = flip_h(g)(x),
           `cube flipped horizontally & vertically` = flip_v(flip_h(g))(x)
           ) %>%
  gather(fun, y, -x)

ggplot(df, aes(x, y)) +
  geom_line() +
  facet_wrap(~fun)

is_even <- function(f, x, ...) {
  isTRUE(all.equal(f(x, ...), flip_h(f)(x, ...)))
}

is_odd <- function(f, x, ...) {
  isTRUE(all.equal(f(x, ...), flip_v(flip_h(f))(x, ...)))
}

is_odd(function(x) x^3, -10:10)

is_even(cos, -10:10)
is_odd(sin, -10:10)

xprod <- function(...) {
  vecs <- list(...)
  m <- do.call(rbind, vecs)
  map_dbl(seq_len(ncol(m)), ~det(m[, -., drop = FALSE]) * (-1) ^ .)
}

u <- c(-2, 4, 1)
v <- c(3, -5, 2)

z <- xprod(u, v)

to_spherical <- function(v, degrees = TRUE) {
  stopifnot(length(v) == 3)
  norm <- function(v) sqrt(sum(v^2))
  rad2deg <- function(x) x * 180 / pi

  proj    <- v[-3]
  r       <- norm(v)
  polar   <- asin(norm(proj) / r)
  azimuth <- atan(v[2] / v[1])

  if(degrees) {
    polar   <- rad2deg(polar)
    azimuth <- rad2deg(azimuth)
  }

  list("r" = r, "polar" = polar, "azimuth" = azimuth)
}

to_spherical(z)

ggplot(data.frame(x = 1:5)) +
  geom_jitter(aes(x, y = gamma(x + 1))) +
  geom_jitter(aes(x, y = factorial(x)))
