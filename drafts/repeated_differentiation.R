f <- function(x) x^5
g <- function(x) x^2
h <- function(x) f(g(x))
i <- function(x) 6 * x^5

d <- function(f, delta = 0.01) {
  function(x) {
    ( f(x + delta) - f(x) ) / delta
  }
}

x <- -5:5

iterate <- function(f, n, .init, ...) {
  accumulate(seq_len(n), ~f(.), .init = .init, ...)
}

funs <- iterate(d, n = 3, f)

reduce(funs, function(x, y) x + stat_function(aes(x), fun = y),
       .init = ggplot() + coord_equal(ylim = range(x)))
