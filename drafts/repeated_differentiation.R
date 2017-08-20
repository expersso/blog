library(tidyverse)

f <- function(x) x^5
g <- function(x) x^2
h <- function(x) f(g(x))
i <- function(x) 6 * x^5

d <- function(f, delta = 0.001) {
  force(f)
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

make_exp_f <- function(k) {
  force(k)
  function(x) {
    k ^ x
  }
}

x <- seq(0, 5, 0.1)
ks <- seq(2, 3, 0.25)
funs <- map(ks, make_exp_f) %>%
  set_names(paste0("f_", ks))

d_funs <- map(funs, d) %>%
  set_names(paste0("d_", ks))

df <- c(funs, d_funs) %>%
  invoke_map_df(list(list(x))) %>%
  mutate(x = x) %>%
  gather(k, y, -x) %>%
  separate(k, c("type", "k"), sep = "_")

ggplot(df, aes(x, y, color = type)) +
  geom_line() +
  stat_function(fun = exp, linetype = "dashed") +
  coord_equal(ylim = c(0, 3), xlim = c(0, 3)) +
  facet_wrap(~k)

i <- function(f) {
  force(f)
  function(a, b, delta = 0.001) {
    x <- seq(a, b, by = delta)
    cumsum(f(x) * delta)
  }
}

x <- seq(-2, 2, by = 0.001)
f <- function(x) x^2
plot(x, i(f)(-2, 2))
