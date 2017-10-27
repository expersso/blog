library(tidyverse)
library(scales)

draw <- function(n, start, pos_ret, neg_ret, p_pos) {
  rbinom(n, 1, p_pos) %>%
    ifelse(pos_ret, neg_ret) %>%
    accumulate(`*`, .init = start)
}

to_df <- function(.n, ...) {
  rerun(.n, tibble(value = draw(...), t = seq_along(value))) %>%
    bind_rows(.id = "id")
}

df <- to_df(300, n = 12, start = 100, pos_ret = 1.7, neg_ret = 0.4, p_pos = 0.5)

p <- ggplot(df, aes(x = t, y = value)) +
  theme_minimal() +
  labs(x = "Period", y = "Price", color = NULL)

p +
  geom_line(aes(group = id), alpha = 0.5, position = position_jitter(),
            color = "grey70") +
  scale_y_log10(labels = comma, breaks = 10^(-1:5))

p +
  stat_summary(aes(color = "Mean"), geom = "line", fun.y = "mean") +
  stat_summary(aes(color = "Median"), geom = "line", fun.y = "median")

# Model probability spaces and random variables

# X :: Probability Space
O <- tribble(
  ~event, ~n,
  "TTT", 3,
  "TTH", 2,
  "THH", 1,
  "HHH", 0,
  "THH", 1,
  "HTH", 1,
  "THT", 2,
  "HHT", 1
)

# f :: Probability Space -> Random Variable
make_rv <- function(Omega) {
  force(Omega)
  X <- function(omega) Omega$n[Omega$event == omega]

  structure(list("X" = X, "Omega" = Omega),
            class = c("rv", class(x)))
}

`<=.rv` <- function(X, x) {
  event <- X$Omega$event
  E <- event[X$X(event) <= x]
  structure(list("event" = E, "Omega" = X$Omega),
            class = c("event", class(x)))
}

P <- function(E) length(E$event) / length(E$Omega$event)

X <- make_rv(O)
P(X <= 0)

library(rgl)

draw_shape <- function(n) {
  f <- function(x, y) sin(x*y) + 6/5
  x <- seq(0.5, 3.5, length.out = n)
  y <- seq(0.5, 2.5, length.out = n)
  z <- outer(x, y, f)
  hist3D(z = z, scale = FALSE, expand = 0.25, phi = 30, theta = 60, bty = "n",
         col = "grey90", border = "grey50", shade = 0.2, d = 2, labs = "")
}

pdf("test.pdf")
par(mfrow = c(2, 2), mar = c(0, 0, 0, 0))
walk(c(5, 10, 25, 100), draw_shape)
dev.off()

int_dx_dy <- function(f, ylower, yupper, xlower, xupper) {
  inner <- function(y) integrate(function(x) f(x, y), xlower, xupper)$value
  integrate(Vectorize(inner), ylower, yupper)
}

double_integral(f, 0.5, 2.5, 0.5, 3.5)

# X ~ Bin(n, p)
X <- function(n, p) {
  function(k) {
    choose(n, k) * p^k * (1 - p)^(n - k)
  }
}

ggplot() +
  geom_jitter(aes(x = 1:10, bin(10, 0.5)(1:10)), color = "blue") +
  geom_jitter(aes(x = 1:10, dbinom(1:10, 10, 0.5), color = "red"))
