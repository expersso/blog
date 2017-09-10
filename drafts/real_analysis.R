library(tidyverse)
library(stringr)

Seq <- function(s, n = 10) {
  a <- eval(substitute(s), list("n" = seq_len(n)))
  structure(a, class = c("sequence", class(n)))
}

print.sequence <- function(n) {
  cat("A sequence:", sep = "\n")
  print(unclass(n))
}

plot.sequence <- function(s) {
  n <- seq_along(s)
  s_formula <- deparse(substitute(s))
  title <- paste("Sequence:", gsub("Seq", "", s_formula))

  ggplot(NULL, aes(x = n, y = s)) +
    geom_point() +
    scale_x_continuous(breaks = n) +
    scale_y_continuous() +
    labs(x = NULL, y = NULL,
         title = title)
}

compare <- function(op) {
  function(s) {
   all(op(head(s, -1), tail(s, -1)))
  }
}

is_increasing <- compare(`<=`)
is_decreasing <- compare(`>=`)
is_strictly_increasing <- compare(`<`)
is_strictly_decreasing <- compare(`>`)
is_monotonic <- function(s) is_increasing(s) || is_decreasing(s)

is_upper_bound <- function(s, b) all(s <= b)
is_lower_bound <- function(s, b) all(s >= b)
is_maximum <- function(s, b) is_upper_bound(s, b) && b %in% s
is_minimum <- function(s, b) is_lower_bound(s, b) && b %in% s

converges <- function(s, L, eps = 0.1, min_run = 100) {
  diff <- abs(s - L) < eps
  tail_trues <- tail_while(diff, ~.x)
  n <- length(tail_trues)
  N <- length(s) - n
  out <- n >= min_run
  attr(out, "N") <- N
  attr(out, "eps") <- eps
  out
}

diverges <- function(s) !converges(s)

s <- Seq(1/n)
s1 <- Seq(2^n)
s2 <- Seq(sin(n))
s3 <- Seq(factorial(n))

plot(Seq(2^n) + Seq(sin(n)))
