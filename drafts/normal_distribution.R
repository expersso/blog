library(tidyverse)
library(scales)

p <- ggplot(NULL, aes(-4:4)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.1) +
  theme_classic() +
  scale_color_discrete(labels = parse_format()) +
  scale_x_continuous(breaks = -3:3) +
  coord_cartesian(xlim = c(-3, 3)) +
  labs(x = "z", y = expression(phi(z)), color = NULL)

nm <- function(f) {
  out <- deparse(body(f))
  out <- gsub("exp\\((.*)\\)", "e^{\\1}", out)
  paste0("phi(z) == ", out)
}

f <- function(z) z^-1
g <- function(z) z^-2
h <- function(z) exp(-z^2)
i <- function(z) exp(-z^2/2)
j <- function(z) (1/sqrt(2*pi)) * exp(-z^2/2)

pfun <- function(.f) {
  area <- tryCatch(
    round(integrate(.f, -Inf, Inf)$value, 2),
    error = function(e) "divergent"
  )

  int_lbl <- paste0("integral(phi(z)~~dz, -infinity, infinity) %~~% ", area)

  list(
    stat_function(fun = .f, n = 1000, color = "blue"),
    annotate(
      geom  = "text",
      x     = c(-3,-3),
      y     = c(Inf, Inf),
      vjust = c(2, 3),
      hjust = c(0, 0),
      parse = TRUE,
      label = c(nm(.f), int_lbl)
    )
  )
}

p + pfun(j)
