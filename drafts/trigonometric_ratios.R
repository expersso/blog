library(tidyverse)

circle <- function(r = 1, x = 0, y = 0) {
  tibble(theta = seq(0, 2 * pi, 0.01),
         x     = r * cos(theta) + x,
         y     = r * sin(theta) + y)
}

secpi <- function(x) 1 / cospi(x)
cscpi <- function(x) 1 / sinpi(x)
degpi <- function(x) x * 180

get_annotations <- function(theta) {

  ct <- cospi(theta)
  st <- sinpi(theta)
  ss <- secpi(theta)
  cs <- cscpi(theta)
  d1 <- degpi(theta)
  d2 <- d1 - 90

  tribble(
    ~x,  ~xend,  ~y, ~yend, ~name,    ~color,     ~angle, ~vjust, ~hjust,
    0,  ct,     0,  st,   "radius", "grey20",     d1,    -.5,    .5,
    ct, ct,     0,  st,   "sin",    "orange",    -90,    -.5,    .5,
    0,  ct,     st, st,   "cos",    "red",         0,    -.5,    .5,
    ct, ss,     st, 0,    "tan",    "steelblue",  d2,    -.5,     1,
    0,  ss,     0,  0,    "sec",    "skyblue",     0,      1,    .5,
    0,  0,      0,  cs,   "csc",    "purple",     90,    -.5,    .5,
    0,  ct,     cs, st,   "cot",    "rosybrown",  d2,    -.5,     1
  )
}

ann <- get_annotations(1/3)

ggplot(circle(), aes(x, y)) +
  geom_path(color = "grey70") +
  geom_segment(aes(xend = xend, yend = yend, color = color), ann) +
  geom_text(aes(
    x = (x + xend) / 2,
    y = (y + yend) / 2,
    label = name,
    color = color,
    angle = angle,
    vjust = vjust,
    hjust = hjust),
  data = ann) +
  scale_color_identity() +
  coord_equal(xlim = c(-2, 2), ylim = c(-2, 2)) +
  theme_void()
