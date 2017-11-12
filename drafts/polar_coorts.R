library(tidyverse)
library(scales)

to_polar <- function(x, y) {
  list("h" = sqrt(x^2 + y^2),
       "v" = atan(y/x))
}

to_rect <- function(r, t) {
  list("h" = r * cos(t),
       "v" = r * sin(t) )
}

segment <- function(h, v, hend, vend, length.out = 20) {
  tibble(
    h = seq(h, hend, length.out = length.out),
    v = seq(v, vend, length.out = length(h))
  )
}

draw_coord_systems <- function(h, v, hend, vend, coord) {

  lmax <- abs(max(c(h, v, hend, vend)))
  lmin <- -lmax

  # Main rectangle
  # df <- bind_rows(
  #   segment(h, v, hend, v),
  #   segment(hend, v, hend, vend),
  #   segment(hend, vend, h, vend),
  #   segment(h, vend, h, v)
  # )
  df <- bind_rows(
    segment(h, v, hend, v),
    segment(hend, v, h, vend),
    segment(h, vend, h, v)
  )
  df$coord <- coord

  # Translated rectangle
  .f <- if(coord == "polar") to_rect else to_polar
  df_translated <- bind_rows(map2(df$h, df$v, .f))
  df_translated$coord <- if(coord == "polar") "rectangular" else "polar"

  ggplot(NULL, aes(h, v)) +
    geom_path(data = df) +
    geom_path(data = df_translated) +
    coord_equal() +
    facet_wrap(~coord) +
    scale_x_continuous(limits = c(lmin, lmax), breaks = pretty_breaks(10)) +
    scale_y_continuous(limits = c(lmin, lmax), breaks = pretty_breaks(10)) +
    labs(x = NULL, y = NULL)
}
