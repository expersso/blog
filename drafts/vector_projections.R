library(tidyverse)

dot <- function(u, v) {
  stopifnot(length(u) == length(v))
  sum(u * v)
}

orthogonal <- function(u, v) dot(u, v) == 0
acute      <- function(u, v) dot(u, v) > 0
obtuse     <- function(u, v) dot(u, v) < 0

norm <- function(u) sqrt(dot(u, u))
norm <- function(u, p = 2) {
  if(is.infinite(p)) return(max(abs(u)))
  sum(abs(u)^p)^(1/p)
}

uv <- function(u) u / norm(u)

proj <- function(u, v) {
  len <- dot(u, v) / norm(v)
  unit_vec <- uv(v)
  len * unit_vec
}

inner_angle <- function(u, v) {
  cos_theta <- dot(u, v) / (norm(u) * norm(v))
  acos(cos_theta)
}

rads2deg <- function(r) (r * 180/pi) %% 360

pv <- function(u, x = 0, y = 0, ...) {
  xend <- x + u[1]
  yend <- y + u[2]
  annotate("segment", x = x, y = y, xend = xend, yend = yend, ...,
           arrow = arrow(length = unit(2, "mm"), type = "closed"))
}

u <- c(1, 3)
v <- c(3, -4)

p <- ggplot() +
  coord_equal() +
  pv(u, color = "red") +
  pv(v, color = "blue")
p

p <- p + pv(uv(v), color = "black")
p
p <- p + pv(proj(u,v), color = "green")
p
p + pv(u - proj(u, v), color = "orange")

orthogonal(u - proj(u, v), v)
(theta <- inner_angle(u - proj(u, v), v))
rads2deg(theta)
