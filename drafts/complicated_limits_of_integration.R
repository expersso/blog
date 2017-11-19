# Point of intersection of curves f1 and f2 on given interval
poi <- function(f1, f2, ...) {
  optimise(function(x) abs(f1(x) - f2(x)), ...)$minimum
}

poi <- Vectorize(poi)

# domain
x <- c(0, 2)

# Bounding curves
bc <- list(
  f1 = function(x) x,
  f2 = function(x) 2 * x,
  f3 = function(x) 2 - x,
  f4 = function(x) 1 - x
)

# (x,y) coordinates for polygon
df <- tribble(
    ~F1,    ~F2,
  bc$f1,  bc$f4,
  bc$f1,  bc$f3,
  bc$f2,  bc$f3,
  bc$f2,  bc$f4
) %>%
  mutate(
    x = poi(F1, F2, x),
    y = invoke_map_dbl(F1, x)
  )

geom_fun <- function(.f, x) {
  lbl <- paste0("y = ", deparse(body(.f)))
  stat_function(aes_(x, linetype = lbl), fun = .f)
}

ggplot() +
  geom_polygon(aes(x, y), df, fill = "grey70") +
  map(bc, geom_fun, x = x) +
  annotate("text", x = 0.65, y = 0.95, label = "Delta",
           parse = TRUE, size = 8) +
  coord_equal(1, x, x, expand = 0) +
  theme_classic() +
  theme(legend.position = c(0.88, 0.5)) +
  scale_x_continuous(breaks = 0:2) +
  scale_y_continuous(breaks = 0:2) +
  labs(x = "x", y = "y", linetype = NULL)
