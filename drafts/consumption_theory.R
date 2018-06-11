library(tidyverse)

I <- .75 # Income
p1 <- 2  # Price of good 1
p2 <- 1  # Price of good 2 (numeraire)

B <- function(x1) I - x1 * p1 # Budget constraint

D <- seq(0, 1, length.out = 200) # Domain
U <- expand.grid(x1 = D, x2 = D) # Consumption bundles

utility <- function(x1, x2) x1^0.3 + x2^0.7 # Cobb-Douglas utility function
U$u <- utility(U$x1, U$x2)                  # Utility

ggplot(U, aes(x = x1)) +
  geom_contour(aes(y = x2, z = u, color = ..level..)) +
  geom_line(aes(y = B(x1)), color = "red") +
  scale_color_gradient2(
    low = "red",
    high = "green",
    mid = "blue",
    midpoint = diff(range(U$u)) / 2
  ) +
  coord_equal(
    xlim = 0:1,
    ylim = 0:1,
    expand = FALSE
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(color = "grey60"),
    axis.ticks = element_line(color = "grey60")
  ) +
  labs(
    x = expression(x[1]),
    y = expression(x[2]),
    color = "Utility"
  )

iris %>%
  tbl_df() %>%
  gather(var_type, val, -Species) %>%
  group_by(Species, var_type) %>%
  summarise(lower = quantile(val, .25),
            mean = mean(val),
            upper = quantile(val, .75)) %>%
  gather(stat, val, -Species, -var_type) %>%
  unite(col, var_type:stat) %>%
  spread(col, val)

iris %>%
  group_by(Species) %>%
  summarise_each(funs(lower = quantile(., .25), mean, upper = quantile(., .75)))
