library(tidyverse)

plot_surpluses <- function(demand, supply, domain) {

  # Equilibrium quantity
  q <- uniroot(function(x) demand(x) - supply(x), domain)$root

  # Equilibrium price
  p <- supply(q)

  # Domain
  x <- seq(domain[1], domain[2], 0.1)

  # Dummy domain for geom_ribbon
  z <- seq(0, q, 0.01)

  ggplot() +
    stat_function(aes(x, color = "Demand"), fun = demand) +
    stat_function(aes(x, color = "Supply"), fun = supply) +
    geom_ribbon(aes(x = z, ymin = supply(z), ymax = p,
                    fill = "Producer surplus"), alpha = 0.25) +
    geom_ribbon(aes(x = z, ymin = p, ymax = demand(z),
                    fill = "Consumer surplus"), alpha = 0.2) +
    annotate("segment", x = q, xend = q,
             y = 0, yend = p,
             linetype = "dashed", color = "grey30") +
    annotate("segment", x = 0, xend = q,
             y = p, yend = p,
             linetype = "dashed", color = "grey30") +
    annotate("point", x = q, y = p, color = "grey30") +
    scale_x_continuous(expand = c(0, 0), breaks = q, labels = "q*") +
    scale_y_continuous(expand = c(0, 0), breaks = p, labels = "p*") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          legend.position = c(1, 1), legend.justification = c(1, 1)) +
    labs(x = "Quantity", y = "Price", color = NULL, fill = NULL)
}

demand <- function(q) (q - 10)^2
supply <- function(q) q^2 + 2*q + 8

plot_surpluses(demand, supply, domain = c(0, 5))
