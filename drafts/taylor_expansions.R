f <- function(x) -x^2 + 4*x
g <- function(x) x^2 - 6*x + 5
f_g <- function(x) f(x) - g(x)
x <- seq(0, 5, 0.1)

annotation_axes <- function() {
  list(geom_hline(yintercept = 0, color = "grey70"),
       geom_vline(xintercept = 0, color = "grey70")
  )
}

theme_math <- function() {
  list(
    theme_bw(),
    theme(panel.grid = element_blank(),
          panel.border = element_blank()),
    scale_x_continuous(expand = c(0, 0)),
    scale_y_continuous(expand = c(0, 0)),
    annotation_axes(),
    labs(color = NULL, fill = NULL)
  )
}

x1 <- uniroot(f_g, lower = 0, upper = 1)$root
x2 <- uniroot(f_g, lower = 1, upper = 5)$root

z <- seq(x1, x2, 0.01)

ggplot() +
  theme_math() +
  stat_function(aes(x, color = "f"), fun = f) +
  stat_function(aes(x, color = "g"), fun = g) +
  geom_ribbon(aes(x = z, ymin = g(z), ymax = f(z),
                  fill = "f(x) - g(x)"), alpha = 0.25)

integrate(f_g, x1, x2)

taylor <- function(f, location, order) {
  taylor_approx <- Taylor(f(x), x, location, order)
  function(x) {
    suppressWarnings(Eval(taylor_approx, list(x = x)))
  }
}

domain <- seq(-pi, pi, 0.1)

ggplot() +
  stat_function(aes(x = domain), fun = cos) +
  stat_function(aes(x = domain), fun = taylor(cos, 0, 6), color = "red") +
  stat_function(aes(x = domain), fun = taylor(cos, 0, 4), color = "blue")
