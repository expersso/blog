sort_by <- function(x, f, ..., decreasing = FALSE, na.last = TRUE,
                    method = c("auto", "shell", "radix")) {
  x[order(f(x, ...), decreasing = decreasing, na.last = na.last)]
}

gcd <- Vectorize(
  function(x, y) {
    r <- x %% y
    if(!r) y else gcd(y, r)
  }
)

