library(tidyverse)
library(scales)

matching <- function(n) sum(seq_len(n) == sample(n))

rerun_matching <- function(n, reps) {
  s <- seq_len(n)
  df <- tibble(
    "id" = s,
    "result" = map(s, ~flatten_int(rerun(reps, matching(.))))
    )
  unnest(df)
}

df <- rerun_matching(10, 50)

ggplot(df, aes(x = id, y = result)) +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.25) +
  stat_summary(fun.y = mean, geom = "point", color = "red", size = 3) +
  scale_x_continuous(breaks = seq_along(df$id)) +
  scale_y_continuous(breaks = seq(min(df$result), max(df$result))) +
  theme_classic() +
  labs(x = "\nNumber of cards",
       y = "Number of matches\n",
       title = "de Montmort's matching problem",
       subtitle = "Number of matches by number of cards in deck",
       caption = "Red dots denotes simulated mean number of matches. Dotted line indicates ")
