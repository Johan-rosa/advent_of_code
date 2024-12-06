library(tidyverse)
library(adventdrob)

eg <- list(
  x = c(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
  )
)

input <- advent_input(1, 2024)
exc <- input

input_df <- tibble(x = exc$x) |>
  separate(x, into = c("x", "y")) |>
  mutate(across(everything(), as.numeric))

# First problem
abs(sort(input_df$x) - sort(input_df$y)) |> sum()

# Second problem
input_df |>
  mutate(
    accurences = map_dbl(x, ~sum(.x == y)),
    similarity = x * accurences
  ) |>
  pull(similarity) |>
  sum()
