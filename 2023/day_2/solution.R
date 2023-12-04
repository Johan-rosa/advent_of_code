library(tidyverse)
library(adventdrob)

input <- advent_input(2, 2023)

bag <- tibble(
  color = c("red", "green", "blue"),
  config = c(12, 13, 14)
)

games_results <- input |>
  rowid_to_column("game") |>
  mutate(x = str_remove(x, "Game \\d+:")) |>
  separate_rows(x, sep = ";") |>
  mutate(
      subset = row_number(),
      .by = game
  ) |> 
  separate_rows(x, sep = ",") |>
  mutate(x = str_squish(x)) |> 
  separate(x, into = c("n", "color"), sep = " ") |>
  select(game, subset, color, n) |>
  left_join(bag, by = "color") |>
  mutate(
    n = as.numeric(n),
    possible = n <= config
  )

# Game 1
games_results |>
  summarise(
   possible = all(possible),
   .by = game
  ) |>
  summarise(
    sum_id = sum(game),
    .by = possible
  )

# Grame 2
games_results |>
  summarise(
    n = max(n),
    .by = c(game, color)
  ) |>
  group_by(game) |> 
  summarise(
    power = prod(n)
  ) |>
  pull(power) |>
  sum()
