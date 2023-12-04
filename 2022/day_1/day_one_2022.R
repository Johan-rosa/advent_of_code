library(tidyverse)
library(adventdrob)

input <- advent_input(1, 2022) |>
  setNames("calories") |>
  mutate(
    elf_id = cumsum(calories == ""),
    calories = as.numeric(calories)
  ) |>
  filter(!is.na(calories))

calories_by_elf <- input |>
  summarise(
    total_calories = sum(calories),
    .by = elf_id
  ) |>
  arrange(total_calories) |>
  mutate(
    cum_calories = zoo::rollsum(total_calories, k = 3, na.pad = TRUE, align = "right")
  )

calories_by_elf |>
  tail(3)
  top_n(wt = total_calories, n = 3)
  
  
  
