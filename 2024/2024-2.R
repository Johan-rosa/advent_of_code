library(tidyverse)
library(adventdrob)

eg <- list(
  x = c(
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
  )
)

input <- advent_input(2, 2024)

exc <- input

exc$x |> map_lgl(\(x) {
  values <- str_split(x, " ") |> 
    unlist() |> 
    as.numeric()

  values_diff <- diff(values)
  same_direction <- all(values_diff > 0) | all(values_diff < 0)
  all_in_threshold <- all(abs(values_diff) < 4)
  
  same_direction && all_in_threshold
}) |>
  sum()


