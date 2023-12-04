library(tidyverse)
library(adventdrob)

input <- advent_input(2, 2022)
example <- data.frame(
  x = c(
    "A Y",
    "B X",
    "C Z"
  )
)

dictionary <- list(
  A = list( # Rock
    X = "DRAW",
    Y = "WIN",
    Z = "LOSE"
  ),
  B = list( # Paper
    X = "LOSE",
    Y = "DRAW",
    Z = "WIN"
  ),
  C = list( # Scissors
    X = "WIN",
    Y = "LOSE",
    Z = "DRAW"
  ),
  result_score = list(
    WIN  = 6,
    DRAW = 3,
    LOSE = 0
  ),
  shape_score = list(
    A = 1, # Rock
    X = 1, # Rock
    C = 2, # Paper
    Y = 2, # Paper
    B = 3, # Scissors
    Z = 3  # Scissors
  )
)

get_shape_score <- function(dictionary) {
  dictionary$shape_score |>
    as_tibble() |>
    pivot_longer(everything(), names_to = "shape", values_to = "shape_score")
}

get_result_score <- function(dictionary) {
   dictionary$result_score |>
    as_tibble() |>
    pivot_longer(everything(), names_to = "result", values_to = "result_score") 
}

get_result_matrix <- function(dictionary, names_to, values_to) {
  dictionary[c("A", "B", "C")] |>
    bind_rows(.id = "elf") |>
    pivot_longer(cols = -elf, names_to = names_to, values_to = values_to)
}

shape_score <- get_shape_score(dictionary)
result_score <- get_result_score(dictionary)
results_matrix <- get_result_matrix(dictionary, "you", "result")

results <- input |>
  separate(x, into = c("elf", "you")) |>
  left_join(results_matrix, by = c("elf", "you")) |>
  left_join(shape_score, by = c("you" = "shape")) |>
  left_join(shape_score, by = c("elf" = "shape"), suffix = c("_you", "_efl")) |>
  left_join(result_score) |> 
  mutate(
    you_score = shape_score_you + result_score
  )

example_result <- example |>
  separate(x, into = c("elf", "you")) |>
  left_join(results_matrix, by = c("elf", "you")) |>
  left_join(shape_score, by = c("you" = "shape")) |>
  left_join(shape_score, by = c("elf" = "shape"), suffix = c("_you", "_efl")) |>
  left_join(result_score) |> 
  mutate(
    you_score = shape_score_you + result_score
  )

results$you_score |> sum()
example_result$result_score |> sum()


# Part 2 ------------------------------------------------------------------

dictionary <- list(
  A = list( # Rock
    X = "C",
    Y = "A",
    Z = "B"
  ),
  B = list( # Paper
    X = "A",
    Y = "B",
    Z = "C"
  ),
  C = list( # Scissors
    X = "B",
    Y = "C",
    Z = "A"
  ),
  result_score = list(
    Z  = 6, # Win
    Y = 3,  # Draw
    X = 0   # Lose
  ),
  shape_score = list(
    A = 1, # Rock
    B = 2, # Paper
    C = 3 # Scissors
  )
)

shape_score <- get_shape_score(dictionary)
result_score <- get_result_score(dictionary)
results_matrix <- get_result_matrix(dictionary, "result", "you")

results <- input |>
  separate(x, into = c("elf", "result")) |>
  left_join(results_matrix, by = c("elf", "result")) |>
  left_join(shape_score, by = c("you" = "shape")) |>
  left_join(shape_score, by = c("elf" = "shape"), suffix = c("_you", "_elf")) |>
  left_join(result_score) |> 
  mutate(
    you_score = shape_score_you + result_score
  )

results$you_score |> sum()
