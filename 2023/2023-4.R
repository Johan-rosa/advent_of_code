library(tidyverse)
library(adventdrob)

input <- advent_input(4, 2023)
# input <- data.frame(
#   x = c(
#     'Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53',
#     'Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19',
#     'Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1',
#     'Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83',
#     'Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36',
#     'Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11'
#   )
# )

all_numbers <- input |>
  rowid_to_column("card_id") |>
  mutate(
    x = str_remove(x, "^.+:"),
    x = str_squish(x)
  ) |>
  separate(x, into = c("winning", "numbers"), sep = "\\|") |>
  mutate(across(-card_id, str_squish))

winning_numbers <- all_numbers |>
  select(card_id, numbers = winning) |> 
  separate_rows(numbers, sep = " ")

card_numbers <- all_numbers |>
  select(card_id, numbers) |> 
  separate_rows(numbers, sep = " ")

card_points <- card_numbers |>
  semi_join(winning_numbers) |>
  group_by(card_id) |> 
  mutate(
    multipier = ifelse(row_number() == 1, 1, 2)
  ) |>
  summarise(
    card_points = prod(multipier)
  )

card_points$card_points |> sum()


# Part two ----------------------------------------------------------------
card_and_copies <- card_numbers |>
  left_join(mutate(winning_numbers, winning = TRUE)) |>
  mutate(winning = !is.na(winning)) |> 
  group_by(card_id) |>
  summarise(
    copies = sum(winning)
  )

cards <- nrow(card_and_copies)
m <- card_and_copies$copies
copies <- rep(1, n_cards)

for (i in seq_len(cards)) {
  if (m[i] > 0) {
    range <- seq(i + 1, min(i + m[i], cards))
    copies[range] <- copies[range] + copies[i]
  }
}

sum(copies)


