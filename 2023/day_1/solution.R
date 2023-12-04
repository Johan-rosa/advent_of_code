library(stringr)

codes <- readLines("2023/day_1/input")

# First Start -------------------------------------------------------------
digits <- str_remove_all(codes, "[A-Za-z]") |>
  str_replace('^(\\d)\\d+(\\d)$', "\\1\\2")

digits <- ifelse(
  str_length(digits) == 1,
  paste0(digits, digits),
  digits
) |> 
  as.numeric() |>
  sum(na.rm = TRUE)


# Second Start -----------------------------------------------------------------
new_example <- c(
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"
)

numbers <- c(
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
)

#paste(numbers, collapse = "|")

number_patter <- "(one|two|three|four|five|six|seven|eight|nine)"

pices <- new_example[6] |>
  str_replace_all("(\\d)", " \\1 ") |>
  str_squish() |> 
  str_split("\\s") |>
  unlist()

pices[1] |>
  str_match_all(number_patter)
