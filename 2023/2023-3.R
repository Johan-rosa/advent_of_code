library(tidyverse)
library(adventdrob)

input <- advent_input(3, 2023) |>
  rownames_to_column("line")

numbers_positions <- input$x |>
  str_locate_all("\\d+") |>
  map(as_tibble)

checking_boundaries <- map(
  numbers_positions,
  ~mutate(
    .x,
    start = ifelse(start > 1, start -1, start),
    end   = ifelse(end == 140, end, end + 1)
  )
)

check_above <- function(line, start, end) {
  if (line == 1) return(FALSE)
  line_content <- input$x[line - 1]
  focus_pice <- str_sub(line_content, start = start, end = end)
  str_detect(focus_pice, "[^\\d\\.]")
}

check_aside <- function(line, start, end) {
  line_content <- input$x[line]
  string <- str_sub(line_content, start = start, end = end)
  str_detect(string, "[^\\d\\.]")
}

check_below <- function(line, start, end) {
  if (line == 140) return(FALSE)
  line_content <- input$x[line + 1]
  string <- str_sub(line_content, start = start, end = end)
  str_detect(string, "[^\\d\\.]")
}
  

checking_boundaries |>
  bind_rows(.id = "line") |>
  mutate(
    line = as.numeric(line),
    symbol_above = pmap_lgl(
      list(line = line, start = start, end = end),
      check_above
    ),
    symbol_below = pmap_lgl(
      list(line = line, start = start, end = end),
      check_below
    ),
    symbol_aside = pmap_lgl(
      list(line = line, start = start, end = end),
      check_aside
    )
  )
