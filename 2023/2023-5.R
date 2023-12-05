library(tidyverse)
library(adventdrob)


# Inputs ------------------------------------------------------------------

input <- advent_input(5, 2023)

x <- c(
  "seeds: 79 14 55 13",
  "",
  "seed-to-soil map:",
  "50 98 2",
  "52 50 48",
  "",
  "soil-to-fertilizer map:",
  "0 15 37",
  "37 52 2",
  "39 0 15",
  "",
  "fertilizer-to-water map:",
  "49 53 8",
  "0 11 42",
  "42 0 7",
  "57 7 4",
  "",
  "water-to-light map:",
  "88 18 7",
  "18 25 70",
  "",
  "light-to-temperature map:",
  "45 77 23",
  "81 45 19",
  "68 64 13",
  "",
  "temperature-to-humidity map:",
  "0 69 1",
  "1 0 69",
  "",
  "humidity-to-location map:",
  "60 56 37",
  "56 93 4"
)

input <- tibble(
  x = x
)


# Logic -------------------------------------------------------------------
input <- input |>
  extract(x, "type", "^(.*):", remove = FALSE) |>
  mutate(type = str_remove(type, " map")) |>
  fill(type) |>
  filter(x != "")

seeds <- input |>
  filter(type == "seeds") |>
  mutate(seed = str_extract_all(x, "\\d+")) |>
  unnest(seed) |>
  select(seed) |>
  mutate(seed = as.numeric(seed))


get_map <- function(data, map_type) {
  names <- str_split(map_type, "-to-") |>
    unlist()
  
  data |>
    filter(
      type == map_type,
      str_detect(x, "^\\d")
    ) |> 
    extract(
      x,
      into = c("destination_start", "source_start", "range_length"),
      "(\\d+) (\\d+) (\\d+)"
    ) |>
    mutate(
      across(-type, as.numeric),
      sd_gap = source_start - destination_start
    ) |>
    mutate(
      source_destination = pmap(
        list(
          source = source_start,
          length = range_length,
          gap = sd_gap
        ),
        function(source, length, gap){
          tibble(
            source = seq(source, source + length - 1),
            destination = source - gap
          )
        }
      )
    ) |>
    select(source_destination) |>
    unnest(source_destination) |>
    setNames(names)
}

maps_names <- input |>
  filter(type != "seeds") |>
  pull(type) |>
  unique()

maps <- maps_names |>
  map(\(type) get_map(input, type)) |>
  setNames(maps_names)

data <- purrr::reduce(
  maps,
  function(source, map) {
    variables <- names(map)
    data <- left_join(source, map)
    data[[variables[2]]] <- ifelse(
      is.na(data[[variables[2]]]),
      data[[variables[1]]],
      data[[variables[2]]]
    )
    
    data
  },
  .init = seeds
)
