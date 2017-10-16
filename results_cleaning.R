library(tidyverse)
library(stringr)

load("results_raw.rda")
load("individual_results_raw.rda")

individual_results_raw <- transpose(individual_results_raw)

url <- "https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"
states <- url %>%
  read_csv() %>%
  rename(state_abbr = Abbreviation, state = State)

results <- results_raw %>%
  select(-Race.Day.Photos, -X) %>%
  mutate(
    start_time = map_chr(individual_results[[2]], ~ .x$X2),
    splits = individual_results[[5]],
    PND = Place.Overall == "PND"
  ) %>%
  mutate_at(vars(starts_with("Place")), as.integer) %>%
  separate(
    Name..CTZ., 
    into = c("last_name", "first_name", "country"), 
    sep = ",(?=[^,]+$)|\\((?=[:alnum:]+\\)$)|\\)$"
  ) %>%
  mutate(last_name = gsub("»", "", last_name)) %>%
  separate(City..State, into = c("city", "state_abbr"), sep = ",") %>%
  mutate_if(is.character, str_trim) %>%
  left_join(states)


