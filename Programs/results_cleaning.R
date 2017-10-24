library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
library(ggridges)
library(parallel)
library(pbapply)

load("results_raw.rda")
load("individual_results_raw.rda")

individual_results_raw <- transpose(individual_results_raw)

states <- read_csv("states.csv")

countries <- read_csv("countries.csv")

results <- results_raw %>%
  select(-Race.Day.Photos, -X) %>%
  mutate(
    start_time = map_chr(individual_results_raw[[2]], ~ .x$X2),
    splits = individual_results_raw[[5]],
    PND = Place.Overall == "PND"
  ) %>%
  mutate_at(vars(starts_with("Place")), as.integer) %>%
  separate(
    Name..CTZ., 
    into = c("last_name", "first_name", "country_code"), 
    sep = ",(?=[^,]+$)|\\((?=[:alnum:]+\\)$)|\\)$"
  ) %>%
  mutate(last_name = gsub("»", "", last_name)) %>%
  separate(City..State, into = c("city", "state_abbr"), sep = ",") %>%
  mutate_if(is.character, str_trim) %>%
  left_join(states) %>%
  left_join(countries) %>%
  mutate_at(vars(country, state, Division, country_code), as.factor)

num_women <- results %>%
  count(Place.Gender) %>%
  filter(n > 1) %$%
  Place.Gender %>%
  max(na.rm = TRUE)
  
results <- results %>%
  mutate(
    gender =
      case_when(
        Place.Gender > num_women ~ "M",
        2*Place.Gender > Place.Overall ~ "M",
        2*Place.Gender < Place.Overall  ~ "F"
      ) %>% as.factor()
  ) %>%
  unite(gender_division, gender, Division, remove = FALSE)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, {library(dplyr)})

results$splits <- 
  pblapply(results$splits, function(x) mutate_all(x, as.character), cl = cl)

results2 <- results

results <- results %>%
  unnest() %>%
  select(-HALF, -Finish, -`Time Of Day`, -Diff, -`min/mile`, -`miles/h`) %>%
  filter(!endsWith(Split, "*")) %>%
  spread(Split, Time) %>%
  select(last_name, first_name, city, state, country, gender, Age, Division,
         gender_division, BIB, Place.Overall, Place.Gender, Place.Division, 
         `05K`, `10K`, `15K`, `20K`, HALF, `25K`, `30K`, `35K`, `40K`, Finish, 
         state_abbr, country_code, PND) %>%
  mutate_at(vars(`05K`:Finish), compose(as.numeric, hms))

save(results, file = "Cleaned/results.rda")

