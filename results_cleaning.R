library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)

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
      )
  )

#TODO: Finish dealing with time
results <- results %>%
  mutate(Finish = Finish %>% hms() %>% as.numeric())


ggplot(results, aes(x = Finish/3600, y = Division, fill = gender)) +
  geom_density_ridges2(scale = 2, alpha = 0.5, color = "white") + 
  theme_ridges(grid = FALSE) +
  scale_fill_manual(values = c("#E4002B", "#009CDE")) +
  scale_x_continuous(breaks = 2:10) +
  labs(
    x = "Finish Time (Hours)", 
    y = "Division", 
    title = "Chicago Marathon Times by Division", 
    fill = "Gender"
  )

