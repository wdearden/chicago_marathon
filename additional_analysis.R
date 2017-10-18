bq_times <- read_csv("bq_times.csv") %>% mutate_if(is.character, as.factor)

boston_qualifiers <- finishers %>%
  left_join(bq_times) %>%
  mutate(bq_time = 3600*bq_hours + 60*bq_minutes, bq = Finish <= bq_time) %>%
  select(-bq_hours, -bq_minutes) %>%
  filter(bq)

cheaters <- finishers %>%
  mutate(
    pace_5_20 = (`20K` - `05K`) / (12.4 - 3.1),
    pace_5_half = (HALF - `05K`) / (13.1 - 3.1),
    pace_5_25 = (`25K` - `05K`) / (15.5 - 3.1)
  ) %>%
  filter(pace_5_20 < 300 | pace_5_half < 300 | pace_5_25 < 300)
