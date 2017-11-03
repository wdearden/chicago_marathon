library(aqrModel)

smooth <- 0.025
hours <- c(1, 2)
minutes <- c(30, 0)
half_times <- hours*3600 + minutes*60

results2 <- results %>%
  select(HALF, Finish) %>%
  filter(!is.na(HALF), !is.na(Finish))
  
results3 <- tibble(half_time = half_times) %>%
  mutate(data = map(half_time, ~ results2)) %>%
  unnest() %>%
  mutate(results_weight = exp(-smooth*abs(HALF - half_time)))

results3 %>%
  arrange(half_time, Finish) %>%
  group_by(half_time) %>%
  mutate(cdf = cumsum(results_weight) / sum(results_weight))

with(results2, {
    plot(HALF, Finish)
    lines(ksmooth(HALF, Finish, "normal", bandwidth = 60), col = 2)
    lines(ksmooth(HALF, Finish, "normal", bandwidth = 120), col = 3)
})

ksmooth_model <- results2 %>%
  filter(Finish/HALF < 3) %>%
  with(ksmooth(HALF, Finish, "normal", bandwidth = 60)) %>%
  as_tibble()
