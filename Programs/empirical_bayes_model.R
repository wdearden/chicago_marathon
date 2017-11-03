finishers <- filter(results, !is.na(Place.Overall), !is.na(Finish))

finishers <- finishers %>%
  mutate(
    gender_division = as.character(gender_division),
    gender_division = as.factor(gender_division)
  )

finishers_by_division <- finishers %>%
  split(.$gender_division)

kernels <- map(finishers_by_division, ~ density(.x$Finish, adjust = 1))

density_tibbles <- map(kernels, ~ tibble(x = .x$x, y = .x$y))
densities <- bind_rows(density_tibbles, .id = "gender_division")

ggplot(densities, aes(x = x, y = y, color = gender_division)) +
  geom_point()

library(forecast)
fit <- lm(Finish ~ HALF*gender_division, data = finishers)
forecasted_finish <- forecast(fit, newdata = finishers)
forecasted_finishers <- bind_cols(finishers, as_tibble(forecasted_finish))


