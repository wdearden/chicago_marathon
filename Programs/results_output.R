library(tidyverse)
library(ggridges)

load("Cleaned/results.rda")

finishers <- results %>%
  filter(!is.na(Finish), !is.na(gender), Finish < 9*3600)

# Results by age group
finishers %>%
  ggplot(aes(x = Finish/3600, y = Division, fill = gender)) +
  geom_density_ridges2(scale = 2, alpha = 0.5, color = "black") + 
  theme_ridges(grid = TRUE) +
  scale_fill_manual(values = c("#E4002B", "#009CDE")) +
  scale_x_continuous(breaks = 2:9) +
  labs(
    x = "Finish Time (Hours)", 
    y = "Division", 
    title = "Chicago Marathon Times by Division", 
    fill = "Gender"
  )

# Final time versus split
finishers %>%
  ggplot(aes(x = HALF/3600, y = Finish/3600, color = gender)) +
  geom_point(alpha = 0.05) +
  scale_color_manual(values = c("#E4002B", "#009CDE")) +
  geom_abline(intercept = c(0, 0), slope = 2) +
  ggthemes::theme_economist() +
  labs(
    x = "Half Marathon Split (Hours)", 
    y = "Finish Time (Hours)", 
    title = "Very Few Runners Negative Split", 
    color = "Gender"
  )

# Pacing of elite men
finishers %>%
  filter(gender == "M", (HALF < 1.25*3600 | Finish < 2.5*3600) & Finish < 2.75*3600) %>%
  ggplot(aes(x = HALF/3600, y = Finish/3600)) +
  geom_point(size = 2) +
  geom_abline(intercept = c(0, 0), slope = 2) +
  ggabbott::theme_abbott(base_family = "Calibri") +
  ggabbott::scale_color_abbott() +
  labs(
    x = "Half Marathon Split (Hours)", 
    y = "Finish Time (Hours)", 
    title = "Splits of Elite Men"
  )
