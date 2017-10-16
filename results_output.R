ggplot(finishers, aes(x = Finish/3600, y = Division, fill = gender)) +
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

results3 <- results

results3$state <- fct_lump(results$state, n = 10)

results3 <- results3 %>%
  filter(!is.na(state), !is.na(gender))

ggplot(filter(results3, Finish < 3600*9), aes(x = Finish/3600, y = state, fill = gender)) +
  geom_density_ridges2(scale = 2, alpha = 0.5, color = "black") + 
  theme_ridges(grid = TRUE) +
  scale_fill_manual(values = c("#E4002B", "#009CDE")) +
  scale_x_continuous(breaks = 2:9) +
  labs(
    x = "Finish Time (Hours)", 
    y = "State", 
    title = "Chicago Marathon Times by State", 
    fill = "Gender"
  )
