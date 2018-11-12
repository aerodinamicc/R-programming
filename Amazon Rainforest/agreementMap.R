anomalies %>%
  filter(year == 2005) %>%
  select(lon, lat, year, preTrmm_et100_r, preCru_et100_r) %>%
  mutate(agreement = ifelse(preTrmm_et100_r < -50 & preCru_et100_r < -50, "Confirmed by both",
                            ifelse(preTrmm_et100_r < -50 | preCru_et100_r < -50, "Not confirmed",0))) %>%
  filter(agreement != 0) %>%
  mutate(agreement = factor(agreement, sort(unique(agreement), decreasing = FALSE))) %>%
  
  select(lon, lat, agreement) %>%
  regmap(region = "ama") +
  geom_tile(aes(fill = agreement)) +
  scale_fill_manual(values = c("firebrick1", "orange"), name = "Agreement of between P-ET100 metrics")
