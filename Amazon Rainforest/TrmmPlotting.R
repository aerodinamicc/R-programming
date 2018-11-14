# by year
trmm_cwd %>% 
  group_by(lon, lat, year) %>% 
  summarise(mcwd = min(wd)) %>% 
  regmap(region = "ama") +
  geom_tile(aes(fill = mcwd)) +
  facet_wrap(~ year) +
  scale_fill_gradient2(mid = "yellow", midpoint = -500)

# comparison to long-term mean (including drought years)
trmm_cwd %>% 
  group_by(lon, lat, year) %>% 
  summarise(mcwd = min(wd)) %>% 
  group_by(lon, lat) %>% 
  summarise(mcwd_ltm = mean(mcwd)) -> trmm_mcwd_ltm

trmm_cwd %>% 
  group_by(lon, lat, year) %>% 
  summarise(mcwd = min(wd)) %>% 
  left_join(trmm_mcwd_ltm) %>% 
  mutate(mcwd_diff = mcwd - mcwd_ltm) %>% 
  regmap(region = "ama") +
  geom_tile(aes(fill = mcwd_diff)) +
  scale_fill_gradient2(high = "green",low = "red", mid = "yellow", midpoint = 0) +
  facet_wrap(~ year)

rm("trmm_mcwd_ltm")

# by trimester...
trimester_vec <- rep(c("JFM", "AMJ", "JAS", "OND"), each = 3)
trmm_cwd <- trmm_cwd %>%
  mutate(trimester = trimester_vec[month])
trmm_cwd$trimester <- factor(trmm_cwd$trimester, levels=c('JFM','AMJ','JAS','OND'))

trmm_cwd %>%
  group_by(year, trimester, lon, lat) %>%
  summarise(TrimesterPrec = sum(monthly_prec)) %>%
  group_by(lon, lat, trimester) %>%
  mutate(MeanByTri = mean(TrimesterPrec)) %>%
  mutate(SdByTri = sd(TrimesterPrec)) %>%
  mutate(AnomalyByTri = (TrimesterPrec - MeanByTri)/SdByTri) %>%
  regmap(region = "ama") +
  geom_tile(aes(x=lon,
                y=lat,
                color = AnomalyByTri)) +
  scale_color_gradient2(low="red",
                        mid = "white",
                        high="blue",
                        midpoint = 0) +
  facet_grid(year ~ trimester)

#Only drought years
trmm_cwd %>%
  group_by(year, trimester, lon, lat) %>%
  filter(year == 1998 || year == 2005 || year == 2010 || year == 2015) %>%
  summarise(TrimesterPrec = sum(monthly_prec)) %>%
  group_by(lon, lat, trimester) %>%
  mutate(MeanByTri = mean(TrimesterPrec)) %>%
  mutate(SdByTri = sd(TrimesterPrec)) %>%
  mutate(AnomalyByTri = (TrimesterPrec - MeanByTri)/SdByTri) %>%
  regmap(region = "ama") +
  geom_tile(aes(color = AnomalyByTri)) +
  scale_color_gradient2(low="red", mid = "white", high="blue", midpoint = 0) +
  facet_grid(year ~ trimester)