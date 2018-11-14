amazon_et %>%
  group_by(month, lon, lat) %>%
  summarise(ET89_05 = mean(totalEt)) %>%
  group_by(lon, lat) %>%
  regmap(region = "ama") +
  geom_tile(aes(x = lon, y = lat, fill = ET89_05)) +
  scale_fill_gradient2(high = "black",low = "red", mid = "yellow", midpoint = 90) +
  ggtitle("Mean ET over the 1989-2005 period")

amazon_et %>%
  filter(year > 1997) %>%
  group_by(lon, lat) %>%
  summarise(ET98_05 = mean(totalEt)) %>%
  group_by(lon, lat) %>%
  regmap(region = "ama") +
  geom_tile(aes(x = lon, y = lat, fill = ET98_05)) +
  scale_fill_gradient2(high = "black",low = "red", mid = "yellow", midpoint = 90) +
  ggtitle("Mean ET over the 1998-2005 period")