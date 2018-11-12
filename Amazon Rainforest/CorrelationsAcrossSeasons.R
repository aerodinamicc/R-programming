# speiTrmmGathered <- amazonWD %>%
#   select(speiTrmm03, spiTrmm03) %>%
#   filter(complete.cases(.)) %>%
#   gather(key = "source", value = "value")
# 
# den <- ggdensity(speiTrmmGathered, x = "value", color = "source", alpha = 0,
#                  linetype = "solid", palette = c("red", "blue"), size = 1) +
#   labs(x = "Anomalies", y = "Density")
# qq <- ggqqplot(speiTrmmGathered, x = "value", color = "source",
#                palette = c("red", "blue"), size = 0.2) + ylim(-4,4)
# ggarrange(den, qq, nrow = 1, ncol = 2, common.legend = TRUE)
# 
coloredSoup <- colorRampPalette(c("tomato3", "yellow", "orange","darkolivegreen1", "darkolivegreen"))(200)

wTr <- amazonWD %>%
  filter(season == "wet") %>%
  select(lon, lat, speiTrmm03, spiTrmm03) %>%
  filter(complete.cases(.)) %>%
  group_by(lon,lat)%>%
  summarise(wetSeasonCor = cor(speiTrmm03, spiTrmm03, method = "kendall")) %>%
  regmap(region = "ama") +
  geom_tile(aes(fill = wetSeasonCor)) +
  scale_fill_gradientn(name = "Kendall tau", 
                       colours = coloredSoup) +
  ggtitle("Wet season - TRMM_Princeton") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 18))

dTr <- amazonWD %>%
  filter(season == "dry") %>%
  select(lon, lat, "speiTrmm03", "spiTrmm03") %>%
  filter(complete.cases(.)) %>%
  group_by(lon,lat)%>%
  summarise(drySeasonCor = cor(speiTrmm03, spiTrmm03, method = "kendall")) %>%
  regmap(region = "ama") +
  geom_tile(aes(fill = drySeasonCor)) +
  scale_fill_gradientn(name = "Kendall tau", 
                       colours = coloredSoup) +
  ggtitle("Dry season - TRMM_Princeton") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 18))

wCru <- amazonWD %>%
  filter(season == "wet") %>%
  select(lon, lat, speiCru03, spiCru03) %>%
  filter(complete.cases(.)) %>%
  group_by(lon,lat)%>%
  summarise(wetSeasonCor = cor(speiCru03, spiCru03, method = "kendall")) %>%
  regmap(region = "ama") +
  geom_tile(aes(fill = wetSeasonCor)) +
  scale_fill_gradientn(name = "Kendall tau", 
                       colours = coloredSoup) +
  ggtitle("Wet season - CRU") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 18))

dCru <- amazonWD %>%
  filter(season == "dry") %>%
  select(lon, lat, speiCru03, spiCru03) %>%
  filter(complete.cases(.)) %>%
  group_by(lon,lat)%>%
  summarise(drySeasonCor = cor(speiCru03, spiCru03, method = "kendall")) %>%
  regmap(region = "ama") +
  geom_tile(aes(fill = drySeasonCor)) +
  scale_fill_gradientn(name = "Kendall tau", 
                       colours = coloredSoup) +
  ggtitle("Dry season - CRU") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 18))

plotlist <- list()
plotlist[[1]] <- wTr
plotlist[[2]] <- dTr
plotlist[[3]] <- wCru
plotlist[[4]] <- dCru
ggarrange(plotlist = plotlist, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")
