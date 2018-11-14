#severitylevels SPEI - temporal evolution graph -----
severityLevelsSpeiTrmm <- amazonWD[complete.cases(amazonWD),] %>%
  dplyr::select(year, month, lon, lat, speiTrmm) %>%
  mutate(trimester = ifelse(month %in% c(1:3), 1,
                            ifelse(month %in% c(4:6), 4,
                                   ifelse(month %in% c(6:9), 7, 10)))) %>%
  group_by(year, trimester, lon, lat, speiTrmm) %>%
  mutate(date = make_date(year, trimester, 01),
         severityLevel = ifelse(speiTrmm <= -2, "Exceptionally dry",
                                ifelse(speiTrmm < -1.60, "Extremely dry",
                                       ifelse(speiTrmm < -1.30, "Severely dry",
                                              ifelse(speiTrmm < -0.8, "Moderately dry",
                                                     ifelse(speiTrmm < -0.51, "Abnormally dry", "Near normal or wet")))))) %>%
  group_by(date, severityLevel) %>%
  summarise(percentage = (n()/1945)*100/3) %>% #1945 is the number of pixel in the basin, divided by 3 months in a trimester
  filter(severityLevel != "Near normal or wet")

severityLevelsSpeiTrmm$severityLevel <- factor(severityLevelsSpeiTrmm$severityLevel,
                                               c("Exceptionally dry", "Extremely dry", "Severely dry", 
                                                 "Moderately dry", "Abnormally dry"))

severityLevelsSpeiTrmmPlot <- ggplot(severityLevelsSpeiTrmm, aes(x=date, y=percentage, fill=severityLevel)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", direction = -1, breaks=rev(levels(severityLevelsSpeiTrmm$severityLevel))) +
  ggtitle("Severity levels SPEI - TRMM")

#severitylevels SPI - temporal evolution graph -----
severityLevelsSpi <- amazonWD[complete.cases(amazonWD),] %>%
  dplyr::select(year, month, lon, lat, spiTrmm) %>%
  mutate(trimester = ifelse(month %in% c(1:3), 1,
                            ifelse(month %in% c(4:6), 4,
                                   ifelse(month %in% c(6:9), 7, 10)))) %>%
  group_by(year, trimester, lon, lat, spiTrmm) %>%
  mutate(date = make_date(year, trimester, 01),
         severityLevel = ifelse(spiTrmm <= -2, "Exceptionally dry",
                                ifelse(spiTrmm < -1.60, "Extremely dry",
                                       ifelse(spiTrmm < -1.30, "Severely dry",
                                              ifelse(spiTrmm < -0.8, "Moderately dry",
                                                     ifelse(spiTrmm < -0.51, "Abnormally dry", "Near normal or wet")))))) %>%
  group_by(date, severityLevel) %>%
  summarise(percentage = (n()/1945)*100/3) %>% #1945 is the number of pixel in the basin, divided by 3 months in a trimester
  filter(severityLevel != "Near normal or wet")

severityLevelsSpi$severityLevel <- factor(severityLevelsSpi$severityLevel,
                                       c("Exceptionally dry", "Extremely dry", "Severely dry", 
                                          "Moderately dry", "Abnormally dry"))

severityLevelsSpiPlot <- ggplot(severityLevelsSpi, aes(x=date, y=percentage, fill=severityLevel)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", direction = -1, breaks=rev(levels(severityLevelsSpi$severityLevel))) +
  ggtitle("Severity levels SPI - TRMM")

#severitylevels CWDPET100 - temporal evolution graph -----
severityLevelsCwdPet100 <- amazonWD[complete.cases(amazonWD),] %>%
  dplyr::select(year, month, lon, lat, cwd_pet100 = cwd_p_pet100_r_monthlySd) %>%
  mutate(trimester = ifelse(month %in% c(1:3), 1,
                            ifelse(month %in% c(4:6), 4,
                                   ifelse(month %in% c(6:9), 7, 10)))) %>%
  group_by(year, trimester, lon, lat, cwd_pet100) %>%
  mutate(date = make_date(year, trimester, 01),
         severityLevel = ifelse(cwd_pet100 <= -2, "Exceptionally dry",
                                ifelse(cwd_pet100 < -1.60, "Extremely dry",
                                       ifelse(cwd_pet100 < -1.30, "Severely dry",
                                              ifelse(cwd_pet100 < -0.8, "Moderately dry",
                                                     ifelse(cwd_pet100 < -0.51, "Abnormally dry", "Near normal or wet")))))) %>%
  group_by(date, severityLevel) %>%
  summarise(percentage = (n()/1945)*100/3) %>% #1945 is the number of pixel in the basin, divided by 3 months in a trimester
  filter(severityLevel != "Near normal or wet")

severityLevelsCwdPet100$severityLevel <- factor(severityLevelsCwdPet100$severityLevel,
                                          c("Exceptionally dry", "Extremely dry", "Severely dry", 
                                            "Moderately dry", "Abnormally dry"))

severityLevelsCwdPet100plot <- ggplot(severityLevelsCwdPet100, aes(x=date, y=percentage, fill=severityLevel)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", direction = -1, breaks=rev(levels(severityLevelsCwdPet100$severityLevel))) +
  ggtitle("Severity levels CWD Pet100")

#severitylevels CWDAETPET - temporal evolution graph -----
severityLevelsCwdaAETPET <- amazonWD[complete.cases(amazonWD),] %>%
  dplyr::select(year, month, lon, lat, cwd_aetPet = cwd_aet_pet_r_monthlySd) %>%
  mutate(trimester = ifelse(month %in% c(1:3), 1,
                            ifelse(month %in% c(4:6), 4,
                                   ifelse(month %in% c(6:9), 7, 10)))) %>%
  group_by(year, trimester, lon, lat, cwd_aetPet) %>%
  mutate(date = make_date(year, trimester, 01),
         severityLevel = ifelse(cwd_aetPet <= -2, "Exceptionally dry",
                                ifelse(cwd_aetPet < -1.60, "Extremely dry",
                                       ifelse(cwd_aetPet < -1.30, "Severely dry",
                                              ifelse(cwd_aetPet < -0.8, "Moderately dry",
                                                     ifelse(cwd_aetPet < -0.51, "Abnormally dry", "Near normal or wet")))))) %>%
  group_by(date, severityLevel) %>%
  summarise(percentage = (n()/1945)*100/3) %>% #1945 is the number of pixel in the basin, divided by 3 months in a trimester
  filter(severityLevel != "Near normal or wet")

severityLevelsCwdaAETPET$severityLevel <- factor(severityLevelsCwdaAETPET$severityLevel,
                                                c("Exceptionally dry", "Extremely dry", "Severely dry", 
                                                  "Moderately dry", "Abnormally dry"))

severityLevelsCwdaAetPetPlot <- ggplot(severityLevelsCwdaAETPET, aes(x=date, y=percentage, fill=severityLevel)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", direction = -1, breaks=rev(levels(severityLevelsCwdaAETPET$severityLevel))) +
  ggtitle("Severity levels CWD actual AET- actual PET")

#Plotting the severity levels plot together ----
gridExtra::grid.arrange(severityLevelsSpeiTrmmPlot, severityLevelsSpiPlot, severityLevelsCwdaAetPetPlot, severityLevelsCwdPet100plot, nrow = 2, ncol = 2)
rm("severityLevelsSpeiTrmms", "severityLevelsSpi", "severityLevelsCwdPet100", "severityLevelsCwdaAETPET",
   "everityLevelsCwdaAetPetPlot", "severityLevelsCwdPet100plot", "severityLevelsSpiPlot", "severityLevelsSpeiTrmmPlot")

#Correlations -----
indicesTimeSeries <- amazonWD[complete.cases(amazonWD),] %>%
  select(starts_with("spei"), starts_with("spi"), ends_with("monthlySd"))

indicesTimeSeries <- indicesTimeSeries %>%
  select(speiTrmm, spiTrmm,
         pet = cwd_p_pet_r_monthlySd,
         petNr = cwd_p_pet_nr_monthlySd,
         pet100 = cwd_p_pet100_r_monthlySd,
         petAvg = cwd_p_petAvg_r_monthlySd,
         petAvgNr = cwd_p_petAvg_nr_monthlySd,
         aet = cwd_p_aet_r_monthlySd,
         aetAvg = cwd_p_aetAvg_r_monthlySd,
         aetPet = cwd_aet_pet_r_monthlySd,
         speiCru)

# Nice visualization of correlations
corrplot.mixed(cor(indicesTimeSeries), order = "hclust", addrect = 3, tl.cex = 1.2, tl.pos = "d", cl.cex = 2, number.cex = 2)
corrplot(cor(indicesTimeSeries),
         method = 'color',
         type = 'lower',
         addCoef.col = "black",
         diag = FALSE,
         tl.col="black", tl.srt=90, tl.cex = 2, cl.cex = 2, number.cex = 2) #Text label color and rotation

# annotate_figure(ladder, top = text_grob("Correlation across all indices", color = "black", face = "bold", size = 14),
#                 bottom = text_grob("NB: All variables except for SpeiTrmm, SpeiCru and SpiTrmm, represent the
#                                     Cumulative Water Deficit computed with the specified input data. When Nr is
#                                   included in the name, no resetting at the end of eah year has been applied.", color = "blue", hjust = 1, x = 1, face = "italic", size = 10),
#                 left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
#                 fig.lab = "Figure x", fig.lab.face = "bold")

#Differences in seasonal correlation ----
seasonalCorrelationFun <- function(dataset){
  dataset <- dataset[complete.cases(dataset),]
  correlationsOverall <- dataset %>%
    select(starts_with("spei"), starts_with("spi"), ends_with("monthlySd"))%>%
    select(speiTrmm, spiTrmm,
           pet = cwd_p_pet_r_monthlySd,
           petNr = cwd_p_pet_nr_monthlySd,
           pet100 = cwd_p_pet100_r_monthlySd,
           petAvg = cwd_p_petAvg_r_monthlySd,
           petAvgNr = cwd_p_petAvg_nr_monthlySd,
           aet = cwd_p_aet_r_monthlySd,
           aetAvg = cwd_p_aetAvg_r_monthlySd,
           aetPet = cwd_aet_pet_r_monthlySd,
           speiCru)
  corrAll <- cor(correlationsOverall)
  
  correlationsDry <- dataset %>%
    filter(month %in% c(4:9)) %>%
    select(starts_with("spei"), starts_with("spi"), ends_with("monthlySd")) %>%
    select(speiTrmm, spiTrmm,
           pet = cwd_p_pet_r_monthlySd,
           petNr = cwd_p_pet_nr_monthlySd,
           pet100 = cwd_p_pet100_r_monthlySd,
           petAvg = cwd_p_petAvg_r_monthlySd,
           petAvgNr = cwd_p_petAvg_nr_monthlySd,
           aet = cwd_p_aet_r_monthlySd,
           aetAvg = cwd_p_aetAvg_r_monthlySd,
           aetPet = cwd_aet_pet_r_monthlySd,
           speiCru)
  corrDry <- cor(correlationsDry)
  
  correlationsWet<- dataset %>%
    filter(month %in% c(1:3, 10:12)) %>%
    select(starts_with("spei"), starts_with("spi"), ends_with("monthlySd"))%>%
    select(speiTrmm, spiTrmm,
           pet = cwd_p_pet_r_monthlySd,
           petNr = cwd_p_pet_nr_monthlySd,
           pet100 = cwd_p_pet100_r_monthlySd,
           petAvg = cwd_p_petAvg_r_monthlySd,
           petAvgNr = cwd_p_petAvg_nr_monthlySd,
           aet = cwd_p_aet_r_monthlySd,
           aetAvg = cwd_p_aetAvg_r_monthlySd,
           aetPet = cwd_aet_pet_r_monthlySd,
           speiCru)
  corrWet <- cor(correlationsWet)
  
  #wet is going to be the bottom left part, dry - the upper right one
  seasonalCorrelations <- corrAll
  i <- dim(seasonalCorrelations)[1]
  
  for (row in 1:i) {
    for (col in 1:i) {
      if (row == col) seasonalCorrelations[row, col] <- 0
      
      if(row > col){
        seasonalCorrelations[row, col] <- corrWet[row,col] - corrAll[row, col]
      }
      else if(row < col){
        seasonalCorrelations[row, col] <- corrDry[row,col] - corrAll[row, col]
      }
    }
  }
  seasonalCorrelations
  #list(seasonalCorrelations, correlationsOverall, correlationsWet, correlationsDry)
}

seasonalCorrelation <- seasonalCorrelationFun(amazonWD)
seasonalCorrelationPlot <- corrplot(seasonalCorrelation, is.corr = FALSE, bg = "gray", method = "circle",
         cl.pos = "b", tl.pos = 'd', tl.srt = 60, tl.cex = 0.9, tl.col = 'black')

annotate_figure(seasonalCorrelationPlot, top = text_grob("Seasonal correlation across all indices", color = "black", face = "bold", size = 14),
                bottom = text_grob(".", color = "blue", hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "black", rot = 90),
                right = text_grob("Figure arranged using ggpubr", color = "black", rot = 270),
                fig.lab = "Figure x", fig.lab.face = "bold")

#gleichlaufigkeit -----
source('glk.R')

gleichlaufigkeit <- glk(indicesTimeSeries)

corrplot.mixed(gleichlaufigkeit, order = "hclust", addrect = 3, tl.cex = 1.2, tl.pos = "d", cl.cex = 2, number.cex = 2)
# Obsolete ----
# correlationsWet <- amazonWD %>%
#   filter(month %in% c(4,9)) %>%
#   select(starts_with("spei"), starts_with("spi"), ends_with("monthlySd"))
# correlationsWet <- correlationsWet[complete.cases(correlationsWet),]
# ggcorr(correlationsWet, method = c("everything", "pearson"), midpoint = 0.2,
#        low = "red", mid = "yellow", high = "blue")
# 
# correlationsDry <- amazonWD %>%
#   filter(month %in% c(1:3,10:12)) %>%
#   select(starts_with("spei"), starts_with("spi"), ends_with("monthlySd"))
# correlationsDry <- correlationsDry[complete.cases(correlationsDry),]
# ggcorr(correlationsDry, method = c("everything", "pearson"), midpoint = 0.2,
#        low = "red", mid = "yellow", high = "blue")

# Distributions of all indices ----
indicesTimeSeries %>% gather(key = "index", value = "sd", 1:11) %>%
  ggplot() +
    geom_boxplot(aes(x = index,
                     y = sd,
                     colour = index)) +
  ggtitle("Distributions over different indices")

grid.arrange(indicesTimeSeries %>% gather(key = "index", value = "sd", 1:11) %>%
  ggplot(aes(sd, fill = index, color = index)) +
  geom_density(position = 'stack') +
  xlim(-3,3) +
  ylim(0,40) + #that's normally up to 40
  scale_fill_hue(l=40) +
  ggtitle("Stacked density function for all indices"),
indicesTimeSeries %>% gather(key = "index", value = "sd", 1:11) %>%
  ggplot(aes(sd, fill = index, color = index)) +
  geom_density(position = 'stack') +
  xlim(-3,3) +
  ylim(0,10) + #that's normally up to 40
  scale_fill_hue(l=40) +
  ggtitle("Ylim surpresses to 10 for better visibility"), nrow = 1, ncol = 2)
