library(tidyverse)
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
glb <- read_csv("GLB.Ts+dSST.csv", skip = 1,
                na = "***")
ggplot(glb) +
  geom_line(aes(x = Year,
                y = `J-D`))

ggplot(glb, aes(x = Year,
                   y = `J-D`)) +
  geom_line() +
  geom_point(aes(colour = `J-D` > 0.5))

glb %>% gather(aggregation, temp, -Year) %>% 
  filter(aggregation %in% c("J-D", "JJA")) %>% 
  ggplot(aes(x = Year,
             y = temp,
             colour = aggregation)) +
  geom_line()

#ACTION 1
#Use the NASA GISTEMP global mean temperature anomalies (file GLB.Ts+dSST.csv) and
#make a x-y (scatter) plot of the yearly anomaly differences between spring (MAM) and autumn (SON),
#and colour the points according to the annual mean anomalies (J-D). Then add a line for the same difference.

#Action 2
#Go back to your previous plot. This time, we want the points to be generally red and
#only the line to reflect the annual mean anomaly.

#Additional ggplot functions
scale_color_manual(values = c("red", "blue", "black"))
scale_y_reverse()
scale_y_log10() #highlights changes in the small fractions, works with numeric
scale_colour_gradient2(low = "red", mid = "grey", high = "blue", midpoint = 0)
#with points and colours these are:
scale_colour_manual(name = "Iris spec.", values = c("pink", "orange", "blue"))
scale_size_continuous(breaks = c(1, 3, 5),labels = c("super small", "not so small","large"))
#iris dataset
g <- ggplot(iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 colour = Species,
                 size = Petal.Length))
g
g + scale_colour_manual(name = "Iris spec.",
                        values = c("pink", "orange", "blue"))
g + scale_size_continuous(breaks = c(1, 3, 5),
                          labels = c("super small",
                                     "not so small",
                                     "large"))
#Reflecting positive/negative values in a continiuos scale:
ggplot(glb, aes(x = Year,
                y = MAM - SON,
                colour = `J-D`)) +
  geom_line() +
  scale_colour_gradient2(low = "red",
                         mid = "grey",
                         high = "blue",
                         midpoint = 0)

#tree dataset
trees
ggplot(trees) + geom_bar(aes(Height)) #it represents the count of something
ggplot(iris) + geom_bar(aes(Species)) #50 counts per species
ggplot(trees) + geom_boxplot(aes(x = Volume, y = Height))
ggplot(glb) + geom_histogram(aes(x = Jan), binwidth = 0.1)

iris%>% gather(feature, measurement, -Species) +
  ggplot() +

  #Boxplot
  glb %>% 
  gather(aggregation, temp, DJF:SON) %>% 
  ggplot() +
  geom_density(aes(x = temp,
                   colour = aggregation))

#Action 3
  lu <- 1:12
names(lu) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "July", "Aug", "Sep", "Oct", "Nov", "Dec")
#didnt use it at the end, it was to create a reference for the months proper order in the graph
#the result that I got didnt have the right order of the months
  glb %>% 
  gather(month, value, Jan:Dec) %>% 
   ggplot() +
  geom_boxplot(aes(x = month,
                   y = value,
                   colour = month))  
