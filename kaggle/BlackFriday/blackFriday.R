setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(scales)
library(highcharter)
library(ggpubr)

b <- read_csv("BlackFriday.csv")
sapply(b, function(x) sum(is.na(x)))

# b <- b %>%
#   mutate(products = paste(ifelse(is.na(Product_Category_1), "", Product_Category_1),
#                           ifelse(is.na(Product_Category_2), "", Product_Category_2),
#                           ifelse(is.na(Product_Category_3), "", Product_Category_3), sep = " "))

# b$products <- factor(b$products)
# freq <- as.tibble(table(b$products))
# 
# freq <- freq %>%
#   select(products = Var1, count = n) %>%
#   arrange(-count)

#Occupation among ages of the customers
b %>%  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(Age, Occupation) %>%
  summarise(occupationCount = n()) %>%
  ggplot(aes(x = Occupation, y = occupationCount)) +
  geom_area(color = "lightblue") +
  facet_grid(Age~.)

#Popularity of categories among customer ages
b %>% 
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  arrange(User_ID) %>% 
  group_by(category, Age) %>%
  summarise(catCount = n()) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x = category, y = catCount))+
  geom_area(color = "lightblue") +
  facet_grid(Age~.)

b %>% filter(User_ID == "1000001")

#Mean products bought and price per User_ID
b %>%
  group_by(User_ID) %>%
  summarise(products_bought = n(), purchase = sum(Purchase)) %>%
  ungroup() %>%
  summarise(mean_products_bought = mean(products_bought, na.rm = TRUE),
            mean_products_price = mean(purchase, na.rm = TRUE))

#
