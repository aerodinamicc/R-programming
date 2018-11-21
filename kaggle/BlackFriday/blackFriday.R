setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(scales)
library(highcharter)
library(ggpubr)

b <- read_csv("BlackFriday.csv")
sapply(b, function(x) sum(is.na(x)))

#Product category presents an interesting feature of the sold products. Each product could fall in up to 3 categories.

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

#Most populat products
b %>%
  group_by(Product_ID) %>%
  summarise(sold = n()) %>%
  arrange(-sold)

#See entries for the most expensive product
b %>%
  filter(Product_ID == "P00265242")

#Most expensive products
b %>%
  group_by(Purchase) %>%
  summarise(count = n()) %>%
  arrange(-count)
#Sales and ages----
#Popularity of categories among customer ages
b %>%
  group_by(Age, Gender) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x = Age, y = count, fill = Gender))

#Absolute number of products per category
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

b %>% 
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  arrange(User_ID) %>% 
  filter(!is.na(category)) %>%
  group_by(Age) %>%
  summarise(total_categoryCount_per_age_group = n()) -> categories_per_age_group

#It seems categories 1, 5 and 8 are equally popular among all ages (percentage-wise)
b %>%
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  arrange(User_ID) %>% 
  filter(!is.na(category)) %>%
  left_join(categories_per_age_group, by = "Age") %>%
  group_by(category, Age, total_categoryCount_per_age_group) %>%
  summarise(catCount = n()) %>%
  mutate(percentage = catCount/total_categoryCount_per_age_group*100) %>%
  arrange(-percentage) %>%
  ggplot(aes(x = category, y = percentage))+
  geom_area(fill = "lightblue") +
  ylab("Percentage of all products") +
  facet_grid(Age~.)

#Most expensive categories/Most money spent on categories----
b %>%
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  arrange(User_ID) %>% 
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(spent_per_category = sum(Purchase)) -> spent_per_category

spent_per_category %>%
  ggplot(aes(x = category, y = total_purchase/1000000000)) +
  geom_bar(stat = "identity") +
  ylab("Total purchases per categories (in billions, *10^9)")

#1, 2, 6, 7 and 10 seem to have the highest spent/sales ratio
#5 and 8 and the other hand, which seem to be the most popular, have a rather low spent/sales ratio
b %>%
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  arrange(User_ID) %>% 
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(total_sales_per_category = n()) %>%
  left_join(spent_per_category, by = "category") %>%
  mutate(spent_sales_ratio = spent_per_category/total_sales_per_category) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x = category, y = sales_spent_ratio))

#Purchases----
#The line highlights 15000 purchases
plot_linetype <- list(15000, "dashed", "orange", 2)

aplot <- b%>%
  filter(Purchase > 6999 & Purchase < 9000) %>%
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = plot_linetype[[1]], linetype = plot_linetype[[2]],
             color = plot_linetype[[3]], size = plot_linetype[[4]]) +
  ggtitle("Sales count in the 7-9K range")

bplot <- b%>%
  filter(Purchase > 14999 & Purchase < 17000) %>%
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  filter(!is.na(category)) %>%
  group_by(category, Age) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = plot_linetype[[1]], linetype = plot_linetype[[2]],
             color = plot_linetype[[3]], size = plot_linetype[[4]]) +
  ggtitle("Sales count in the 15-17K range")

ggarrange(aplot, bplot, nrow = 1, ncol = 2, common.legend = TRUE)

#purchase histogram vs city
b %>%
  select(Purchase, City_Category) %>%
  ggplot() +
  geom_histogram(aes(x = Purchase), bins = 24) +
  facet_wrap(City_Category~.)

#Average spendings per Age
customers_count_per_age <- b %>%
  group_by(User_ID, Age) %>%
  summarise(customers_count_per_age = n())

b %>%
  group_by(Age) %>%
  left_join(customers_count_per_age, by = "Age") %>%
  summarise(number_of_items_bought = n(), average_purchase = mean(Purchase))


#Mean products bought and price per User_ID----
e <- b %>%
  group_by(User_ID, Age) %>%
  summarise(products_bought = n(), purchase = sum(Purchase)) %>%
  group_by(Age) %>%
  summarise(mean_products_bought = mean(products_bought, na.rm = TRUE),
            mean_products_price = mean(purchase, na.rm = TRUE))

#Age vs stays in the city since
b %>%  
  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(Age, Stay_In_Current_City_Years) %>%
  summarise(count = n()) %>%
  ggplot(aes(Age, Stay_In_Current_City_Years)) +
  geom_point(aes(size = count))
