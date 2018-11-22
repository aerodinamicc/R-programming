setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(scales)
library(highcharter)
library(ggpubr)

b <- read_csv("BlackFriday.csv")
sapply(b, function(x) sum(is.na(x)))

b_cat <- b%>% 
  gather(product_cat, category, c("Product_Category_1", "Product_Category_2", "Product_Category_3")) %>%
  select(-product_cat) %>%
  filter(!is.na(category))

#Occupation among ages of the customers----
#Based on that we can assume occupation 10 is student and 4 university student
#7 is also interesting but no clue what could it be
b %>%  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(Age, Occupation) %>%
  summarise(occupationCount = n()) %>%
  ggplot(aes(x = Occupation, y = occupationCount)) +
  geom_area(color = "lightblue") +
  facet_grid(Age~.)

#Prices----
#Mean price per product in different cities
#City C has relatively higher mean price per sold product

b %>%
  group_by(City_Category)%>%
  summarise(mean_price_per_product = mean(Purchase)) %>%
  ggplot(aes(x = City_Category, y = mean_price_per_product)) +
  geom_bar(stat="identity", fill = "lightblue")

#Products----
#Most populat products
b %>%
  group_by(Product_ID) %>%
  summarise(sold = n()) %>%
  arrange(-sold)

#Most expensive products
b %>%
  group_by(Purchase) %>%
  summarise(count = n()) %>%
  arrange(-count)

#Product category presents an interesting feature of the sold products. Each product could fall in up to 3 categories----
#Most products are in categories 1,5,8
#NB! Some product can have just one category and other up to 3, so if a product falling into 3 categories is very well bought that could distort the results
ggarrange(
b %>%
  group_by(Product_Category_1) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = Product_Category_1, y = catCount))+
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Category count taking only Product_category_1 into account"),
b_cat %>%
  group_by(category) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount))+
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Category count taking all categories (1,2,3) into account"),
ncol = 2)

#Most of the customers are (78% are in the 18-45 age category)
b %>%
  group_by(Age, Gender) %>%
  summarise(count = n(),
            percentage = round(n()/nrow(b)*100, 0)) %>%
  ggplot(aes(x = Age, y = percentage, fill = Gender)) +
  geom_bar(stat = "identity") +
  ylab("Percentage of total purchases")

b_cat %>%
  group_by(Age) %>%
  summarise(total_categoryCount_per_age_group = n()) -> categories_per_age_group

#It seems categories 1, 5 and 8 are equally popular among all ages (percentage-wise)
#2,3,4 seem to be slightly more atracktive to younger 0-25 customers

b_cat %>%
  left_join(categories_per_age_group, by = "Age") %>%
  group_by(category, Age, total_categoryCount_per_age_group) %>%
  summarise(catCount = n()) %>%
  mutate(percentage = catCount/total_categoryCount_per_age_group*100) %>%
  ggplot(aes(x = category, y = percentage))+
  geom_bar(stat = "identity") +
  ylab("Percentage of all products") +
  facet_grid(Age~.)

#Most expensive categories/Most money spent on categories----
b_cat %>%
  group_by(category) %>%
  summarise(spent_per_category = sum(Purchase)) -> spent_per_category

spent_per_category %>%
  ggplot(aes(x = category, y = spent_per_category/1000000000)) +
  geom_bar(stat = "identity") +
  ylab("Total purchases per categories (in billions, *10^9)") +
  ggtitle("Most purchased categories")

#1, 2, 6, 7 and 10 seem to have the highest spent/sales ratio
#5 and 8 and the other hand, which seem to be the most popular, have a rather low spent/sales ratio
b_cat %>%
  group_by(category) %>%
  summarise(total_sales_per_category = n()) %>%
  left_join(spent_per_category, by = "category") %>%
  mutate(spent_sales_ratio = spent_per_category/total_sales_per_category) %>%
  ggplot() +
  geom_bar(stat = "identity", aes(x = category, y = spent_sales_ratio)) +
  ggtitle("Purchase/sales ratio across categories")

#Purchases----
#purchase histogram vs city
#There are 2 more distinct regions 7-9K and 15-17K
b %>%
  select(Purchase, City_Category) %>%
  ggplot() +
  geom_histogram(aes(x = Purchase), bins = 24) +
  facet_wrap(City_Category~.)

#The line highlights 15000 purchases
plot_linetype <- list(15000, "dashed", "orange", 2)

aplot <- b_cat %>%
  filter(Purchase > 6999 & Purchase < 9000) %>%
  group_by(category) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = plot_linetype[[1]], linetype = plot_linetype[[2]],
             color = plot_linetype[[3]], size = plot_linetype[[4]]) +
  ggtitle("Sales count in the 7-9K range")

bplot <- b_cat %>%
  filter(Purchase > 14999 & Purchase < 17000) %>%
  group_by(category, Age) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = plot_linetype[[1]], linetype = plot_linetype[[2]],
             color = plot_linetype[[3]], size = plot_linetype[[4]]) +
  ggtitle("Sales count in the 15-17K range")

ggarrange(aplot, bplot, nrow = 1, ncol = 2, common.legend = TRUE)

#Average items and spendings across Age
b %>%
  group_by(User_ID, Age) %>%
  summarise(products_bought = n(), purchase = sum(Purchase)) %>%
  group_by(Age) %>%
  summarise(mean_products_bought = round(mean(products_bought, na.rm = TRUE), 0),
            mean_products_price = mean(purchase, na.rm = TRUE))

#Customers and number of sales vs cities
#There is much less number of customers in A and B than in city C
customer_count_per_city <- b %>%  
  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(City_Category, Age) %>%
  summarise(customers_count = n()) %>%
  arrange(City_Category, Age)

#Even though city A has less sales than city C, it has more sales in the 26-35 age range, whereas city C has twice as much sales in the 46-55+ age category
#That signifies that the customers in city A tend to be younger than those in city C
citySales <- b %>%
  group_by(City_Category)%>%
  summarise(sold_by_city = n(), 
            percentage_by_city = round(n()/nrow(b)*100,0))

ggarrange(
  customer_count_per_city %>%
    ggplot(aes(x = City_Category, y = customers_count, fill = Age)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle("Number of customers across cities"),
  
  #Average number of bought items across cities
  #Three very different patterns
  b %>%
    group_by(User_ID, City_Category, Age) %>%
    summarise(bought_items = n()) %>%
    group_by(City_Category, Age) %>%
    summarise(items_bought_per_city = sum(bought_items)) %>%
    arrange(City_Category, Age) %>%
    cbind(customers_count = customer_count_per_city$customers_count) %>%
    mutate(average_items_per_person_in_city = items_bought_per_city/customers_count) %>%
    ggplot(aes(x = City_Category, y = average_items_per_person_in_city, fill = Age)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle("Average number of bought items across cities"),
  b %>%
    group_by(City_Category, Age) %>%
    summarise(sales = n()) %>%
    left_join(citySales, by = "City_Category") %>%
    mutate(percentage = round(sales/sold_by_city*100, 0),
           percentage = ifelse(percentage < 3, "", as.character(paste0(percentage, "%")))) %>%
    select(-percentage_by_city) %>%
    ggplot(aes(x = City_Category, y = sales, fill = Age)) +
    geom_bar(stat="identity", position = position_dodge()) +
    ggtitle("Number of sales across age groups in the three cities"),
  b %>%
    group_by(City_Category) %>%
    summarise(sales = n(), 
              percentage_by_city = paste0(round(n()/nrow(b)*100,0), "%")) %>%
    ggplot(aes(x = City_Category, y = sales)) +
    geom_bar(stat="identity", fill = "royalblue") +
    geom_text(aes(label = percentage_by_city),
              vjust=1.6, 
              color="white",
              size=8) +
    ggtitle("Number of sales in the three cities"),
  nrow = 2, ncol = 2, common.legend = TRUE)

#Age vs stays in the city since
#Interesting that a significant amount of the customers are staying in the city for 1 year now, could be that the store is relevant for newcomers
b %>%  
  group_by(User_ID) %>%
  filter(row_number(User_ID) == 1) %>%
  group_by(Age, Stay_In_Current_City_Years, City_Category) %>%
  summarise(count = n()) %>%
  ggplot(aes(Age, count, fill = Stay_In_Current_City_Years)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(City_Category~.)

#Gender, Marital status and categories

#Age and marital status
