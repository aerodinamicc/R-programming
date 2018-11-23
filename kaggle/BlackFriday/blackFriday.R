setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(scales)
library(highcharter)
library(ggpubr)
library(circlize)

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

student_customers_count <- b_cat %>%
  group_by(User_ID, Occupation, City_Category, category) %>%
  filter(Occupation %in% c(4, 10)) %>%
  summarise(total_items_bought = n()) %>%
  group_by(City_Category, Occupation, category) %>%
  summarise(total_items_bought = sum(total_items_bought)) %>%
  ungroup() %>%
  mutate(Occupation = ifelse(Occupation == 4, "4 - supposedly University/College", "10 - supposedly School"),
         Occupation = factor(Occupation)) %>%
  arrange(Occupation, City_Category, category)

student_customers_count %>%
  ggplot(aes(x = category, y = total_items_bought, fill = Occupation)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(City_Category~.) +
  ggtitle("Total items bought by occupation 4 and 10 across categories and cities")

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
  geom_bar(stat = "identity", fill = "royalblue") +
  ggtitle("Category count taking only Product_category_1 into account"),
b_cat %>%
  group_by(category) %>%
  summarise(catCount = n()) %>%
  ggplot(aes(x = category, y = catCount))+
  geom_bar(stat = "identity", fill = "royalblue") +
  ggtitle("Category count taking all categories (1,2,3) into account"),
ncol = 2)

#Most of the customers are (78% are in the 18-45 age category)
ggarrange(
  b %>%
    group_by(Age, Gender, City_Category) %>%
    summarise(count = n(),
              percentage = round(n()/nrow(b)*100, 1)) %>%
    ggplot(aes(x = Age, y = percentage, fill = Gender)) +
    geom_bar(stat = "identity") +
    facet_wrap(City_Category~.) +
    ylab("Percentage of total purchases") +
    ggtitle("Age and gender profile of customers across cities") +
    theme(axis.text.x.bottom = element_text(angle = 90)),
  
  b %>%
    group_by(Age, Gender, City_Category) %>%
    summarise(count = n()) %>%
    spread(Gender, count) %>%
    mutate(men_women_ratio = M/F) %>%
    ungroup() %>%
    select(-M, -F) %>%
    ggplot(aes(x = Age, y = men_women_ratio)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(City_Category~.) +
    ggtitle("M/F customers ratio across cities") +
    theme(axis.text.x.bottom = element_text(angle = 90)),
  ncol = 2)

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

#NEW STUFF AFTER PUBLISHING
b_users <- b %>%
  group_by(User_ID, Age, Gender, Marital_Status, Occupation, City_Category) %>%
  summarise(av_purchase = mean(Purchase),
            sum_purchase = sum(Purchase),
            products_bought = n()) %>%
  ungroup() %>%
  mutate(Age = factor(Age),
         Occupation = factor(Occupation),
         Gender = factor(Gender),
         City_Category = factor(City_Category))
 #There are 5891 customers

#Gender

ggarrange(
  b_users %>%
    ggplot(aes(x = factor(Age), y = products_bought, fill = Gender)) +
    geom_boxplot(aes(middle = mean(products_bought))) +
    facet_wrap(City_Category~.),
  
  b_users %>%
    ggplot(aes(Age, y = av_purchase, fill = Gender)) +
    geom_boxplot(aes(middle = mean(av_purchase))) +
    facet_wrap(City_Category~.),
  
  b_users %>%
    ggplot(aes(Age, y = sum_purchase, fill = Gender)) +
    geom_boxplot(aes(middle = mean(sum_purchase))) +
    facet_wrap(City_Category~.),
  nrow = 3, common.legend = TRUE)

#Occupation
b_users_occupation <- b_users %>%
  group_by(Age, City_Category, Occupation) %>%
  summarise(av_purchase = mean(av_purchase),
            sum_purchase = sum(sum_purchase),
            products_bought = sum(products_bought))

ggarrange(
  b_users %>%
    group_by(User_ID) %>%
    filter(row_number(User_ID) == 1) %>%
    group_by(City_Category, Age, Occupation) %>%
    summarise(customers = n()) %>%
    ggplot(aes(x = Occupation, y = customers, fill = Age)) +
    scale_fill_brewer(palette = "Set1") +
    geom_bar(stat = "identity", position = position_stack()) +
    facet_wrap(City_Category~.) +
    ggtitle("Number of customers across age, cities and occupation")+
    theme(plot.title = element_text(hjust = 0.5)),
  
  b_users_occupation %>%
    ggplot(aes(x = Occupation, y = products_bought, fill = Age)) +
    geom_bar(stat = "identity", position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(City_Category~.) +
    ggtitle("Products bought across age, cities and occupation")+
    theme(plot.title = element_text(hjust = 0.5)),
  
  b_users_occupation %>%
    ggplot(aes(x = Occupation, y = av_purchase, fill = Age)) +
    geom_bar(stat = "identity", position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(City_Category~.) +
    ggtitle("Average price per bought item across age, cities and occupation")+
    theme(plot.title = element_text(hjust = 0.5)),
  
  b_users_occupation %>%
    ggplot(aes(x = Occupation, y = sum_purchase, fill = Age)) +
    geom_bar(stat = "identity", position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(City_Category~.) +
    ggtitle("Total paid by customer across age, cities and occupation")+
    theme(plot.title = element_text(hjust = 0.5)),
nrow = 4, common.legend = TRUE)

#Model taking occupation into account
predictedOccupation <- b_users_occupation %>%
  group_by(Age, Occupation, City_Category)%>%
  summarise(av_purchase = NA,
            sum_purchase = NA,
            products_bought = NA) %>%
  ungroup() %>%
  mutate(Age = factor(Age),
         Occupation = factor(Occupation),
         City_Category = factor(City_Category))

age <- b_users_occupation$Age
city_category <- b_users_occupation$City_Category
occupation <- b_users_occupation$Occupation

items_occupation_model <- lm(formula = b_users_occupation$products_bought ~ age + city_category + occupation)
predictedOccupation$products_bought <- predict(items_occupation_model, list(age = predictedOccupation$Age,
                                                                            occupation = predictedOccupation$Occupation,
                                                                            city_category = predictedOccupation$City_Category))

ggarrange(
  predictedOccupation %>%
  ggplot(aes(x = Occupation, y = products_bought, fill = Age)) +
  geom_bar(stat = "identity", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(City_Category~.),
  
  b_users_occupation %>%
    ggplot(aes(x = Occupation, y = products_bought, fill = Age)) +
    geom_bar(stat = "identity", position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(City_Category~.) +
    ggtitle("Products bought across age, cities and occupation")+
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 2, common.legend = TRUE)


