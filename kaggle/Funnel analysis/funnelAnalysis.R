#Read in----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(ggpubr)

home_page <- read_csv("home_page_table.csv")
search_page <- read_csv("search_page_table.csv")
payment_page <- read_csv("payment_page_table.csv")
confirmation_page <- read_csv("payment_confirmation_table.csv")
user_table <- read_csv("user_table.csv")

f <- user_table %>%
  left_join(home_page, by = "user_id") %>%
  left_join(search_page, by = "user_id") %>%
  left_join(payment_page, by = "user_id") %>%
  left_join(confirmation_page, by = "user_id")

range(f$date)

pages <- c("home", "search", "payment", "confirmation")
colnames(f)[5:8] <- pages

#Do people come twice - retention?----
length(unique(f$user_id))

#Daily visits on the home page----
#By device
ggarrange(
  f %>%
    filter(!is.na(home)) %>%
    group_by(date, device) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = device)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Home page"),
  f %>%
    filter(!is.na(search)) %>%
    group_by(date, device) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = device)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Search page"),
  f %>%
    filter(!is.na(payment)) %>%
    group_by(date, device) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = device)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Payment page"),
  f %>%
    filter(!is.na(confirmation)) %>%
    group_by(date, device) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = device)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Confirmation page"),
  nrow = 2, ncol = 2, common.legend = TRUE)

#By gender
ggarrange(
  f %>%
    filter(!is.na(home)) %>%
    group_by(date, sex) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = sex)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Home page"),
  f %>%
    filter(!is.na(search)) %>%
    group_by(date, sex) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = sex)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Search page"),
  f %>%
    filter(!is.na(payment)) %>%
    group_by(date, sex) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = sex)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Payment page"),
  f %>%
    filter(!is.na(confirmation)) %>%
    group_by(date, sex) %>%
    summarise(customer_count = n()) %>%
    ggplot(aes(x = date, y = customer_count, fill = sex)) +
    scale_color_manual(values = c("royalblue", "orange3")) +
    geom_bar(stat = "identity", position = position_stack()) +
    ggtitle("Confirmation page"),
  nrow = 2, ncol = 2, common.legend = TRUE)
  

#Gathering data----
f <- f %>%
  mutate(home = ifelse(!is.na(home), TRUE, FALSE),
         search = ifelse(!is.na(search), TRUE, FALSE),
         payment = ifelse(!is.na(payment), TRUE, FALSE),
         confirmation = ifelse(!is.na(confirmation), TRUE, FALSE),
         year = year(date),
         month = month(date),
         day = day(date)) %>%
  gather(page, visited, pages) %>%
  mutate(page = factor(page, rev(pages)))

#Per device
page_visit_count_per_device <- f %>%
  filter(visited == TRUE) %>%
  group_by(device, month) %>%
  summarise(total_page_visit_count = n())

f %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, device) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visit_count_per_device, by = c("device", "month")) %>%
  mutate(visualDate = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/total_page_visit_count*100, 1)) %>%
  ggplot(aes(x = visualDate, y = monthly_visits, fill = page)) +
  geom_bar(stat = "identity") +
  geom_label_repel(aes(label = paste0(percentage, " %")), position = position_stack(vjust = 0.5), size = 7,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  facet_wrap(device~.) 
  
#Per gender
page_visit_count_per_gender <- f %>%
  filter(visited == TRUE) %>%
  group_by(month, sex) %>%
  summarise(total_page_visit_count = n())

f %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, sex) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visit_count_per_gender, by = c("month", "sex")) %>%
  mutate(visualDate = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/total_page_visit_count*100, 1)) %>%
  ggplot(aes(x = visualDate, y = monthly_visits, fill = page)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(percentage, " %")), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(sex~.)
