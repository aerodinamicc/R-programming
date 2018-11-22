setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggrepel)
library(lubridate)
library(scales)
library(highcharter)
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
  group_by(page, device) %>%
  summarise(total_page_visit_count = n()) %>%
  arrange(page, device)

f %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, device) %>%
  summarise(monthly_visits = n()) %>%
  arrange(page, year, month, device) %>%
  left_join(page_visit_count_per_device, by = c("page", "device")) %>%
  mutate(visualDate = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/total_page_visit_count*100, 1)) %>%
  ggplot(aes(x = visualDate, y = percentage, fill = page)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(percentage, " %")), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(device~.)

f %>%
  filter(visited == TRUE) %>%
  group_by(date, page) %>%
  summarise(visits = n()) %>%
  ggplot(aes(x = date, y = visits, fill = page)) +
  geom_bar(stat = "identity")

#Per gender
page_visit_count_per_gender <- f %>%
  filter(visited == TRUE) %>%
  group_by(page, sex) %>%
  summarise(total_page_visit_count = n())

f %>%
  filter(visited == TRUE) %>%
  group_by(year, month, page, sex) %>%
  summarise(monthly_visits = n()) %>%
  left_join(page_visit_count_per_gender, by = c("page", "sex")) %>%
  mutate(visualDate = dmy(paste0("01/", month, "/", year)),
         percentage = round(monthly_visits/total_page_visit_count*100, 1)) %>%
  ggplot(aes(x = visualDate, y = percentage, fill = page)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(percentage, " %")), stat = "identity", position = position_stack(vjust = 0.5)) +
  facet_wrap(sex~.)
