#rvest is new package that makes it easy to scrape (or harvest) data from html web
#pages, inspired by libraries like beautiful soup. It is designed to work with
#magrittr so that you can express complex operations as elegant pipelines composed of simple,
#easily understood pieces. Install it with:
install.packages("rvest")
library(rvest)
lego_movie <- html("http://www.imdb.com/title/tt1490017/")
#To extract the rating, we start with selectorgadget to figure out which css selector matches the data we want:
#strong span. (If you haven't heard of selectorgadget, make sure to read vignette("selectorgadget") - it's the
#easiest way to determine which selector extracts the data that you're interested #in.) We use html_node() to
#find the first node that matches that selector, extract its contents with html_text(), and convert it to numeric with as.numeric():
lego_movie %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()

#https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/