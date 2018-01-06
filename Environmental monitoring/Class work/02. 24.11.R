library(tidyverse)
setwd("G:/R/EnvMOn")
cli <- read_csv("cru_kg.csv")
summary(cli)

#ACTION 1
#Compute the average July temperature for the Cfb climate.
cli %>% filter (month == 7, cls == "Cfb") %>% summarise (mean = mean(tmp))

#Compute the average July temperatures for all different climate classes.
cli %>% filter (month == 7) %>% group_by(cls) %>% summarise(mean = mean(tmp))

#filter
#How about filtering for strings?

pequod <- tibble(
  name = c("ahab", "flask", "pip", "stubb",
           "starbuck", "queequeg", "tashtego"),
  job = c("captain", "3rd mate", "ship-keeper", 
          "2nd mate", "1st mate", "harpooneer",
          "harpooneer")
)
pequod %>% filter(job == "harpooneer")
install.packages("htmlwidgets") # for display
library(htmlwidgets)

#To learn regexps, we use two functions (from stringr):
?str_view()
?str_view_all()

x <- c("apple", "banana", "pear")
str_view(x, "an")

#Match any character (except newline) with .:
str_view(x, ".a.")
str_view(x, "a..")

#Match literal "." with \. (the \ needs to be "escaped" as \\):
str_view(c("hello.", "goodbye!"), "\\.")
#1st argument is the input, 2nd arg is the search string

#ACTION 2
x <- c("wood", "plastic", "bone", "clay", "ice")
#all elements that contain an "i"
#all elements that contain an "i" that is followed by two arbitrary letters.
str_view(x, "i")
str_view(x, "i..")
str_view(x, "^c") # starts with
#ACTION 3
str_view(x, "e$") # ends with

#match alternatives with []
#exclude matches with [^]
y <- c("13 dogs", "one cat", "1 or 2 goldfish")
str_view(y, "\\d") #match any digit with \d
str_view(y, "\\d\\d") #needs two digits
str_view(y, "\\s\\d") #needs space and a digit #match any whitespace with \s
str_view(y, "[th]") # "t" or "h"

#ACTION 4
#Use your string x
#and find all matches that end with a character
#that is neither "e" nor "y".
str_view(x, "([^ey]$)")



#Alternatives: "|" alternates between one or more alternative patterns.

x <- c("abc", "deaf", "defg", "hugh")
str_view(x, "abc|d..f")
str_view(x, "(abc)|(d..f)")
str_view(x, "(^d)|([^c]$)")

#Special case for sets of characters:
#[a-f] match any of the letters a, b, c, d, e, f
#[D-G] match any of the letters D, E, F, G
#[0-9] match any single digit

x <- c("kool", "and", "the", "gang")
str_view(x, ".[a-c].")

#Repetition: specify how many times a pattern should match:
  
#  ?: 0 or 1
#+: 1 or more
#*: 0 or more

x <- c("aaaab", "aabb", "abbb", "bbbb")
str_view(x, "a?") # at least one "a" with something after
str_view(x, "a+") # one or more "a" 
str_view(x, "(aa)+") # at least 2 or more "a"
str_view(x, "a+(bb)+") # at least 1x"a" and 2x"b"

#Precise repetition: specify how many times a pattern should match:
x <- c("aaaab", "aabb", "abbb", "bbbb")
str_view(x, "a{4}") #{n}: exactly n times
str_view(x, "a{2,}") #{n,}: n times or more
str_view(x, "a{0,3}") #{n, m}: between n and m times

#Use the following character vector
x <- c("brds", "birds", "biirds", "biiirds", "birrrds")
#to find matches that correspond to
str_view(x, "i{1,1}")#"birds"
str_view(x, "i{1,3}r{1}") #"birds", "biirds", "biiirds"
str_view(x, "iii|rrr") #"biiirds", "birrrds"

#https://qntm.org/files/re/re.html more about regular expressions

#CHALLANGE OF THE WEEK
# The Koeppen-Geiger group A is defined as follows:

# Tropical climates are characterized by constant high temperatures (at sea
# level and low elevations); all 12 months of the year have average temperatures
# of 18 °C (64.4 °F) or higher.

# Can you confirm the classification in `cls` in the file "cru_kg.csv" with your
# own classification based on the above rule? Hint: the functions `unique()` and
# `all()` can help in your `dplyr` pipeline!

own_class <- cli %>% 
  group_by(lat, lon, month) %>% 
  summarise(mean_tmp = mean(tmp),  # mean per lat/lon/month group, i.e. over years
            cls = unique(cls)) %>% # propagate cls in summarised data set
  group_by(lat, lon) %>%           # group by lat/lon again to reference single grid cells
  mutate(A = all(mean_tmp > 18))   # climate zone A everywhere, where all month-wise mean temps are > 18 per grid cell

# where are we right?
matches <- own_class %>% filter(A, str_detect(cls, "^A"))
nrow(matches)
# where do we detect A wrongly?
false_positives <- own_class %>% filter(A, !str_detect(cls, "^A"))
nrow(false_positives)
# where do we fail to detect A?
false_negatives <- own_class %>% filter(!A, str_detect(cls, "^A"))
nrow(false_negatives)

# We fail to detect A quite often; this is most probably due to different data
# sets and reference periods for the classification


