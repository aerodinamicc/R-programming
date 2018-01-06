# action1
#Compute the average August temperature for all climate zones per hemisphere.
cli %>% filter (month == 8) %>%
  group_by(cls, shem= lat < 0) %>%
  summarise(mean = mean(tmp, na.rm =TRUE))

  #act2
#The de-Martonne aridity index is computed as with annual precipitation sum Psum in mm and annual temperature mean
# Tmean in degree Celsius. Compute a new data set with the de-Martonne aridity index for each grid cell.
# DMI = Psum/(Tmean + 10)
act2 <- cli %>%
  group_by(lat,lon, year) %>% # year because it's annual
  summarise(psum = sum(pre, na.rm=TRUE), tmean = mean(tmp, na.rm = TRUE)) %>%
  mutate(DMI = psum/(tmean+10))

act2 %>% arrange(abs(lat)) #abs() gives back a module out of the numbers that you pass it
#average in the initial condition would mean that we will have to use summarise somewhere later
#August leads to the assumption that we will need filter 

#mutate could be used for group standatisation

#the following would create an additional reference table, so that you can compare a value to a mean
# Use mutate() in groups (with group_by()) for group-wise operations that result in vectors of the original length,
# e.g. group-wise standardisations:
cli %>% 
  filter(cls == "Cfb", month == 6) %>% 
  group_by(lon, lat) %>% 
  mutate(stdised_pre = pre/mean(pre))

#summarise we use when we wan to collapse the data

#morski svincheta
data("ToothGrowth")

summarise(group_by(ToothGrowth, supp, dose),
          mean_len = mean(len))

ToothGrowth %>% 
  group_by(supp, dose) %>% 
  summarise(mean_len = mean(len))
#rewriing
cos(1:100) %>% abs() %>% sqrt() %>% median()

#everything is a function in R
f <- 1:5
f[5]
'['(f, 5)

#Remember: everything you do in R is a function call. So we can do:
  
ToothGrowth$len
ToothGrowth %>% .$len
ToothGrowth %>% `$`(., len)

ToothGrowth[,2]
ToothGrowth %>% .[,2]
ToothGrowth %>% `[[`(., 2)

#To understand computations in R, two slogans are helpful: Everything that exists is an object.
#Everything that happens is a function call. - John Chambers



# Env-s is where the names of the variables are stored
# In the following case <<- assigning inside the function assigns a value to a global variable
breakfast <- "icecream"

dont_change_breakfast <- function(x) {
  breakfast <- x
}
dont_change_breakfast("raisins")
breakfast

change_breakfast <- function(x) {
  breakfast <<- x
}
change_breakfast("shortbread")
breakfast

# FUNCIONS
cubic <- function(x) {x^3}
cubic(3)

#act3
#Write a small function with two arguments x and y that computes x to the power of y.
#Try it out with some values.
#Inspect what R considers body and formals of your function.
fun <- function(x,y){x^y}

#Now implementing a function in the computation of DMI
#Action 3
#Go back to the first action from today (= last action from last time). How could would a write a function to
#compute the de-Martonne aridity index? Can you use it in your pipeline?
dmi_fun <- function(sump, meant){
  sump / (meant+10)
}

cli %>%
    group_by(lat,lon, year) %>% # year because it's annual
    summarise(psum = sum(pre, na.rm=TRUE), tmean = mean(tmp, na.rm = TRUE)) %>%
    mutate(DMI = dmi(sump, meant))
    act2 %>% arrange(abs(lat))

dmi_fun <- function(x){
  
}

#CHALLENGE OF THE WEEK
# look at this code:

f <- function(x) {
  function(y) {
    x + y + z
  }
}

# try to answer the following questions by reasoning first, then by trying out:

# 1. what will
z <- 10
f(3)
# return?

# 2. what will
z <- 10
f(3)(4)
# return?

# 3. what will
z <- 11
f(3)(4)
# return?

# 4. what will
z <- 11
x <- 4
f(3)(4)
# return?

# If you are interested in learning more about functions in R, look here:
# http://adv-r.had.co.nz/Functional-programming.html

