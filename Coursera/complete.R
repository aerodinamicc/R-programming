complete <- function(directory, id){
  setwd(directory)
  files <- c(dir())
  count <- numeric()
  place <- 1
  for (i in id){
  file <- read.csv(files[i], header = TRUE)
    cc <- sum(complete.cases(file))
    count[place] <- cc
    place <- place + 1
    }
  df <- data.frame(id, count)
  colnames(df) <- c("id", "nods")
  print(df)
  }

  complete("D:/specdata", 1:5)