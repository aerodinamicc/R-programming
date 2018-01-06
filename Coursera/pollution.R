big <- numeric()
pollutantmean <- function(directory, pollutant, id){
  setwd(directory)
  files <- c(dir())
  for (i in id){
  name <- files[i]
  file <- read.csv(name, header = TRUE)
  z <- file[,pollutant]
  vec <- z[which(!is.na(z) == TRUE)]
  big <-c(big, vec)
  }
  print(mean(big))
}

pollutantmean("D:/specdata", "sulfate", 100:200)
