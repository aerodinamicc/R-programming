complete <- function(directory, threshold = 0){
  setwd(directory)
  files <- c(dir())
  count <- numeric()
  for (i in 1:length(files)){
    file <- read.csv(files[i], header = TRUE)
    sumcc <- sum(complete.cases(file))
      if (sumcc > threshold){
    listcc <- complete.cases(file)
    df <- subset(file, listcc == TRUE)
    sulfate <- df$sulfate
    nitrate <- df$nitrate
    corr <- cor(df$sulfate, df$nitrate)
    count <- c(count, corr)
          } else {}
      }
    print(count)
  }


  
  complete("D:/specdata", 150)
      
    