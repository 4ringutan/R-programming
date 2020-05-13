corr <- function(directory, threshold=0) {
        
        dat <- complete(directory)
        ids = dat[dat["nobs"] > threshold, ]$id
        corre <- numeric() ##class assignment
        for (i in ids) {
          corrdata <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                                     ".csv", sep = ""))
          corrdata <- corrdata[complete.cases(corrdata), ] ##subset complete cases
          corre <- c(corre, cor(corrdata$sulfate, corrdata$nitrate)) ##correlation between sulfate and nitrate in dat
        }
        return(corre)
}