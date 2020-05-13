complete<- function(directory, id=1:332) {
            
           files_list <- list.files(directory, full.names=TRUE) ##lists files
           dat <- data.frame() ##assigns class to dat
           nobs <- numeric() ##assigns class to nobs
           for (i in id) {
             dat <- read.csv(files_list[i]) ##reads data
             nobs <- c(nobs, sum(complete.cases(dat))) ##creates vector "nobs" with sum of values excluding NAs
            
           }
          
           return(data.frame(id, nobs))
           
          
}
