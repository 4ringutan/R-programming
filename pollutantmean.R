pollutantmean<- function(directory, pollutant, id=1:332) {
                files_list <- list.files(directory, full.names=TRUE) 
                dat <- data.frame() 
                for (i in id) {
                  dat <- rbind(dat, read.csv(files_list[i])) ##reads data and binds the rows for id= x:y
                }
                 mean(dat[[pollutant]], na.rm=TRUE) ## returns mean of character subset in pollutant
                }
                

##Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows               