rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  r   <- as.data.frame(cbind(data[, 2],   # hospital
                             data[, 7],   # state
                             data[, 11],  # heart attack
                             data[, 17],  # heart failure
                             data[, 23]), # pneumonia ##bind the relevant columns
                       stringsAsFactors = FALSE) ##chooses strings as factors and not strings, which is why the heart failure and pneumonia was not working because they thought it was a string
  colnames(r) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") ##specify the column names
 r[,eval(outcome)] <- as.numeric(r[,eval(outcome)]) ##makes a column of ranking
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
     
} else if(is.numeric(num)){ ##if num is a number
  split_state <- with(r, split(r, state))
  st <- which(r[, "state"] == state) ##gives position of the specified state
  spst <- r[st, ] ##subsets the position of the specified state
  spst[,eval(outcome)] <- as.numeric(spst[,eval(outcome)]) ##makes a column of ranking
  rankcol <- spst[order(spst[,eval(outcome)], spst[,"hospital"], decreasing = FALSE), ]
  result <- spst[, "hospital"][, "state"]
}
return(result) 
}
  