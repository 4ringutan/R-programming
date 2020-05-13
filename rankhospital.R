rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  r   <- as.data.frame(cbind(data[, 2],   # hospital
                             data[, 7],   # state
                             data[, 11],  # heart attack
                             data[, 17],  # heart failure
                             data[, 23]), # pneumonia ##bind the relevant columns
                       stringsAsFactors = FALSE) ##chooses strings as factors and not strings, which is why the heart failure and pneumonia was not working because they thought it was a string
  colnames(r) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") ##specify the column names
  
  if(!state %in% r[, "state"]){      
    stop('invalid state') ##if state provided is not in the column state
  
    }else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  
      } else if(is.numeric(num)){ ##if num is a number
    st <- which(r[, "state"] == state) ##gives position of the specified state
    spst <- r[st, ] ##subsets the position of the specified state
    spst[,eval(outcome)] <- as.numeric(spst[,eval(outcome)]) ##makes a column of ranking
    rankcol <- spst[order(spst[,eval(outcome)], spst[, "hospital"], decreasing = FALSE), ]
    result <- spst[, "hospital"][num]
  
    } else if (!is.numeric(num)){ ##if num is not a number
    if (num == "best") {
      st <- which(r[, "state"] == state)
      spst <- r[st, ]    
      spst[, eval(outcome)] <- as.numeric(spst[, eval(outcome)])
      spst <- spst[order(spst[, eval(outcome)], spst[, "hospital"], decreasing = FALSE), ]
      result <- spst[, "hospital"][1]
    
      } else if (num == "worst") {
      st <- which(r[, "state"] == state)
      spst <- r[st, ]    
      spst[, eval(outcome)] <- as.numeric(spst[, eval(outcome)])
      spst <- spst[order(spst[, eval(outcome)], spst[, "hospital"], decreasing = TRUE), ]
      result <- spst[, "hospital"][1]
    
      } else {
      stop('NA')
    }
  }
  return(result)
}
    