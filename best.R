best <- function(state, outcome) {
 
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
    
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
    
  } else {
    st <- which(r[, "state"] == state) ##gives position of the specified state
    spst <- r[st, ] ##subsets the position of the specified state
    oi <- as.numeric(spst[, eval(outcome)])
    min_val <- min(oi, na.rm = TRUE)
    result  <- spst[, "hospital"][which(oi == min_val)] 
    output  <- sort(result)
  }
  return(output)
} 