rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings =  "Not Available")
  ## Check that state and outcome are valid
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  if (!(state %in% unique(data$State))){
    stop("invalid state")
  }
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,c("heart attack", "heart failure", "pneumonia"))]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data_state <- data[data$State == state,]
  result <- data_state[order(as.double(data_state[,colName]), data_state[,"Hospital.Name"]),]
  result <- result[!is.na(result[,colName]),]
  if (num=="best"){
    return(result[1, "Hospital.Name"]) 
  }
  else if (num=="worst"){
    return(result[nrow(result), "Hospital.Name"])
  }
  else if (num>nrow(result)) {
    return(NA)
  } else {
    return(result[num, "Hospital.Name"])
  }
  
}