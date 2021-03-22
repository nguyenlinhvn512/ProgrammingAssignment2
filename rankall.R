rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings =  "Not Available")
  
  ## Check that state and outcome are valid
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,c("heart attack", "heart failure", "pneumonia"))]
  
  unique_state <- order(unique(data$State))
  state <- unique(data$State)[unique_state]
  
  hospital <- c()
  ## For each state, find the hospital of the given rank
  for (st in state){
    data_state <- data[data$State == st,]
    result <- data_state[order(as.double(data_state[,colName]), data_state[,"Hospital.Name"]),]
    if (num=="best"){
      hospital <- c(hospital, result[1, "Hospital.Name"])
    }
    else if (num=="worst"){
      hospital <- c(hospital, result[nrow(result), "Hospital.Name"])
    }
    else if (num>nrow(result)) {
      hospital <- c(hospital, NA)
    } else {
      hospital <- c(hospital, result[num, "Hospital.Name"])
    }    
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df <- data.frame(hospital, state)
  df
}