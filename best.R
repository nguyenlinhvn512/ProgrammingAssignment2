best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  ## Read outcome data
  # if (!(is.element(outcome, c("heart attack", "heart failure", "pneumonia")))){
  #   stop("invalid outcome")
  # }
  if (!(outcome) %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  ## Check that state and outcome are valid
  # if (!(is.element(state,unique(oc$State)))){
  #   stop("invalid state")
  # }
  if (!(state %in% unique(data$State))){
    stop("invalid state")
  }
  
  # n <-0
  # if (outcome == "heart attack") {n<-11}
  # else if (outcome == "heart failure") {n<-17}
  # else if (outcome == "pneumonia") {n<-23}
  # 
  # # num_oc <- sapply(oc[n], as.numeric)
  # # valid_oc <- num_oc[!is.na(num_oc)]
  # # oc[valid_oc,]
  # 
  # ## Return hospital name in that state with lowest 30-day death
  # state_data <- data[data$State==state,]
  # result <- state_data[order(state_data[n],state_data$Hospital.Name),]
  # head(result)
  # #result[1,"Hospital.Name"]
  # ## rate

  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,c("heart attack", "heart failure", "pneumonia"))]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]
  idx <- which.min(as.double(data.state[,colName]))
  data.state[idx,"Hospital.Name"]

}
