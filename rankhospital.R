rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- levels(as.factor(outcomedata$State)) ## what if csv doesn't contain all states?
  outcomes <- list(n = c("heart attack", "heart failure", "pneumonia"), c = c(11, 17, 23)) # also save column nums
  
  ## If an invalid state value is passed to best, throw an error via the stop function with the exact message "invalid state". 
  if(!(state %in% states)){
    stop("invalid state")
  }
  
  ## If an invalid outcome value is passed to best, the function should throw an error via the stop function with the exact message "invalid outcome".
  if(!(outcome %in% outcomes$n)){
    stop("invalid outcome")
  }
  
  ##The num argument can take values "best", "worst", or an integer indicating the ranking
  ##(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
  ##state, then the function should return NA. 
  
  ##Hospitals that do not have data on a particular outcome should
  ##be excluded from the set of hospitals when deciding the rankings.
  
  ## If multiple hospitals have the same 30-day mortality rate for a given cause
  ##of death, ties should be broken by using the hospital name.
  ##One can use the order function to sort multiple vectors in this
  ##manner (i.e. where one vector is used to break ties in another vector).
  
  ## Return hospital name in that state with the given rank 30-day death rate
}