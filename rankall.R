rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome and num are valid
  states <- levels(as.factor(outcomedata$State)) ## what if csv doesn't contain all states?
  outcomes <- list(n = c("heart attack", "heart failure", "pneumonia"), c = c(11, 17, 23)) # also save column nums

  ## If an invalid outcome value is passed to best, the function should throw an error via the stop function with the exact message "invalid outcome".
  if(!(outcome %in% outcomes$n)){
    stop("invalid outcome")
  }
  
  ##The num argument can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better)
  if(num == "best"){
    num <- 1
  } else if(num == "worst"){
  } else if(class(num) != "numeric"){
    stop("invalid num")
  } else{
    num <- round(num, 0)
  }

  ## read outcome values
  colnum <- outcomes$c[outcome == outcomes$n] ## find column name for requested outcome
  outcomedata[, colnum] <- as.numeric(outcomedata[, colnum])
  statesoutcomes <- split(outcomedata, outcomedata$State)  

  myfun <- function(dataframe){

    ##If the number given by num is larger than the number of hospitals in that state, then the function should return NA.
    ##Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
    numstatehosp <- sum(!is.na(dataframe[,colnum]))
    if(num == "worst"){
      num <- numstatehosp
    } else if(num > numstatehosp){
      return(NA)
    }
  
    ## For each state, find the hospital of the given rank
    orderedstateoutcomes <- dataframe[order(dataframe[, colnum], dataframe[, 2]), c(2,colnum)]
    orderedstateoutcomes[num, 1]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  lapply(statesoutcomes, myfun)
}