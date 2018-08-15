best <- function(state, outcome) {
  
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
  
  ## read outcome values
  stateoutcomes <- outcomedata[outcomedata$State == state, ] ## subset table for requested state
  colnum <- outcomes$c[outcome == outcomes$n] ## find column name for requested outcome
  stateoutcomerates <- as.numeric(stateoutcomes[, colnum]) ## subset vector of requested outcome rate values
 
  ## Return hospital name in that state with lowest 30-day death
  ## Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
  ##If there is a tie for the best hospital for a given outcome, then the hospital names should
  ## be sorted in alphabetical order and the first hospital in that set should be chosen
  bestoutcomerate <- min(stateoutcomerates, na.rm = TRUE) ## find minimum rate for the requested subset
  bestrecords <- stateoutcomerates == bestoutcomerate & !is.na(stateoutcomerates) ## records with minimum rate
  besthospitals <- stateoutcomes[bestrecords,"Hospital.Name"] ## look up hospital names for the best records
  
  min(besthospitals) ## return first hospital name alphabetically
}