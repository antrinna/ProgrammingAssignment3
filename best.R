best <- function(state, outcome) {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## If an invalid state value is passed to best, the  function should throw an error via the stop function with the exact message "invalid state". If an invalid
  ## outcome value is passed to best, the function should throw an error via the stop function with the exact message "invalid outcome".
  
  ## The hospital name is the name provided in the Hospital.Name variable. The outcomes can
  ## be one of "heart attack", "heart failure", or "pneumonia".
  
  ## sample reading outcome values
  ## outcomedata[, 11] <- as.numeric(outcomedata[, 11])
  
  ## Return hospital name in that state with lowest 30-day death
  ## Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
  ##  If there is a tie for the best hospital for a given outcome, then the hospital names should
  ## be sorted in alphabetical order and the first hospital in that set should be chosen
  ## rate
}