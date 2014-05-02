best <- function(state, outcome) {
  ## alter case in user inputs for uniformity throughout program.
  outcome <- tolower(outcome)
  state <- toupper(state)
  outcomeVect <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome data and create subset by mortality outcome for heart attack, heart failure and pneumonia.
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  vect1 <- as.character(data[,2])
  vect2 <- as.character(data[,7])
  vect3 <- as.numeric(data[,11])
  vect4 <- as.numeric(data[,17])
  vect5 <- as.numeric(data[,23])
  
  ## Create dataset of all hospitals
  allHospitals <- data.frame(hospital=vect1, state=vect2, heartAttack=vect3, heartFailure=vect4, pneumonia=vect5)
  
  ## Remove any rows with NA from the data frame.
  completeHospitals <- allHospitals[complete.cases(allHospitals),]
  
  ## Check that user inputs for state and outcome are valid.
  if (!(state %in% completeHospitals$state)) {
    
    stop("Invalid state.")
    
  } else if (!(outcome %in% outcomeVect)) {
    
    stop("Invalid outcome.")
    
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate. Any ties are handled via alphabetization.
   stateHospitals <- completeHospitals[completeHospitals$state == state,]
   head(stateHospitals)

}

