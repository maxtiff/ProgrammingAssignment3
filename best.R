best <- function(state, outcome) {
  ## Read outcome data and create subset by mortality outcome for heart attack, heart failure and pneumonia.
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  vect1 <- as.character(data[,2])
  vect2 <- as.character(data[,7])
  vect3 <- as.numeric(data[,11])
  vect4 <- as.numeric(data[,17])
  vect5 <- as.numeric(data[,23])
  
  ## Create dataset of all hospitals
  allHospitals <- data.frame(hospital=vect1, state=vect2, heartAttack=vect3, heartFailure=vect4, pneumonia=vect5)
  
  head(allHospitals)
  
  ## Check that state and outcome are valid

  

  ## Return hospital name in that state with lowest 30-day death
  ## rate. Any ties are handled via alphabetization.
  
  
}

