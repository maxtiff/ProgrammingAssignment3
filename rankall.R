rankall <- function(outcome, num = "best") {
  ## Alter outcome in user inputs for uniformity throughout program.
  outcome <- tolower(outcome)
  
  ## Create vector of valid outcomes to check against. 
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that outcome is valid
  if (!(outcome %in% outcomes)) {
    stop("Invalid outcome.")
  }
  
  ## Read outcome data and create subset by mortality outcome for heart attack, heart failure and pneumonia.
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Establish vectors for data frame.
  vect1 <- as.character(data[,2])
  vect2 <- as.character(data[,7])
  vect3 <- as.numeric(data[,11])
  vect4 <- as.numeric(data[,17])
  vect5 <- as.numeric(data[,23])
  
  ## Build vector of states to loop through.
  states <- unique(sort(vect2))
  
  ## Create data frame of all hospitals
  allHospitals <- data.frame(hospital=vect1, state=vect2, heart.attack=vect3, heart.failure=vect4, pneumonia=vect5)
  
  ## Remove any rows with NA from the data frame.
  completeHospitals <- allHospitals[complete.cases(allHospitals),]
  
  ## For each state, find the hospital of the given rank
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

}