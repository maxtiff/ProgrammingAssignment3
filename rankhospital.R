rankhospital <- function (state, outcome, num = "best") {
  ## Alter case in user inputs for uniformity throughout program.
  outcome <- tolower(outcome)
  state <- toupper(state)
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome data and create subset by mortality outcome for heart attack, heart failure and pneumonia.
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Establish vectors for data frame.
  vect1 <- as.character(data[,2])
  vect2 <- as.character(data[,7])
  vect3 <- as.numeric(data[,11])
  vect4 <- as.numeric(data[,17])
  vect5 <- as.numeric(data[,23])
  
  ## Create data frame of all hospitals
  allHospitals <- data.frame(hospital=vect1, state=vect2, heart.attack=vect3, heart.failure=vect4, pneumonia=vect5)
  
  ## Remove any rows with NA from the data frame.
  completeHospitals <- allHospitals[complete.cases(allHospitals),]
  
  ## Check that user inputs for state and outcome are valid.
  if (!(state %in% completeHospitals$state)) {
    stop("Invalid state.")    
  } else if (!(outcome %in% outcomes)) {
    stop("Invalid outcome.")
  }
  
  ## Return hospital name in that state with the give rank
  ## 30-day death rate
  stateHospitals <- completeHospitals[completeHospitals$state == state,]
  
  if (outcome == "heart attack") {
    stateHospitals <- subset(stateHospitals, select = c(hospital,state,heart.attack))
    bestHospital <- stateHospitals[with(stateHospitals, order(heart.attack, hospital)), ]
  } else if (outcome == "heart failure") {
    stateHospitals <- subset(stateHospitals, select = c(hospital,state,heart.failure))
    bestHospital <- stateHospitals[with(stateHospitals, order(heart.failure, hospital)), ]
  } else {
    stateHospitals <- subset(stateHospitals, select = c(hospital,state,pneumonia))
    bestHospital <- stateHospitals[with(stateHospitals, order(pneumonia, hospital)), ]
  }
  
  if (num == "best") {
    return(as.character(bestHospital[1,1]))
  } else if (num == "worst") {
    return(as.character(bestHospital[nrow(bestHospital),1]))
  } else {
    return(as.character(bestHospital[num,1]))
  }
}