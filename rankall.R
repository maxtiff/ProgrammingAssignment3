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
  
  ## Build vector of all states to loop through.
  states <- unique(sort(vect2,decreasing=TRUE))
  
  ## Create data frame of all hospitals
  allHospitals <- data.frame(hospital=vect1, state=vect2, heart.attack=vect3, heart.failure=vect4, pneumonia=vect5)
  
  ## Remove any rows with NA from the data frame.
  completeHospitals <- allHospitals[complete.cases(allHospitals),]
  
  ## Vector for ranked hospital for outcome in each state.
  df <- data.frame()
  
  ## For each state, find the hospital of the given rank
  for (state in states) {
    
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
      best <- bestHospital[1,c("hospital","state")]
    } else if (num == "worst") {
      best <- bestHospital[nrow(bestHospital),c("hospital","state")]
    } else {
      best <- bestHospital[num, c("hospital","state")]
    }
    df <- rbind(best,df)
    
    
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  print(df)
}