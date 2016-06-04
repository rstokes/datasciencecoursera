best <- function(state, outcomename) {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  outcome[, 17] <- as.numeric(outcome[, 17])
  outcome[, 23] <- as.numeric(outcome[, 23])
  
  state <-  outcome[outcome$State == state,]
  
  outcomeIndex <- 0
  if(outcomename == "heart attack"){
    outcomeIndex <- 11
  }else if(outcomename =="heart failure"){
    outcomeIndex <- 17
  }else if(outcomename == "pneumonia"){
    outcomeIndex <- 23
  }
  
  minForState <- state[state[,outcomeIndex] == min(state[,outcomeIndex], na.rm=TRUE),]
  
  min(minForState$Hospital.Name, na.rm = TRUE)
}




rankhospital <- function(state, outcomename, num){
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  outcome[, 17] <- as.numeric(outcome[, 17])
  outcome[, 23] <- as.numeric(outcome[, 23])
  
  state <-  outcome[outcome$State == state,]
  
  outcomeIndex <- 0
  if(outcomename == "heart attack"){
    outcomeIndex <- 11
  }else if(outcomename =="heart failure"){
    outcomeIndex <- 17
  }else if(outcomename == "pneumonia"){
    outcomeIndex <- 23
  }
  
  os <- state[order(state[,outcomeIndex]),]
  
  s0 <- data.frame(os$Hospital.Name, os[,outcomeIndex])

  s0
  
}



