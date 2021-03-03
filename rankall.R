rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  dataframe <- as.data.frame(cbind(data[ ,2],
                                   data[ ,7],
                                   data[ ,11],
                                   data[ ,17],
                                   data[ ,23]),
                             stringsAsFactors = FALSE)
  colnames(dataframe) <- c("hospital",
                           "state",
                           "heart attack",
                           "heart failure",
                           "pneumonia")
  ## Check that state and outcome are valid
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  ## For each state, find the hospital of the given rank
  if (outcome == "heart attack") {
    dataframe <- dataframe[ ,c(1,2,3)]
  } else if (outcome == "heart failure") {
    dataframe <- dataframe[ ,c(1,2,4)]
  } else if (outcome == "pneumonia") {
    dataframe <- dataframe[ ,c(1,2,5)]
  }
  
  colnames(dataframe)[3] <- "Mortality"
  
  dataframe <- dataframe[!is.na(dataframe$Mortality),]
  splittedDF <- split(dataframe, dataframe$state)
  processList <- lapply(splittedDF, function(oi, num){
    oi <- oi[order(oi$Mortality, oi$hospital), ]
    if (num == "best") {
      return (oi$hospital[1])
    } else if (num == "worst") {
      return (oi$hospital[nrow(oi)])
    } else {
      return (oi$hospital[num])
    }
  }, num)
  
  res <- data.frame(hospital=unlist(processList), state=names(processList))
  ## Return a data frame with the hospital names and the
  return (res)
  ## (abbreviated) state name
}
