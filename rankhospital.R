rankhospital <- function(state, outcome, num = "best") {
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
  if (!state %in% unique(data$State))
    return("Invalid State")
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  ## Return hospital name in that state with the given rank
  if (num == "best")
    return (best(state, outcome))
  if (num == "worst") {
    selectedState <- which(dataframe[ ,"state"] == state)
    dataFromState <- dataframe[selectedState, ]
    numericalOutcome <- as.numeric(dataFromState[ ,outcome])
    getDesOrder <- dataFromState[order(numericalOutcome,
                                       dataFromState[ ,"hospital"],
                                       decreasing = TRUE), ]
    return(getDesOrder[ ,"hospital"][1])
  }
  if (is.numeric(num)) {
    selectedState <- which(dataframe[ ,"state"] == state)
    dataFromState <- dataframe[selectedState, ]
    numericalOutcome <- as.numeric(dataFromState[ ,outcome])
    getDesOrder <- dataFromState[order(numericalOutcome,
                                       dataFromState[ ,"hospital"]), ]
    return(getDesOrder[ ,"hospital"][num])
  }
  return("Invalid num")
  ## 30-day death rate
}