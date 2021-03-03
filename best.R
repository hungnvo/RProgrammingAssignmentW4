best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  dataframe <- as.data.frame(cbind(data[ ,2],
                                  data[ ,7],
                                  data[ ,11],
                                  data[ ,17],
                                  data[ ,23]), stringsAsFactors = FALSE)
  colnames(dataframe) <- c("hospital",
                    "state",
                    "heart attack",
                    "heart failure",
                    "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% unique(data$State))
    return("Invalid State")
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("invalid Outcome")
  ## Return hospital name in that state with lowest 30-day death
  selectedState <- which(dataframe[ ,"state"] == state)
  dataFromState <- dataframe[selectedState, ]
  numericalOutcome <- as.numeric(dataFromState[ ,outcome])
  getMin <- min(numericalOutcome, na.rm = TRUE)
  res <- dataFromState[ ,"hospital"][which(numericalOutcome == getMin)]
  return(res[order(res)])
  ## rate
}