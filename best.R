# best function

best <- function(state, outcome) {
  # load the data
  outcome_data <- read.csv("data/outcome-of-care-measures.csv", 
                           colClasses = "character")
  
  # check validity of state
  if (!(state %in% outcome_data$State)){
    stop("Invalid state")
  }
  
  # check validity of outcome
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes)){
    stop("Invalid outcome")
  }
  
  # select the mortality-rate column for given outcome
  outcome_col <- switch(outcome,
                        "heart attack" = 11,
                        "heart failure" = 17,
                        "pneumonia" = 23)
  
  # filter hospitals to those from the given state
  state_data <- outcome_data[outcome_data$State == state, ]
  
  # convert to numeric
  state_data[, outcome_col] <- as.numeric(state_data[, outcome_col])
  
  # remove hospitals with missing mortality data
  state_data <- state_data[!is.na(state_data[, outcome_col]), ]
  
  # handle ties by sorting
  state_data <- state_data[order(state_data[, outcome_col],
                                 state_data$Hospital.Name), ]
  
  # return the hospital w/ the lowest mortality rate given the state and outcome
  state_data$Hospital.Name[1]
}