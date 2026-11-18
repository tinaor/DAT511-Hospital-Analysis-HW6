# rankhospital function

rankhospital <- function(state, outcome, num = "best") {
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
  
  # determine given rank
  if (num == "best") {
    rank_num <- 1
  } else if (num == "worst") {
    rank_num <- nrow(state_data)
  } else if (is.numeric(num) && length(num) == 1 && !is.na(num)) {
    rank_num <- num
  } else {
    stop("Invalid rank")
  }
  
  # return NA if the given rank is greater than the number of hospitals
  if (rank_num < 1 || rank_num > nrow(state_data)) {
    return(NA_character_)
  }
  
  # return the name of the hospital at the given rank
  state_data$Hospital.Name[rank_num]
}
