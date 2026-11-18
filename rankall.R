#rankall function

rankall <- function(outcome, num = "best") {
  # load the data
  outcome_data <- read.csv("data/outcome-of-care-measures.csv", 
                           colClasses = "character")
  
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
  
  # sort distinct states in alphabetical order
  states <- sort(unique(outcome_data$State))
  
  # create output dataframe
  result <- data.frame(hospital = character(length(states)),
                       state = states,
                       stringsAsFactors = FALSE)
  
  # determine given rank
  if (num == "best") {
    rank_num <- 1
  } else if (num == "worst") {
    rank_num <- "worst"
  } else if (is.numeric(num) && length(num) == 1 && !is.na(num)) {
    rank_num <- num
  } else {
    stop("Invalid rank")
  }
  
  # find the hospital at the given rank for each state
  for (i in seq_along(states)) {
    # select hospitals from current state
    state_data <- outcome_data[outcome_data$State == states[i], ]
    
    # convert to numeric
    state_data[, outcome_col] <- as.numeric(state_data[, outcome_col])
    
    # remove hospitals with missing mortality data
    state_data <- state_data[!is.na(state_data[, outcome_col]), ]
    
    # handle ties by sorting
    state_data <- state_data[order(state_data[, outcome_col],
                                   state_data$Hospital.Name), ]
    
    # 
    if (nrow(state_data) == 0) {  # state has no usable data
      result$hospital[i] <- NA_character_
    } else if (rank_num == "worst") {   # last hospital is the worst
      result$hospital[i] <- state_data$Hospital.Name[nrow(state_data)]
    } else if (rank_num <= nrow(state_data) && rank_num >= 1) {
      result$hospital[i] <- state_data$Hospital.Name[rank_num]
    } else {  # hospital rank out of range
      result$hospital[i] <- NA_character_
    }
  }
  result
}