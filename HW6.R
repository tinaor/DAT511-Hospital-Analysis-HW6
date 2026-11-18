# Assignment 6
rm(list = ls())

# Set working directory
setwd("~/Downloads/DAT 511/HW 6")

# Part 1

## Load the data
outcome_data <- read.csv("data/outcome-of-care-measures.csv", 
                         colClasses = "character")
head(outcome_data)

## Convert column 11 to numeric 
outcome_data[,11] <- as.numeric(outcome_data[,11])

## Plot histogram 
hist(outcome_data[,11], main = "30-Day Mortality Rates for Heart Attack",
     xlab = "Mortality Rate",
     ylab = "Frequency",
     col = "blue"
     )

# Part 2
source("best.R")

## Validation Exercises
best("TX", "heart attack")    # valid input
best("ZZ", "heart attack")    # invalid state
best("TX", "wrong outcome")   # invalid outcome

# Part 3 
source("rankhospital.R")

## Validation Exercises
rankhospital("TX", "heart failure", 4)          # valid input
rankhospital("TX", "heart failure", "best")     # best hospital
rankhospital("TX", "heart failure", "worst")    # worst hospital
rankhospital("TX", "heart failure", 10000)      # exceeding num of hospitals

# Part 4
source("rankall.R")

## Validation Exercises
head(rankall("heart attack", "best"), 10)       # best hospitals 
head(rankall("heart attack", 20), 10)           # 20th ranked hospitals
tail(rankall("heart failure", "worst"), 5)      # missing data handling