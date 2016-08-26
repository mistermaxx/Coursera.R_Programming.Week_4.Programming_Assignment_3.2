# |*****************************************************************************
# | Dwayne Macadangdang 8/24/2016
# | Week 4 Programming Assignment #3.2

# | Write a function called best that take two arguments: the 2-character abbreviated name 
# | of a state and an outcome name. The function reads the outcome-of-care-measures.csv file 
# | and returns a character vector with the name of the hospital that has the best (i.e. lowest) 
# | 30-day mortality for the specified outcome in that state. The hospital name is the name 
# | provided in the Hospital.Name variable. The outcomes can be one of “heart attack”,
# | heart failure”, or “pneumonia”. Hospitals that do not have data on a particular outcome
# | should be excluded from the set of hospitals when deciding the rankings.

# | The function should check the validity of its arguments. If an invalid state value is 
# | passed to best, the function should throw an error via the stop function with the exact 
# | message “invalid state”. If an invalid outcome value is passed to best, the function
# | should throw an error via the stop function with the exact message “invalid outcome”.
# |*****************************************************************************

# | directory:
# | setwd("/Users/mistermaxx/Documents/work/personal/Coursera/R_Programming/Week_4/rprog-data-ProgAssignment3-data")

best <- function(state, outcome)
{
  #check that state and outcome are valid
  #validate state vector using grep() and R's built-in state abbreviation vector
  if(length(grep(state,state.abb)) == 0) 
  {
    stop("invalid state")
  }

  #validate outcome vector
  if(length(grep(outcome, c("heart attack", "heart failure", "pneumonia"))) == 0)
  {
    stop("invalid outcome")
  }
  
  # read data from file
  outcome.filedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  outcome.temp.data <- subset(outcome.filedata, outcome.filedata$State == state, c(2, 7, 11, 17, 23))
  
  my_column <- as.numeric(switch(outcome, "heart attack" = 3, "heart failure" = 4, "pneumonia" = 5))
  
  # subset, reduce, and filter the data down to only the required columns (2, 11, 17, 23)
  outcome.data <- subset(outcome.temp.data, outcome.temp.data$State == state, c(1, my_column))
  outcome.filter <- complete.cases(outcome.data)
  outcome.data <- outcome.data[outcome.filter, ]
  
  # redefine column names
  names(outcome.data) <- c("name","value")
  
  #get the hospital row
  hospital_row <- subset(outcome.data, outcome.data$value == min(outcome.data$value))
  
  #get the hospital name(s), sorted
  hospital_name <- sort(hospital_row[, 1])
  
  return(hospital_name)
}

# test cases
# > best("HI", "pneumonia")
# [1] "PALI MOMI MEDICAL CENTER"
# > best("CT", "heart attack")
# [1] "WATERBURY HOSPITAL"
# > best("ZZ", "heart attack")
# Error in best("ZZ", "heart attack") : invalid state
# > best("NY", "blah")
# Error in best("NY", "blah") : invalid outcome