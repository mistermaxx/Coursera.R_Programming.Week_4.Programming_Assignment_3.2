# |*****************************************************************************
# | Dwayne Macadangdang 8/25/2016
# | Week 4 Programming Assignment #3.3

# | Write a function called rankhospital that takes three arguments: 
# | the 2-character abbreviated name of a state (state), an outcome (outcome), 
# | and the ranking of a hospital in that state for that outcome (num). The function 
# | reads the outcome-of-care-measures.csv file and returns a character vector with 
# | the name of the hospital that has the ranking specified by the num argument.
# |*****************************************************************************


rankhospital <- function(state, outcome, num = "best")
{
  # check that state and outcome are valid
  #v alidate state vector using R's built-in state abbreviation vector
  if(!state %in% state.abb) { stop("invalid state") }
  
  # validate outcome against valid outcome vector
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) { stop("invalid outcome") }
  
  #validate incoming number: expecting numeric, "best", or "worst"
  if(!is.numeric(num) && !num %in% c("best", "worst")) {stop("invalid number")}
  
  # read data from file
  outcome.filedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  # get subset of data for selected state, getting only columns 2, 7, 11, 17, 23
  outcome.temp.data <- subset(outcome.filedata, outcome.filedata$State == state, c(2, 7, 11, 17, 23))
  
  # determine target column from outcome vector
  my_column <- as.numeric(switch(outcome, "heart attack" = 3, "heart failure" = 4, "pneumonia" = 5))
  
  #reduce the data frame to necessary columns and and filter out rows with "NA"
  outcome.subset.data <- outcome.temp.data[, c(1, my_column)]
  outcome.filter <- complete.cases(outcome.subset.data)
  outcome.data <- outcome.subset.data[outcome.filter, ]
  
  # redefine column names for readability
  names(outcome.data) <- c("name","value")
  
  # sort the data frame on the value column using the order() function
  outcome.data.sorted <- outcome.data[order(outcome.data[,2]), ]
  
  # translate non-numeric "best" and "worst" values to 1 or the number of rows in the dataframe
  if(is.numeric(num) == FALSE)
  {
    if(num == "best") {num <- 1}
    
    if(num == "worst") {num <- nrow(outcome.data.sorted)}
  }
  
  # get the value of the outcome using num as the row index
  outcome.value <- outcome.data.sorted[num,2]
  
  # set the ranked hospital(s) data frame using the outcome value
  outcome.ranked <- outcome.data.sorted[outcome.data.sorted$value == outcome.value, 1]
  
  return(sort(outcome.ranked))
}
