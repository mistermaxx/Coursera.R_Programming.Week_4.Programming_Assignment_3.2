# |*****************************************************************************
# | Dwayne Macadangdang 8/25/2016
# | Week 4 Programming Assignment #3.4

# | Write a function called rankall that takes two arguments: an outcome name (outcome) 
# | and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file 
# | and returns a 2-column data frame containing the hospital in each state that has the 
# | ranking specified in num. For example the function call rankall("heart attack", "best") 
# | would return a data frame containing the names of the hospitals that are the best in their 
# | respective states for 30-day heart attack death rates. The function should return a value 
# | for every state (some may be NA). The first column in the data frame is named hospital, 
# | which contains the hospital name, and the second column is named state, which contains the 2-character abbreviation for the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings. 
# |*****************************************************************************

# | directory:
# | setwd("/Users/mistermaxx/Documents/work/personal/Coursera/R_Programming/Week_4/rprog-data-ProgAssignment3-data")

rankall <- function(outcome, num = "best")
  {
    # create and initialize all variables
    outcome.data.final <- NULL

    # create and initialize the state vector
    state_vector <- as.vector(state.abb)

    #Function returns the hospital name for the given state at the specified rank.
    get.state.hospital.data <- function(target.state) 
      {
        # subset the larger data frame by the target state  
        outcome.data.state <- subset(outcome.data.sorted, outcome.data.sorted$state == target.state, c(1:3))
        
        # order the data frame by the value column
        outcome.data.state <- outcome.data.state[order(outcome.data.state[, 3]), ]
        
        # translate non-numeric "best" and "worst" values to 1 or the number of rows in the dataframe
        if(is.numeric(num) == FALSE)
        {
          if(num == "best") {num <- 1}
          
          if(num == "worst") {num <- nrow(outcome.data.sorted)}
        }
        
        # return the row corresponding to the rank, with the hospital name and state columns only
        outcome.data.state <- data.frame(outcome.data.state[num, c(1:2)])
        
        return(outcome.data.state)
      }
    
    # validate outcome against valid outcome vector
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) { stop("invalid outcome") }
    
    #validate incoming number: expecting numeric, "best", or "worst"
    if(!is.numeric(num) && !num %in% c("best", "worst")) {stop("invalid number")}
    
    # read data from file
    outcome.filedata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    # get subset of data for selected state, getting only columns 2, 7, 11, 17, 23
    outcome.temp.data <- outcome.filedata[, c(2, 7, 11, 17, 23)]
    
    # determine target column from outcome vector
    my_column <- as.numeric(switch(outcome, "heart attack" = 3, "heart failure" = 4, "pneumonia" = 5))
    
    #reduce the data frame to necessary columns and and filter out rows with "NA"
    outcome.subset.data <- outcome.temp.data[, c(1, 2, my_column)]
    outcome.filter <- complete.cases(outcome.subset.data)
    outcome.data <- outcome.subset.data[outcome.filter, ]
    
    names(outcome.data) <- c("name","state","value")
    
    outcome.data.sorted <- outcome.data[order(outcome.data[,2]), ]
                                     
    # call the get.state.hospital.data() function
    outcome.data.final <- lapply(state_vector, get.state.hospital.data) 
    
    # a list of data frames is returned; reduce down to a simple data frame with do.call and rbind()
    outcome.data.return <- do.call("rbind", outcome.data.final)
    
    # get rid of the row names
    row.names(outcome.data.return) <- NULL
    
    return(outcome.data.return)
  }