best <- function(state, outcome) {
    
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    outcomeNum <- 1L
    hospitalNum <- 2
    hospital <- vector("character")
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    allStates <- unique(data[, 7])  #get distinct state values
    # check if the state provided is valid
    if (!(state %in% allStates)) {
        stop("invalid state")
    }
    # check if the outcome provided is valid
    if (!(outcome %in% validOutcome)) {
        stop("invalid outcome")
    }
    
    if (outcome == "heart attack") {
        outcomeNum <- 11
    } 
    else if (outcome == "heart failure") {
        outcomeNum <- 17
    }
    else {
        outcomeNum <- 23
    }
    # coercion of the values in that column to numeric values
    # basically any non-numeric value would become NA to handle in R
    data[, outcomeNum] <- as.numeric(data[, outcomeNum])
    
    # now focus of the state provided
    dataIntermediate <- split(data, data$State) # create a list for each state
    dataState <- dataIntermediate[[state]] # data of this particular state
    minimumValue <- min(dataState[, outcomeNum], na.rm=TRUE)

    # get all hospitals with having that minimum number
    for (i in 1:nrow(dataState)) {
        if (!is.na(dataState[, outcomeNum][i])) {
            if (dataState[, outcomeNum][i] == minimumValue) {
                hospital <- c(hospital, dataState[, hospitalNum][i])
            }   
        }
    }
    hospital <- sort(hospital)
    return(hospital[1])
}