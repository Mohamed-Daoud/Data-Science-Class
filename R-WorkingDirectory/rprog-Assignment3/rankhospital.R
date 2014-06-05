rankhospital <- function(state, outcome, num = "best") {
    
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
    # check if num is valid
    if (!is.numeric(num) & !(num %in% c("best", "worst"))) {
        stop("Not applicable ranking")
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
    
    if (!is.numeric(num)) {
        if (num == "best") {
            # get the best hospital with minimum value
            minimumValue <- min(dataState[, outcomeNum], na.rm=TRUE)
            # get all hospitals having that minimum value
            for (i in 1:nrow(dataState)) {
                if (!is.na(dataState[, outcomeNum][i])) {
                    if (dataState[, outcomeNum][i] == minimumValue) {
                        hospital <- c(hospital, dataState[, hospitalNum][i])
                    }   
                }
            }
        }
        else {
            # get the worst hospital having the maximum value
            maximumValue <- max(dataState[, outcomeNum], na.rm=TRUE)
            # get all hospitals having that maximum value
            for (i in 1:nrow(dataState)) {
                if (!is.na(dataState[, outcomeNum][i])) {
                    if (dataState[, outcomeNum][i] == maximumValue) {
                        hospital <- c(hospital, dataState[, hospitalNum][i])
                    }   
                }
            }
        }
        hospital <- sort(hospital)
        return(hospital[1])
    }

    # at this point num is numeric 
    if (num > nrow(dataState)) {
        return("NA")
    }
    # generate the new ordered data frame withoug the NA values
    # order by outcomeNum first then hospital name
    newData <- dataState[order(dataState[, outcomeNum], dataState[, hospitalNum], na.last=NA) , ]
    dataLength <- nrow(newData)
    hospitalRank <- data.frame(hospitalName=character(dataLength), rate=numeric(dataLength), rank=integer(dataLength))
    hospitalRank$hospitalName <- newData[, hospitalNum]
    hospitalRank$rate <- newData[, outcomeNum]
    for (i in 1:dataLength) {
        hospitalRank$rank[i] <- i
    }
    
    return(hospitalRank$hospitalName[num])
}