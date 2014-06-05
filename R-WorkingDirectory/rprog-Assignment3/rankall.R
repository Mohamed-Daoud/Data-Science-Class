rankall <- function(outcome, num = "best") {
    
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    outcomeNum <- 1L
    hospitalNum <- 2
    stateNum <- 7
    hospital <- character()  # holds all tied hospital with same outcome value
    chospital <- character() # holds hospitals to be assigned to hospital rank data frame
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    allStates <- unique(data[, stateNum])  #get distinct state values
    allStates <- sort(allStates)
    stateLength <- length(allStates)
    hospitalRank <- data.frame(hospital=character(stateLength), state=character(stateLength))

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
    
    # now split the data on each state
    data <- split(data, data$State) # create a list for each state

    if (!is.numeric(num)) {
        if (num == "best") {
            # get the best hospital with minimum value
            for (n in 1:stateLength) {
                tempdata <- data[[allStates[n]]]
                hospital <- character()
                minimumValue <- min(tempdata[, outcomeNum], na.rm=TRUE)
                # get all hospitals having that minimum value
                for (i in 1:nrow(tempdata)) {
                    if (!is.na(tempdata[, outcomeNum][i])) {
                        if (tempdata[, outcomeNum][i] == minimumValue) {
                            hospital <- c(hospital, tempdata[, hospitalNum][i])
                        }   
                    }
                }
                hospital <- sort(hospital)
                chospital <- c(chospital, hospital[1])
            }
        }
        else {
            
            # get the worst hospital having the maximum value
            for (n in 1:stateLength) {
                tempdata <- data[[allStates[n]]]
                hospital <- character()
                maximumValue <- max(tempdata[, outcomeNum], na.rm=TRUE)
                # get all hospitals having that maximum value
                for (i in 1:nrow(tempdata)) {
                    if (!is.na(tempdata[, outcomeNum][i])) {
                        if (tempdata[, outcomeNum][i] == maximumValue) {
                            hospital <- c(hospital, tempdata[, hospitalNum][i])
                        }   
                    }
                }
                hospital <- sort(hospital)
                chospital <- c(chospital, hospital[1])
            }
        }
        hospitalRank$hospital <- chospital
        hospitalRank$state <- allStates
        return(hospitalRank)
    }
    
    # at this point num is numeric
    # check if num is not valid and get the max number of records among all states
    largestState <- max(sapply(data, nrow)) 
    if (num > largestState) {
        return("NA")
    }
    chospital <- character()
    for (m in 1:stateLength){
        # generate the new ordered data frame withoug the NA values for each state
        # order by outcomeNum first then hospital name
        tempdata <- data[[allStates[m]]] # now we have a certain state
        newData <- tempdata[order(tempdata[, outcomeNum], tempdata[, hospitalNum], na.last=NA) , ]
        dataLength <- nrow(newData)
        temphospitalRank <- data.frame(hospitalName=character(dataLength), rate=numeric(dataLength), rank=integer(dataLength))
        temphospitalRank$hospitalName <- newData[, hospitalNum]
        temphospitalRank$rate <- newData[, outcomeNum]

        for (s in 1:dataLength) {
            temphospitalRank$rank[s] <- s
        }
        chospital <- c(chospital, temphospitalRank$hospitalName[num])
    }
    
    hospitalRank$hospital <- chospital
    hospitalRank$state <- allStates
    return(hospitalRank)

}