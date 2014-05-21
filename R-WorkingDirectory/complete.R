complete <- function (directory, id = 1:332) {
    # get all the file names in the directory
    fileNames <- list.files(directory)
    
    # loop through the requested files
    firstTime <- TRUE
    for (i in id) {
        if (firstTime == TRUE){
            # build the file path/name
            filePath <- paste(directory, "/", fileNames[i], sep = "")
            getFile <- read.csv(filePath)
            # now read each column in the file and check the complete cases
            fsulfate <- getFile[["sulfate"]]
            fnitrate <- getFile[["nitrate"]]
            flogic <- complete.cases(fsulfate, fnitrate)
            # nobs are the number of complete cases without NA
            nobs <- length(fsulfate[flogic])
            # id is the requested file number
            id <- i
            firstTime = FALSE
        }
        else {
            # build the file path/name
            filePath <- paste(directory, "/", fileNames[i], sep = "")
            getFile <- read.csv(filePath)
            # now read each column in the file and check the complete cases
            fsulfate <- getFile[["sulfate"]]
            fnitrate <- getFile[["nitrate"]]
            flogic <- complete.cases(fsulfate, fnitrate)
            # nobs are the number of complete cases without NA
            # now append on the first and subsequent ones
            nobs <- c(nobs, length(fsulfate[flogic]))
            id <- c(id, i)
        }
    }
    # we build the data frame from the ids and nobs vectors
    caseResult <- data.frame(id, nobs)
    # print(caseResult)
    return(caseResult)
}