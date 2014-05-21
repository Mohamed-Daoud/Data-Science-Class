corr <- function(directory, threshold = 0) {
    # return information of each file and it's complete cases
    dirCases <- complete(directory)
    firstTime <- TRUE
    #print("now to enter the for loop")
    for (i in 1:nrow(dirCases)) {
        if (dirCases$nobs[i] > threshold) {
            if (firstTime == TRUE) {
                #this is the first iteration
                id <- dirCases$id[i]
                nobs <- dirCases$nobs[i]
                firstTime <- FALSE
            } 
            else {
                id <- c(id, dirCases$id[i])
                nobs <- c(nobs, dirCases$nobs[i])
            }
        }
    }
    # now I have a data frame having all those who exeeded the threshold
    # id holds the file number and nobs holds the number of complete cases
    if (exists("id") == TRUE){
        metThreshold <- data.frame(id, nobs)
        # for each line i would open the file and get the correlation
        firstTime <- TRUE
        for (i in 1:nrow(metThreshold)) {
            fileName <- sprintf("%03d" ,metThreshold$id[i]) # make sure to pad zeros
            filePath <- paste(directory, "/", fileName, ".csv", sep = "") 
            getFile <- read.csv(filePath)
            fsulfate <- getFile[["sulfate"]]
            fnitrate <- getFile[["nitrate"]]
            flogic <- complete.cases(fsulfate, fnitrate)
            if (firstTime == TRUE) {
                corrx <- cor(fsulfate[flogic], fnitrate[flogic])
                firstTime <- FALSE
            } else {
                tcorr <- cor(fsulfate[flogic], fnitrate[flogic])
                corrx <- c(corrx, tcorr)
            }
        }
    }
    # return numeric 0 if no monitors found
    if (exists("corrx") == TRUE) {
        return (corrx)
    } else {
        corrx <- vector("numeric", 0)
        return(corrx)
    }
    
}