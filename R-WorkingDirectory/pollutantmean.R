pollutantmean <- function (directory, pollutant, id = 1:332) {
    # change the ids to correspond to file names (i.e padded with zeros)
    # the result would be a character vector with the leasing zeros
    fileName <- sprintf("%03d", id) 
    
    # build a character vector having all requested files and their paths
    filePath <- paste(directory, "/", fileName, ".csv", sep = "")
    
    # build the data frame from the file numeric vector from files
    firstTime <- TRUE
    for (i in seq_along(filePath)) {
        # read whole file first which builds a data frame
        getFile <- read.csv(filePath[i])
        if (firstTime == TRUE) {
            # get the required PM(particle matter)
            # this creates a numeric vector having all the vaules including NAs
            pMatter <- getFile[[pollutant]]
            firstTime <- FALSE
        }
        else {
            pMatter <- c(pMatter, getFile[[pollutant]])
        }
        
    }
    # Now we have a numeric vector having all values from all files
    # get the mean for it now excluding the NA ones
    print(mean(pMatter, na.rm = TRUE))
}
