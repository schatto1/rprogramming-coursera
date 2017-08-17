complete <- function(directory, id = 1:332)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV file
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the 
    ## number of complete cases
    
    completeData <- data.frame(id=numeric(), nobs=numeric())
    
    for (i in id)
    {
        if (i < 100)
        {
            currentNumber <- formatC(i, digits = 2, flag = "0")    
        }
        else
        {
            currentNumber <- i
        }
        currentFile <- paste(directory, "/", currentNumber, ".csv", sep="")
        currentData <- read.csv(currentFile)
        completeRows <- nrow(na.omit(currentData))
        
        completeData <- rbind(completeData, data.frame(id = i, nobs = completeRows))
    }
    
    completeData
}