corr <- function(directory, threshold = 0)
{
    ## 'directory is a character vector of length 1 indicating'
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    storedCorr <- c()
    
    for (i in 1:332)
    {
        checkComplete <- complete(directory, i)
        ## print(checkComplete)
        
        if (checkComplete[["nobs"]] > threshold)
        {
            currentNumber <- formatC(i, digits = 2, flag = "0")
            currentFile <- paste(directory, "/", currentNumber, ".csv", sep="")
            currentData <- read.csv(currentFile)
            curatedData <- na.omit(currentData)
            calculatedCorr <- cor(curatedData[["nitrate"]], curatedData[["sulfate"]])
            
            storedCorr <- c(storedCorr, calculatedCorr)
        }
    }
    
    storedCorr
}