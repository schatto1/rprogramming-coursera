best <- function(state, outcome)
{
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% outcomeData$State))
    {
        stop("invalid state")
    }
    
    if (!(outcome %in% validOutcomes))
    {
        stop("invalid outcome")
    }
    
    ## state and outcome are valid; create data subset
    ## subset will only contain columns that are related to desired outcome
    target = "Hospital.30.Day.Death..Mortality..Rates.from."
    if (outcome == "heart attack")
    {
        target <- paste(target, "Heart.Attack", sep = "")
    }
    else if (outcome == "heart failure")
    {
        target <- paste(target, "Heart.Failure", sep = "")
    }
    else if (outcome == "pneumonia")
    {
        target <- paste(target, "Pneumonia", sep = "")
    }
    subsetData <- outcomeData[c("Hospital.Name", "State", target)]
    
    ## rename column names for later use
    names(subsetData) <- c("Name", "State", "Outcome")
    
    ## Coerce columns of interest into numeric columns
    subsetData[, "Outcome"] <- as.numeric(subsetData[, "Outcome"])
    
    ## filter by state and remove incomplete entries
    subsetData <- na.omit(subsetData[subsetData$State == state, ])
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    ## order data by Outcome values, then by Name
    subsetData <- subsetData[order(subsetData$Outcome, subsetData$Name), ]
    
    ## Return the Name of the Hospital at the top
    subsetData[1,1]
    
}