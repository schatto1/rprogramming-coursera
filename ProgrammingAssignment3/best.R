best <- function(state, outcome)
{
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    ## stateNames <- unique(outcome["State"]) ## contains distinct state names
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
    
    ## Coerce columns of interest into numeric columns
    subsetData[, target] <- as.numeric(subsetData[, target])
    
    ## filter by state and remove incomplete entries
    subsetData <- na.omit(subsetData[subsetData$State == state, ])
    
    print(head(subsetData))
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    targetOutcome <- paste("subsetData", target, sep = "$")
    print(targetOutcome)
    
    ## Sort by death rate, and then hospital name
    subsetData <- subsetData[order(target), ]
    
    subsetData[1,1]
    
}