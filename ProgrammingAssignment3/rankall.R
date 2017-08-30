rankall <- function(outcome, num = "best")
{
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
   
    if (!(outcome %in% validOutcomes))
    {
        stop("invalid outcome")
    }
    if (!(class(num) == "numeric" | num == "best" | num == "worst"))
    {
        stop("invalid ranking")
    }
    
    ## outcome and rank are valid; create data subset and list of unique state names
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
    
    ## create list of unique states
    states <- unique(subsetData$State)
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}