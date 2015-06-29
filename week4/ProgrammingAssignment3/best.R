best <- function(state, outcome) {

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        if (!(state %in% data[,7])) {
                stop("invalid state")
        }
        
        ## Default is heart attack
        selectedColumn <- 11
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if (outcome == "heart failure") {
                selectedColumn <- 17
        }
        else if (outcome == "pneumonia") {
                selectedColumn <- 23
        }
        else {
                selectedColumn <- 11
        }
        
        data[, selectedColumn] <- as.numeric(data[, selectedColumn])
        data <- data[complete.cases(data),]
        stateData <-subset(data[,c(2,7,selectedColumn)], State==state)
        stateData[order(stateData[,3], stateData[,1]),][1,1]
}
