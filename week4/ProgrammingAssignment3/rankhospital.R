rankhospital <- function(state, outcome, num="best") {
        
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
        stateData <- subset(data[,c(2,7,selectedColumn)], State==state)
        
        selectIndex <- 0
        output <- NA
        
        if (num == "best") {
                selectIndex <- 1
        }
        else if (num == "worst") {
                selectIndex <- nrow(stateData)
        }
        else {
                if (num > 0 | num <= nrow(stateData)) {
                        selectIndex <- num
                }
        }
        if (selectIndex > 0) {
                output <- stateData[order(stateData[,3], stateData[,1]),][selectIndex,1]
        }
        output
}