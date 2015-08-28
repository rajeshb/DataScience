library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 

isMon <- function(x) {
        d <- as.Date(x)
        if (weekdays(d) == "Monday") {
                return(TRUE)
        }
        return(FALSE)
}

#Q5 - Part 1
print(table(grepl("2012-", sampleTimes)))

print(table(sapply(grep("2012", sampleTimes, value=TRUE), isMon)))