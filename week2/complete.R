complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    result <- data.frame(id=integer(), nobs=integer(),stringsAsFactors=FALSE)
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    x = list(id=vector("numeric", length=0), nobs=vector("numeric", length=0))
    for (i in id) {
        file <- paste(directory, "/", sprintf("%03d",i) ,".csv", sep='')
        csvData <- read.csv(file, header=TRUE)
        
        data <- data.frame(id=i, nobs=nrow(csvData[complete.cases(csvData),]))
        result <- rbind(result, data)
    }
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    result
}
