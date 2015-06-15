pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    dat <- vector(mode="numeric", length=0)
    for (i in id) {
        file <- paste(directory, "/", sprintf("%03d",i) ,".csv", sep='')
        csvData <- read.csv(file, header=TRUE)
        dat = c(dat, csvData[,pollutant])
        #dat = c(dat, subset(csvData, !is.na(csvData$sulfate), c(sulfate,nitrate)))
    }

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    mean(dat, na.rm=TRUE)
}