corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    threshdat <- complete(directory,id = 1:332)
    thresh <- subset(threshdat, threshdat$nobs > threshold)
    
    data <- data.frame()
    return_cor <- vector(mode="numeric", length=0)
    for (i in thresh$id) {
        file <- paste(directory, "/", sprintf("%03d",i) ,".csv", sep='')
        data <- read.csv(file)
        sub_cor <- cor(data$sulfate, data$nitrate, use = "complete.obs")
        return_cor<-c(return_cor, sub_cor)
    }
    return_cor
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
}