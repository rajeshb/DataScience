# Question 1
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

if (!file.exists("data")) {
        dir.create("data")
}

download.file(dataUrl, "data/data.csv", method="curl")

data <- read.csv(file="data/data.csv", header=TRUE, sep=",")

cleanData <- data[!is.na(data$VAL),]

# Number of properties greater than 1,000,000 - code 24
cat(sprintf("Number of properties > 1,000,000 = %d\n", sum(cleanData$VAL >= 24)))
