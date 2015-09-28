# Question 1
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

if (!file.exists("data")) {
        dir.create("data")
}

download.file(dataUrl, "data/data.csv", method="curl")

data <- read.csv(file="data/data.csv", header=TRUE, sep=",")

# Remove NAs
cleanData <- data[complete.cases(data$VAL),]

# Properties worth $1,000,000 or more
result <- cleanData[cleanData$VAL >= 24,]

cat(sprintf("Number of properties worth $1,000,000 or more : %d", nrow(result)))