# Question 1
require(data.table)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

if (!file.exists("data")) {
        dir.create("data")
}

fileName <- "data/housing.csv"
download.file(dataUrl, fileName, method="curl")

data <- read.csv(fileName,header=TRUE)

agricultureLogical <- data[which(data$ACR >= 3 & data$AGS >= 5),]

print(head(agricultureLogical[,1:2], n=3))
