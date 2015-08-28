# Question 1
require(data.table)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

if (!file.exists("data")) {
        dir.create("data")
}

fileName <- "data/housing.csv"
download.file(dataUrl, fileName, method="curl")

data <- read.csv(fileName,header=TRUE)

result <- strsplit(names(data), "wgtp")

print(result[123])
