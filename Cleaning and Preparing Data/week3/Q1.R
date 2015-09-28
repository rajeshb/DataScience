# Question 1
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

if (!file.exists("data")) {
        dir.create("data")
}

download.file(dataUrl, "data/data.csv", method="curl")

data <- read.csv(file="data/data.csv", header=TRUE, sep=",")

