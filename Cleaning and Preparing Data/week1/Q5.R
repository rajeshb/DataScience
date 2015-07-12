#Question 5
require(data.table)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"

if (!file.exists("data")) {
        dir.create("data")
}

fileName = "data/communites.csv"
download.file(dataUrl, fileName, method="curl")

DT = fread(fileName)