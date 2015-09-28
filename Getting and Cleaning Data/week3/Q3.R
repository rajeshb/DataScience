# Question 3
require(data.table)
require(plyr)

data1Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
data2Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

if (!file.exists("data")) {
        dir.create("data")
}

gdpFile <- "data/GDP.csv"
eduFile <- "data/Education.csv"

download.file(data1Url, gdpFile, method="curl")
download.file(data2Url, eduFile, method="curl")

gdpData <- read.csv(gdpFile, header=FALSE, sep=",", col.names=c("CountryCode", "GDPRank", "Col3", "Col4", "Col5", "Col6","Col7", "Col8", "Col9","Col10"), na.strings=c("", "NA"), blank.lines.skip=TRUE, skip=5)
eduData <- read.csv(eduFile, header=TRUE, sep=",", na.strings=c("", "NA"), blank.lines.skip=TRUE)

# Clean data, remove missing values on Country code an GDP Rank columns
gdpData.clean <- gdpData[!is.na(gdpData$CountryCode) & !is.na(gdpData$GDPRank),]
eduData.clean <- eduData[!is.na(eduData$CountryCode),]

# Transform GDPRank as numeric data type
gdpData.clean.trans <- transform(gdpData.clean, GDPRank = as.numeric(GDPRank))

# Merge 2 datasets based on country code
merge.data <- merge(gdpData.clean.trans, eduData.clean, by = "CountryCode")

# Descending based on GDP Rank
merge.data.sorted <- arrange(merge.data, desc(GDPRank))

# Print Answers
print(nrow(merge.data.sorted))
print(gdpData.clean.trans[13,1:2])

# 