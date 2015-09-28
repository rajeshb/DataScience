irequire(Hmisc)
require(plyr)

#Question #4 
data1Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
data2Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

if (!file.exists("data")) {
        dir.create("data")
}

gdpFile <- "data/GDP.csv"
eduFile <- "data/Education.csv"

download.file(data1Url, gdpFile, method="curl")
download.file(data2Url, eduFile, method="curl")

gdpData <- read.csv(gdpFile, stringsAsFactors=FALSE, header=FALSE, sep=",", na.strings=c("", "NA"), blank.lines.skip=TRUE, skip=5)
eduData <- read.csv(eduFile, stringsAsFactors=FALSE, header=TRUE, sep=",", na.strings=c("", "NA"), blank.lines.skip=TRUE)

# Clean data, remove missing values on Country code an GDP Rank columns
gdpData.clean <- gdpData[0:190,]
names(gdpData.clean) <- c("CountryCode", "GDPRank", "Col3", "Col4", "Col5", "Col6","Col7", "Col8", "Col9","Col10")

# GDPRank as numeric data type
gdpData.clean$GDPRank <- as.integer(gdpData.clean$GDPRank)

eduData.clean <- eduData[!is.na(eduData$CountryCode),]

# Merge 2 datasets based on country code
merge.data <- merge(gdpData.clean, eduData.clean, by = "CountryCode")

# Descending based on GDP Rank
merge.data.sorted <- arrange(merge.data, desc(GDPRank))

# Print Answers
print(nrow(merge.data.sorted))
print(merge.data.sorted[13, 1:4])

# Question 4
gdpSplit <- split(merge.data.sorted$GDPRank,merge.data.sorted$Income.Group)
print(sapply(gdpSplit, mean))

#Question 5
merge.data.sorted$GDPGroups <- cut2(merge.data.sorted$GDPRank, g=5)
print(table(merge.data.sorted$GDPGroups,merge.data.sorted$Income.Group))
