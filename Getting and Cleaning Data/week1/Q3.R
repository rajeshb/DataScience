# Question 3
require(xlsx)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

if (!file.exists("data")) {
        dir.create("data")
}

download.file(dataUrl, "data/NGAP.xlsx", method="curl")

rowsInterested = 18:23
colsInterested = 7:15
dat <- read.xlsx("data/NGAP.xlsx", sheetIndex=1, rowIndex=rowsInterested, colIndex=colsInterested)

cat(sprintf("Result of sum(dat$Zip*dat$Ext,na.rm=T) : %d", sum(dat$Zip*dat$Ext,na.rm=T)))