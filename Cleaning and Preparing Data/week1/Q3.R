#Question 3
require(xlsx)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

if (!file.exists("data")) {
        dir.create("data")
}

download.file(dataUrl, "data/nationalGas.xlsx", method="curl")

colsInterested <- 7:23
rowsInterested <- 18:23
dat <- read.xlsx(file="data/nationalGas.xlsx", sheetIndex= 1, rowIndex=rowsInterested, colIndex=colsInterested)

cat(sprintf("Result = %d\n", sum(dat$Zip*dat$Ext,na.rm=T)))