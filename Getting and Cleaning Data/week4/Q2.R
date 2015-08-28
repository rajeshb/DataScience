# Question 2
require(data.table)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"

if (!file.exists("data")) {
        dir.create("data")
}

fileName <- "data/GDP.csv"
download.file(dataUrl, fileName, method="curl")

# Encoding is required for reading country names with unicode characters
data <- read.csv(fileName,header=TRUE, as.is=TRUE,encoding="UTF-8")

print(mean(sapply(data[5:194,5], function(x) { as.numeric(gsub(",","",x))})))

#Question 3
countryNames <- sapply(data[5:194,4], function(x) { gsub("\\\\","/",x)})
print(table(grepl("^United",countryNames)))