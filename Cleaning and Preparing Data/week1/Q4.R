#Question 4
require(XML)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

if (!file.exists("data")) {
        dir.create("data")
}

download.file(dataUrl, "data/restaurants.xml", method="curl")

doc <- xmlTreeParse("data/restaurants.xml", useInternal=TRUE)

root <- xmlRoot(doc)

zipcodes <- xpathApply(root, "//zipcode", xmlValue)

cat(sprintf("Number of Baltimore restaurants in zipcode 21231 = %d\n", length(zipcodes[zipcodes=="21231"])))
