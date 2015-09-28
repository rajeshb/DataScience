# Question 2
require(jpeg)
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"

if (!file.exists("data")) {
        dir.create("data")
}

fileName <- "data/image.jpeg"
download.file(dataUrl, fileName, method="curl")

data <- readJPEG(fileName,native=TRUE)

print(quantile(data, probs = c(0.3,0.8)))