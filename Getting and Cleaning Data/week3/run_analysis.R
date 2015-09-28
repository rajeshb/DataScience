library(data.table)

# read the features, activity labels
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, as.is = TRUE, col.names = c("featureID", "feature"))
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, as.is = TRUE, col.names=c("activityID", "activity"))
activities$activity <- as.factor(activities$activity)

# load test data, activities and subject
test.data <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, col.names = features$feature, check.names = FALSE)
test.activity <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = c("activityID"))
test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = c("subjectID"))

# load train data, activities and subject
train.data <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, col.names = features$feature, check.names = FALSE)
train.activity <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = c("activityID"))
train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = c("subjectID"))

# keep only columns with mean() and std()
columnsToKeep <- grep(".*mean|.*Mean|.*mean\\(\\)|.*std\\(\\)", features$feature)
test.data <- test.data[ , columnsToKeep]
train.data <- train.data[ , columnsToKeep]

# include subject and activity columns in both test and train data sets
test.data$subject <- test.subject$subjectID
test.data$activity <- test.activity$activityID
train.data$subject <- train.subject$subjectID
train.data$activity <- train.activity$activityID

# merge training and test set
merged.data <- rbind(test.data, train.data)

# replace activity with descriptive activity name
merged.data$activity <- activities[merged.data$activity,2]

# generate tidy data set through write.table. Output set was grouped by subject and activity and column means were taken
tidy.data <- aggregate(. ~activity + subject, merged.data, mean)
write.table(tidy.data, file = "tidydata.txt", row.names = FALSE)
