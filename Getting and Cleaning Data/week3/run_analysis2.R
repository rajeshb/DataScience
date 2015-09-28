library(dplyr)

#
#  Read features list, which is a list of the variables contained in the X_test.txt and 
#  X_train.txt files and read activity_labels, which are text descriptions of the activities
#  encoded by 1:6 in the data
#

features <- read.table("./UCI HAR Dataset/features.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")

#
# Process the entries in features (the variable names) to standardize appearance
#

features[, 2] <- gsub("\\(|\\)", "", features[, 2])  # Remove ()
features[, 2] <- gsub("\\-", ".", features[, 2])     # Replace dashes with periods

#
#  Read test files
#

test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
names(test_subjects) <- "subject"                    # Rename generic variable V1 as Subject

test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
names(test_activity) <- "activity"                   # Rename generic variable V1 as Activity

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                        col.names = features[, 2])

#
#  Add Subject and Activity identifiers to test data
#  Add Type variable to distinguish between test and train data
#
type <- "Test"
test_complete <- cbind(test_subjects, test_activity, type, test_data)

#
#  Read train files
#

train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(train_subjects) <- "subject"                    # Rename generic variable V1 as Subject

train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
names(train_activity) <- "activity"                   # Rename generic variable V1 as Activity

train_data <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                         col.names = features[, 2])

#
#  Add Subject and Activity identifiers to train data
#  Add Type variable to distinguish between test and train data
#
type <- "Train"
train_complete <- cbind(train_subjects, train_activity, type, train_data)

#
#  Merge test and training data into one dataset (Requirement #1)
#

completeData <- rbind(test_complete, train_complete)

#
# Change activity from code to description (Requirement #3)
#

completeData$activity <- activities[completeData$activity, 2]

#
#  Subset variables for mean and std only (Requirement #2)
#

subsetData <- select(completeData, 
                     subject, 
                     activity, 
                     matches("(mean)|(std)"))

#
#  Compute averages of the variables in the subsetted data by subject and activity
#

tidyData <- subsetData %>%
        group_by(subject, activity) %>%
        summarise_each(funs(mean))
