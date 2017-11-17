run_analysis <- function () {
## download the data zip file and unzip the files
if (!file.exists("data")) {dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/dataset.zip")
unzip(zipfile="./data/dataset.zip",exdir="./data")

## read data sets
Training_set <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
Training_labels <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

Test_set <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
Test_labels <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

features <- read.table("./data/UCI HAR Dataset/features.txt")
activity_names <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

## 4. Appropriately labels the data set with descriptive variable names.
colnames(Training_set) <- features[,2]
colnames(Test_set) <- features[,2]
colnames(Training_labels) <- "activity_id"
colnames(Test_labels) <- "activity_id"
colnames(subject_train) <- "subject_id"
colnames(subject_test) <- "subject_id"
colnames(activity_names) <- c ("activity_id", "activity_names")

## 1. Merges the training and the test sets to create one data set called "merged_together".
merged_train <- cbind(Training_labels, subject_train,Training_set)
merged_test <- cbind(Test_labels, subject_test,Test_set)
merged_together <- rbind(merged_train, merged_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
i <- grep("mean", colnames(merged_together))
j <- grep("std", colnames(merged_together))
Extract_mean_std <- merged_together[, sort(c(i, j)) ]
mean_std_data <- cbind(merged_together[,1:2], Extract_mean_std)

## 3. Uses descriptive activity names to name the activities in the data set
data_set_named <- merge(activity_names,mean_std_data, by="activity_id", all.x=TRUE)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <-aggregate(. ~subject_id + activity_names, data_set_named, mean)
tidy_data <-tidy_data[order(tidy_data$subject_id, tidy_data$activity_id),]
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
  }
