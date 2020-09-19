#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
library(dplyr)
library(data.table)
#   read training data
trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainingValues <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingActivity <- read.table("UCI HAR Dataset/train/y_train.txt")
#   read test data
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("UCI HAR Dataset/test/y_test.txt")
#   read features, don't convert text labels to factors
features <- read.table("UCI HAR Dataset/features.txt", as.is=T)
#   read activity labels
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("activityId", "activityLabel")
##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################
#   concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)
#   assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")
##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################
#   determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
#   ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]
##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################
#   replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])
##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################
#   expand abbreviations and clean up names and correct typo
names(humanActivity)<-gsub("Acc", "Accelerometer", names(humanActivity))
names(humanActivity)<-gsub("Gyro", "Gyroscope", names(humanActivity))
names(humanActivity)<-gsub("BodyBody", "Body", names(humanActivity))
names(humanActivity)<-gsub("Mag", "Magnitude", names(humanActivity))
names(humanActivity)<-gsub("^t", "Time", names(humanActivity))
names(humanActivity)<-gsub("^f", "Frequency", names(humanActivity))
names(humanActivity)<-gsub("tBody", "TimeBody", names(humanActivity))
names(humanActivity)<-gsub("-mean()", "Mean", names(humanActivity), ignore.case = TRUE)
names(humanActivity)<-gsub("-std()", "StandardDeviation", names(humanActivity), ignore.case = TRUE)
names(humanActivity)<-gsub("-freq()", "Frequency", names(humanActivity), ignore.case = TRUE)
##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################
#   group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))
# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
