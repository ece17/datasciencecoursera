
library(dplyr)
setwd("C:/Users/tshealy - admin/Desktop/datasciencecoursera")

#Assigning data frames and column names to train and test data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityID", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activityID")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activityID")

#1.Merges the training and the test sets to create one data set
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
sub <- rbind(subject_train, subject_test)
merge <- cbind(sub, y, x)

#2.Extracts only the measurements on the mean and standard deviation for each measurement
tidy <- merge %>% select(subject, activityID, contains("mean"), contains("std"))

#3.Uses descriptive activity names to name the activities in the data set
tidy$activityID <- activities[tidy$activityID, 2]

#4.Appropriately labels the data set with descriptive variable names.
names(tidy)[2] = "activity"
names(tidy)<-gsub("Acc", "Accelerometer", names(tidy))
names(tidy)<-gsub("Gyro", "Gyroscope", names(tidy))
names(tidy)<-gsub("BodyBody", "Body", names(tidy))
names(tidy)<-gsub("Mag", "Magnitude", names(tidy))
names(tidy)<-gsub("^t", "Time", names(tidy))
names(tidy)<-gsub("^f", "Frequency", names(tidy))
names(tidy)<-gsub("tBody", "TimeBody", names(tidy))
names(tidy)<-gsub("-mean()", "Mean", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-std()", "STD", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-freq()", "Frequency", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("angle", "Angle", names(tidy))
names(tidy)<-gsub("gravity", "Gravity", names(tidy))

#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy2<-aggregate(. ~subject + activity, tidy, mean)
tidy2<-tidy2[order(tidy2$subject,tidy2$activity),]
write.table(tidy2, file = "tidydata.txt",row.name=FALSE)