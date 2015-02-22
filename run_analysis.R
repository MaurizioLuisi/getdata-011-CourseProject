# This R script called run_analysis.R that does the following:
# Data source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 0) Reades the txt files from the data sets downloaded from the above link into the local
#    folder or "path"
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# load relevant library
library(dplyr)
# Assing path to data folder
path <- "C:/Users/Week3/UCI HAR Dataset"
# Import the training and the test data sets
X_train <- read.table(paste(path,"/train/X_train.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
y_train <- read.table(paste(path,"/train/y_train.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
subject_train <- read.table(paste(path,"/train/subject_train.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
X_test <- read.table(paste(path,"/test/X_test.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
y_test <- read.table(paste(path,"/test/y_test.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
subject_test <- read.table(paste(path,"/test/subject_test.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
# Import the features and the activity descriptive data sets
features <- read.table(paste(path,"/features.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
activity <- read.table(paste(path,"/activity_labels.txt",sep=""), quote="\"", stringsAsFactors=FALSE)
#########################################################################
# Replace activities numeric codes with descriptive activity names from the activities names data set
for(y in 1:dim(activity)[1]){
  y_test[y_test==activity[y,1]] <- activity[y,2]
  y_train[y_train==activity[y,1]] <- activity[y,2]
}
# Combines the activity, subject and features data for the training and the test sets
X_train <- cbind(y_train,subject_train,X_train)
X_test <- cbind(y_test,subject_test,X_test)
labels.col <- c("Activity","Subject",features[,2])
#Appropriately labels the data set with descriptive variable names. 
new.labels <- make.names(gsub("[()]","",labels.col))
dimnames(X_train)[2]<- list(new.labels)
dimnames(X_test)[2]<-list(new.labels)

# Merges the training and the test sets to create one data set
X_merged <- rbind(X_train,X_test)
# Select index vectors for Means and Stds
MeanVec <- setdiff(grep("mean", labels.col ), grep("meanFreq",labels.col ))
StdVec <- grep("std", labels.col )
# Extracts only the measurements on the mean and standard deviation for each measurement.
X_dataSet<- X_merged[, c(1,2,MeanVec,StdVec)]
# Creates a tidy data set with the average of each variable for each activity and each subject
TidyDataSet<-X_dataSet%>% group_by(Activity,Subject)%>% summarise_each(funs(mean))
# Saves the tidy data set created as a txt file 
write.table(TidyDataSet,paste(path,"/TidyDataSet.txt",sep=""),row.name=FALSE)