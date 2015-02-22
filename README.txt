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
# Assign path to data folder
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

##################################################################################
##### CODE BOOK ##################################################################
##################################################################################
The TidyDataSet has 180 observations and 68 variables.

The complete list of variables in the TidyDataSet is reported below:

Activity
Subject
tBodyAcc.mean.X
tBodyAcc.mean.Y
tBodyAcc.mean.Z
tGravityAcc.mean.X
tGravityAcc.mean.Y
tGravityAcc.mean.Z
tBodyAccJerk.mean.X
tBodyAccJerk.mean.Y
tBodyAccJerk.mean.Z
tBodyGyro.mean.X
tBodyGyro.mean.Y
tBodyGyro.mean.Z
tBodyGyroJerk.mean.X
tBodyGyroJerk.mean.Y
tBodyGyroJerk.mean.Z
tBodyAccMag.mean
tGravityAccMag.mean
tBodyAccJerkMag.mean
tBodyGyroMag.mean
tBodyGyroJerkMag.mean
fBodyAcc.mean.X
fBodyAcc.mean.Y
fBodyAcc.mean.Z
fBodyAccJerk.mean.X
fBodyAccJerk.mean.Y
fBodyAccJerk.mean.Z
fBodyGyro.mean.X
fBodyGyro.mean.Y
fBodyGyro.mean.Z
fBodyAccMag.mean
fBodyBodyAccJerkMag.mean
fBodyBodyGyroMag.mean
fBodyBodyGyroJerkMag.mean
tBodyAcc.std.X
tBodyAcc.std.Y
tBodyAcc.std.Z
tGravityAcc.std.X
tGravityAcc.std.Y
tGravityAcc.std.Z
tBodyAccJerk.std.X
tBodyAccJerk.std.Y
tBodyAccJerk.std.Z
tBodyGyro.std.X
tBodyGyro.std.Y
tBodyGyro.std.Z
tBodyGyroJerk.std.X
tBodyGyroJerk.std.Y
tBodyGyroJerk.std.Z
tBodyAccMag.std
tGravityAccMag.std
tBodyAccJerkMag.std
tBodyGyroMag.std
tBodyGyroJerkMag.std
fBodyAcc.std.X
fBodyAcc.std.Y
fBodyAcc.std.Z
fBodyAccJerk.std.X
fBodyAccJerk.std.Y
fBodyAccJerk.std.Z
fBodyGyro.std.X
fBodyGyro.std.Y
fBodyGyro.std.Z
fBodyAccMag.std
fBodyBodyAccJerkMag.std
fBodyBodyGyroMag.std
fBodyBodyGyroJerkMag.std

VARIABLES DESCRIPTION:
"Subject": The variable "Subject" referes to a group of 30 volunteers within an age bracket of 19-48 years, each person is coded with a number form 1 to 30.
"Activity": Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 

Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation

Notes: 
======
- The original Features are normalized and bounded within [-1,1], hence the resulting mean are bounded within [-1,1].
