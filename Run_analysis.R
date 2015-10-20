######

## Coursera Getting and Cleaning Data Course Project
## Di Yao
## 2015-10-19

##Run_analysis.R that does the following. 
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


rm(list=ls())

setwd("~/Google Drive/Learnings/Data_Science/Clean_data/Course_Project")

if(!file.exists("dataset.zip")){
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileurl,"dataset.zip",method = "curl")
}

if(!file.exists("UCI HAR Dataset")){
  unzip("dataset.zip")
}  

setwd('./UCI HAR Dataset')
features = read.csv("features.txt",sep="", header = FALSE)
act_type = read.csv("activity_labels.txt",sep = "",header = FALSE)
subject_train = read.csv("./train/subject_train.txt", sep="",header = FALSE)
x_train = read.csv("./train/x_train.txt", sep="",header = FALSE)
y_train = read.csv("./train/y_train.txt", sep="",header = FALSE)

trainData = cbind(x_train,y_train,subject_train)
##colnames() <- c("subject_ID",features$V2,"activity_ID")

subject_test = read.csv("./test/subject_test.txt", sep="",header = FALSE)
x_test = read.csv("./test/x_test.txt", sep="",header = FALSE)
y_test = read.csv("./test/y_test.txt", sep="",header = FALSE)
testData = cbind(x_test,y_test,subject_test)

Data=rbind(trainData,testData)  ## Data Merged. Task #1 Completed



features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'StdDev', features[,2])
features[,2] = gsub('[-()]', '', features[,2])



## Task #2 Extract only Mean and Std measurement
cols_mean_std <- grep(".*Mean.*|.*StdDev.*", features[,2])
features <- features[cols_mean_std,]
cols_mean_std <- c(cols_mean_std, 562, 563)
Data <- Data[,cols_mean_std]

##Task #4: label data set
colnames(Data) <- c(features$V2, "Activity", "Subject")
colnames(Data) <- tolower(colnames(Data))

##Task #3: use descriptive names
act_ID = 1
for (current_act_type in act_type$V2) {
  Data$activity <- gsub(act_ID, current_act_type, Data$activity)
  act_ID <- act_ID + 1
}

Data$activity <- as.factor(Data$activity)
Data$subject <- as.factor(Data$subject)

## Task #5: generate new data set
tidy = aggregate(Data, by=list(activity = Data$activity, subject=Data$subject), mean)

head(tidy[,1:5])
