
# Coursera: Getting and Cleaning Data
# John Binder 
# Course Project 
# You should create one R script called run_analysis.R that does the following:  
# 1. Merges the training and the test sets to create one data set. 
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.  
# 3. Uses descriptive activity names to name the activities in the data set 
# 4. Appropriately labels the data set with descriptive variable names.  
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each  
#    variable for each activity and each subject. AF Note: also export final tidy data set for upload. 

##  MERGE TRAINING + TEST SETS 
## load packages  
require(plyr) 
require(dplyr)  
require(tidyr) 

# read in features from features table 
features <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/features.txt") 
 
# read in training data tables 
train_set <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/train/X_train.txt") 
train_activity <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/train/Y_train.txt") 
subject_train <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/train/subject_train.txt") 

# update train_set for feature labels names 
names(train_set) <- features$V2 

# update train_activity activity column name 
names(train_activity) <- c("Activity") 

# update subject_train subject column name 
names(subject_train) <- c("Subject") 

# merge training data tables 
dt_train <- cbind(subject_train, train_activity, train_set) 

# read in test data tables 
test_set <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/test/X_test.txt") 
test_activity <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/test/Y_test.txt") 
subject_test <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/test/subject_test.txt") 
 
# update test set for feature labels names 
names(test_set) <- features$V2 

# update test_activity activity column name 
names(test_activity) <- c("Activity") 

# update subject_test subject column name 
names(subject_test) <- c("Subject") 

# merge test data tables 
dt_test <- cbind(subject_test, test_activity, test_set) 

# merge training and test data tables 
dt_merged <- rbind(dt_test, dt_train) 
## STEP 1 COMPLETED 

## BEGIN STEP 2 - EXTRACT ONLY MEAN AND STANDARD DEVIATION MEASUREMENTS FROM COMBINED DATA TABLE. 
#select columns cotaining Activity, Subjecct, mean, std then combine 
Activity <- dt_merged[, grepl("Activity", colnames(dt_merged))] 
Subject <- dt_merged[, grepl("Subject", colnames(dt_merged))] 
dt_mean <- dt_merged[,grepl("mean", colnames(dt_merged))] 
dt_std <- dt_merged[,grepl("std", colnames(dt_merged))] 
dt_mean_std <- cbind(Activity, Subject, dt_mean, dt_std) 
# STEP 2 COMPLETED 

## BEGIN STEP 3 - UPDATE TABLE FROM STEP 2 W/ ACTIVITY DESCRIPTION 
# Read in activity label data table 
activity_labels <- read.table("C:/Users/jbinderaero/OneDrive/Data Scientists Toolbox/Getting and Cleaning Data/Week 4/Project/UCI HAR Dataset/activity_labels.txt") 
# Update Activity column values with descriptions from activity_labels table 
dt_mean_std$Activity <- factor(dt_mean_std$Activity, levels = activity_labels[,1], labels = activity_labels[,2]) 
# STEP 3 COMPLETED. 


## BEGIN STEP 4 - LABEL FINAL DATA SET WITH DESCRIPTIVE VALUE NAMES 
# update codes in field values according to mapping info in features_info.txt file 
names(dt_mean_std) <- gsub("^t", "time", names(dt_mean_std)) 
names(dt_mean_std) <- gsub("^f", "frequency", names(dt_mean_std)) 
names(dt_mean_std) <- gsub("Acc", "Accelerometer", names(dt_mean_std)) 
names(dt_mean_std) <- gsub("Gyro", "Gyroscope", names(dt_mean_std)) 
names(dt_mean_std) <- gsub("Mag", "Magnitude", names(dt_mean_std)) 
names(dt_mean_std) <- gsub("BodyBody", "Body", names(dt_mean_std)) 
# STEP 4 COMPLETE 

## BEGIN STEP 5 - CREATE TIDY DATA SET W/ AVE. OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT 
# take the mean of all values for each activity and subject 
dt_final <- aggregate(. ~ Activity + Subject, data = dt_mean_std, mean)   
# tidy up data by grouping for each activity and subject, according to step 5 requirement 
dt_final <- dt_final[order(dt_final$Activity, dt_final$Subject),] 
# STEP 5 COMPLETE 

## Create data file from STEP 5 for course project upload. 

write.table(dt_final, "dt_final.txt", row.names = FALSE, quote = FALSE) 

