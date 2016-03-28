#Coursera Getting and Cleaning Data Course Project

# run_analysis.r
# Written in March 2016

# Objectives of code

# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# The data to use should be in the same directory and is available here
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# A description of the project and the data produces is here: -
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# Code included below

###########################################
# 1.Merges the training and the test sets to create one data set.

#  Load up the data
zipfile<-"getdata_projectfiles_UCI HAR Dataset.zip"
features = read.table(unz(zipfile,'UCI HAR Dataset/features.txt'),header=F)
activityLabels = read.table(unz(zipfile,'UCI HAR Dataset/activity_labels.txt'),header=F)
# load training data
subjectTrain = read.table(unz(zipfile,'UCI HAR Dataset/train/subject_train.txt'),header=F)
XTrain= read.table(unz(zipfile,'UCI HAR Dataset/train/X_train.txt'),header=F)
yTrain= read.table(unz(zipfile,'UCI HAR Dataset/train/y_train.txt'),header=F)
# load test data
subjectTest = read.table(unz(zipfile,'UCI HAR Dataset/test/subject_test.txt'),header=F)
XTest= read.table(unz(zipfile,'UCI HAR Dataset/test/X_test.txt'),header=F)
yTest= read.table(unz(zipfile,'UCI HAR Dataset/test/y_test.txt'),header=F)

# sort out column names for the above
colnames(activityLabels) = c('activityID','activityType')
colnames(subjectTrain) = "subjectID"
colnames(XTrain) = features[,2] # the names of the features are stored in features
colnames(yTrain) = "activityID"
colnames(subjectTest) = "subjectID"
colnames(XTest) = features[,2] # the names of the features are stored in features
colnames(yTest) = "activityID"

# merge the two sets together having combined the two individual sets
allTraining = cbind(yTrain,subjectTrain,XTrain)
allTest = cbind(yTest,subjectTest,XTest)
allTrainingandTest = rbind(allTraining,allTest)

###########################################
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

columnNames<-names(allTrainingandTest)
                      
useHeadings<-grepl("subjectID",columnNames) | grepl("activityID", columnNames)
useMeans<- grepl("mean",columnNames) & !grepl("meanF",columnNames)
useStd<- grepl("std",columnNames) 
useColumns<- useHeadings | useMeans | useStd

extractedData<- allTrainingandTest[useColumns]


# 3.Use descriptive activity names to name the activities in the data set
# merge with the activity labels to give the activities a description
describedData<- merge(extractedData,activityLabels,by ='activityID')

# 4.Appropriately label the data set with descriptive variable names. 
# Clean up the variable names
columnNames<-names(describedData)
for (i in 1:length(columnNames)) 
{
        columnNames[i] = gsub("\\()","",columnNames[i])
        columnNames[i] = gsub("-std","StdDev",columnNames[i])
        columnNames[i] = gsub("-mean","Mean",columnNames[i])
        columnNames[i] = gsub("^(t)","time",columnNames[i])
        columnNames[i] = gsub("^(f)","frequency",columnNames[i])
        columnNames[i] = gsub("([Gg]ravity)","Gravity",columnNames[i])
        columnNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNames[i])
        columnNames[i] = gsub("[Gg]yro","Gyro",columnNames[i])
        columnNames[i] = gsub("Mag","Magnitude",columnNames[i])
        columnNames[i] = gsub("Acc","Acceleration",columnNames[i])
}
# assign these columnnames to the described data
names(describedData)<-columnNames

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, finalDataNoActivityType without the activityType column
extractedDataforSumming  = describedData[,names(describedData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(extractedDataforSumming[,names(extractedDataforSumming) != c('activityID','subjectID')], by=list(activityID=extractedDataforSumming$activityID,subjectID=extractedDataforSumming$subjectID), FUN=mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityLabels,by='activityID',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')