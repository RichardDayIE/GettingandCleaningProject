# Getting and Cleaning Data Course Project
## GettingandCleaningProject


The purpose of this project is to demonstrate an ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. 

It is required to submit: 
1) a tidy data set as described below, 
2) a link to a Github repository with your script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 

# This is the CodeBook.md file

## Source Data
 A full description of the data is available at the site where the original data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data is available here: -

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Background
The readme file from the zip data contains the following information about the source of the data

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

### For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### The dataset includes the following files:
=========================================

- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 


# Code and transformations
The different sections of the code and what they do in order to transform the data are as follows: -
### 1.Merges the training and the test sets to create one data set.

####  Load up the data
Load up the training and text data from the zip file.
Load up the features and activity types (activityLabels) too


#### sort out column names for the above
Give the data tables appropriate column names.  The second column in features will name the columns on the main X data 

#### merge the two sets together having combined the two individual sets
Then the three sets in training and test can be put together with cbind
and the training and test set can be combined with a rbind 

### 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

The columns to be used can be found with grepl, and then combined.
This vector is then used to extract the relevant columns.

### 3.Use descriptive activity names to name the activities in the data set
The activity labels are merged to give the activities a description


### 4.Appropriately label the data set with descriptive variable names. 
The names are cleaned up using a for loop and gsub to replace t with time, Mag with Magnitude etc.


# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

This is accomplished by aggregating the data by activity and subject, combining this back with the activity labels and then writing out the required file.

