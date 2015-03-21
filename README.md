#  Coursera Getting and Cleaning Data Assignment

##  Project Overview

Assignment is to create a tidy dataset from Smartphone Data on the UCI Machine Learning Repository.
The data includes user activity observations from an accelerometer on Samsung Galaxy S Smartphone.

The database was built from the recordings of 30 Subjects aged 19-48 performing activities of 
daily living while carrying a waist mounted smartphone with embedded inertial sensors.

The major steps in the course assignment script(run_analysis.R) to create the Tidy dataset are as follows:

1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement. 
3. Use descriptive activity names to name the activities in the data set.
4. Appropriately label the data set with descriptive variable names.
5. Create a second, independent tidy data set with the average of each variable for each Activity and subject.

##  Contents of this Repository

- README.md

- run_analysis.R:  R script that creates the dataset by executing steps described above.

- CodeBook.md:     Describes steps to get from raw data to tidy data plus data descriptions.

##  How to use this script
1. First Download the files using this link.
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

2. Update the data_folder variable in the run_analysis.R with your valid path name and run.
