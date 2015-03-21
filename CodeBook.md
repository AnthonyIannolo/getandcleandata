#  CodeBook for Coursera Getting and Cleaning Data Assignment

##  Project Overview

Assignment is to create a tidy dataset from Smartphone Data on the UCI Machine Learning Repository [1]. The data includes user activity observations from accelerometer on a Samsung Galaxy S Smartphone.

##  Raw Data Set Information from UCI Machine Learning Repository:

The experiments were done with a group of 30 volunteers between 19-48 years. Each volunteer performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using the embedded accelerometer and gyroscope, they captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz.  The datasets were randomly partitioned into two sets, where 70% of the volunteers were selected for generating the training data and 30% for the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


For each record in the dataset the following was provided:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label of the 6 described above. 
- An identifier of the subject volunteer who carried out the experiment.

Here is a description of the files read in:

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

- 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

## Steps to Get from Raw Data to Tidy Data

###1.  Read the raw data and merge the training and the test data sets to create one data set.
Raw Files to read:
  subject_train, 
  X_train, 
  y_train, 
  subject_test, 
  X_test, 
  y_test, 
  features, 
  activity_labels
  
Combining the data results in a Data Frame of 10,299 observations of 563 variables.

###2.  Extract only the measurements on the mean and standard deviation for each measurement.

Extracting the mean and standard deviation measurements results in a Data Frame of 10299 observations and 81 variables.

###3.  Use descriptive activity names to name the activities in the data set.
Add improved descriptive activity names instead of numerical designations.

###4.  Appropriately label the data set with descriptive variable names.
Tidy up the variable names by removing parentheses, include full words for abbreviations and change to lowercase.

###5.  Create a second, independent tidy data set with the average of each variable for each Activity and Subject.
Average of each variable by Activity and subject results in a DataFrame of 180 Observations of 81 variables.


##  References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012