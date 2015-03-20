##  Coursera Getting and Cleaning Data Assignment
##
##  Assignment is to create a tidy dataset from Smartphone Data on UCI Machine Learning Repository.
##  The data include user activity observations from accelerometer on Samsung Galaxy S Smartphone.
##  
##  Major steps as follows:
##  1. Merge the training and the test sets to create one data set.
##  2. Extract only the measurements on the mean and standard deviation for each measurement. 
##  3. Use descriptive activity names to name the activities in the data set.
##  4. Appropriately label the data set with descriptive variable names.
##  5. Create a second, independent tidy data set with the average of each variable for each
##     Activity and Subject.

##  Variable that specifies the path to the downloaded data from UCI
##
data_folder <- "/Users/anthonyiannolo/coursera/course3assign/"


##  Create strings to subsequently read the data
##
subject_train_string <- paste(data_folder,"/train/subject_train.txt",sep="")
X_train_string <- paste(data_folder,"train/X_train.txt", sep="")
y_train_string <- paste(data_folder,"/train/y_train.txt", sep="")
subject_test_string <- paste(data_folder,"/test/subject_test.txt",sep="")
X_test_string <- paste(data_folder,"/test/X_test.txt",sep="")
y_test_string <- paste(data_folder,"/test/y_test.txt",sep="")
features_string <- paste(data_folder,"/features.txt", sep="")
activity_labels_string <- paste(data_folder,"/activity_labels.txt",sep="")
                                                      
##  Read the Training and Test Data
##
subject_train <- read.table(subject_train_string)
X_train <- read.table(X_train_string)
y_train <- read.table(y_train_string)
subject_test <- read.table(subject_test_string)
X_test <- read.table(X_test_string)
y_test <- read.table(y_test_string)
features <- read.table(features_string, colClasses = c("character"))
activity_labels <- read.table(activity_labels_string, col.names = c("ActivityId", "Activity"))


##  Combine the columns for the Training and Test Data
##
train_data <- cbind(y_train, subject_train, X_train)
test_data <- cbind(y_test, subject_test, X_test)

##  Step 1. Combine the rows for the Training and Test Data to create one dataset
##
combined_data <- rbind(train_data, test_data)

##  Create the combined column Labels
## 
combined_labels <- rbind(c(1, "ActivityId"), c(2,"Subject"), features)[,2]
names(combined_data) <- combined_labels

##  Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
##
data_mean_std <- combined_data[,grepl("ActivityId|Subject|mean|std", names(combined_data))]

##  Step 3. Merge the activity labels with the extracted mean and std values from above
##  to create dataset with descriptive activity names
labeled_data_mean_std  <- merge(activity_labels,data_mean_std,by.x="ActivityId",by.y="ActivityId",all=TRUE)

##  Remove the numeric ID description
##
labeled_data_mean_std <- labeled_data_mean_std[2:82]

##  Step 4. Label the dataset with improved descriptive variable names
##
## Remove parentheses
##
names(labeled_data_mean_std) <- gsub('\\(|\\)',"",names(labeled_data_mean_std), perl = TRUE)

##  Improve descriptive names
##
names(labeled_data_mean_std) <- gsub("^f","frequency",names(labeled_data_mean_std))
names(labeled_data_mean_std) <- gsub("^t","time",names(labeled_data_mean_std))
names(labeled_data_mean_std) <- gsub("Freq","frequency",names(labeled_data_mean_std))
names(labeled_data_mean_std) <- gsub("Acc","acceleration",names(labeled_data_mean_std))
names(labeled_data_mean_std) <- gsub("Gyro","gyroscope",names(labeled_data_mean_std))
names(labeled_data_mean_std) <- gsub("Mag","magnitude",names(labeled_data_mean_std))
names(labeled_data_mean_std) <- gsub("std","standarddeviation",names(labeled_data_mean_std))

##
##  Make all lowercase
names(labeled_data_mean_std) <- tolower(names(labeled_data_mean_std))


##  Step 5. Create second tidy data set with the average of each variable for activity and subject.
##
tidy_avg_by_activity_subject <- aggregate(labeled_data_mean_std[,-1],
                                    by=list(labeled_data_mean_std$subject, 
                                            labeled_data_mean_std$activity), mean)

##  Clean up variable names and trim column prior to writing
##
tidy_avg_by_activity_subject  <- tidy_avg_by_activity_subject [2:82]
names(tidy_avg_by_activity_subject) <- gsub("Group.2","activity",names(tidy_avg_by_activity_subject))

##  Write out second tidy data set to a text file.
##
write.table(tidy_avg_by_activity_subject, file = paste(data_folder, "tidy_avg_by_activity_subject.txt",sep=" "),row.name=FALSE)

