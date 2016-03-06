# Use install.packages("data.table")  with quotes

library(data.table)
library(dplyr)

# I already downloaded the .zip and extracted it in my working directory
# Now Read in the Metadada
# The metadata are the names of features "fNames" and activities "aLables"
fNames <- read.table("UCI HAR Dataset/features.txt")
aLables <- read.table("UCI HAR Dataset/activity_labels.txt")

# Read in the training data for subject, activity and features trains
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
aTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
fTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)


# Enter the test data for the same areas
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
aTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
fTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Task 1. Merge the training and the test sets to create one data set. Look at past notes for rbind.
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(aTrain, aTest)
features <- rbind(fTrain, fTest)

# Now take the columns in the features data sent from fNames...
colnames(features) <- t(fNames[2])

# Task 1. Merge the data....

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
Data <- cbind(features, activity, subject)

# Task 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# create a column with the mean and standard deviation calculation from the "Data" object
meanstd <- grep(".*Mean.*|.*Std.*", names(Data), ignore.case=TRUE)

# Columns needed for Task #2

Columns <- c(meanstd, 562, 563)
dim(Data)

# Make Data2 with needed columns from Columns

Data2 <- Data[,Columns]
dim(Data2)

# Task 3. Use descriptive activity names to name the activities in the data set

Data2$activity <- as.character(Data2$activity)

for (i in 1:6) {
        
        Data2$Activity[Data2$Activity == i] <- as.character(aLables[i,2])
}

# factor activity variable once names are current

Data2$Activity <- as.factor(Data2$Activty)

# Task 4. Label the data set with descriptive variable names

names(Data2)

# Make the names understandable

names(Data2)<-gsub("Acc", "Accelerometer", names(Data2))
names(Data2)<-gsub("Gyro", "Gyroscope", names(Data2))
names(Data2)<-gsub("BodyBody", "Body", names(Data2))
names(Data2)<-gsub("Mag", "Magnitude", names(Data2))
names(Data2)<-gsub("^t", "Time", names(Data2))
names(Data2)<-gsub("^f", "Frequency", names(Data2))
names(Data2)<-gsub("tBody", "TimeBody", names(Data2))
names(Data2)<-gsub("-mean()", "Mean", names(Data2), ignore.case = TRUE)
names(Data2)<-gsub("-std()", "STD", names(Data2), ignore.case = TRUE)
names(Data2)<-gsub("-freq()", "Frequency", names(Data2), ignore.case = TRUE)
names(Data2)<-gsub("angle", "Angle", names(Data2))
names(Data2)<-gsub("gravity", "Gravity", names(Data2))

# Task 5. From the data set in step 4, create a second, independent tidy data set with the average of
# each varable for each activity and each subject

# use "Subject" as a factor variable

Data2$Subject <- as.factor(Data2$Subject)
Data2 <- data.table(Data2)

# Make "Tidy1" as a dataset with the average for each activit and each subject.

Tidy1 <- aggregate(. ~Subject + Activity, Data2, mean)
Tidy1 <- Tidy1[order(Tidy1$Subject,Tidy1$Activity),]
write.table(Tidy1, file = "Tidy.txt", row.names = FALSE)




