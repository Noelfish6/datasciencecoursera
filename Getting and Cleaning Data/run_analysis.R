
# 1. merges the training and the test sets to create one data set

## create training data set
### import the data
activityLabels <- read.table('UCI HAR Dataset/activity_labels.txt', header=FALSE); 
colnames(activityLabels) <- c("activityLabel","activityName");

features <- read.table('UCI HAR Dataset/features.txt', header=FALSE);

subjectTrain <- read.table('UCI HAR Dataset/train/subject_train.txt', header=FALSE); 
colnames(subjectTrain) <- "subjectId";

xTrain <- read.table('UCI HAR Dataset/train/x_train.txt', header=FALSE); 
colnames(xTrain) <- features[,2];

yTrain <- read.table('UCI HAR Dataset/train/y_train.txt', header=FALSE); 
colnames(yTrain) <- "activityId";

### merge data into a file
trainingSet = cbind(yTrain,subjectTrain,xTrain);

## create test data set
### import test data
subjectTest <- read.table('UCI HAR Dataset/test/subject_test.txt', header=FALSE); 
colnames(subjectTest) <- "subjectId";

xTest <- read.table('UCI HAR Dataset/test/x_test.txt',header=FALSE); 
colnames(xTest) <- features[,2];

yTest <- read.table('UCI HAR Dataset/test/y_test.txt',header=FALSE); 
colnames(yTest) <- "activityId";

### merge data into a file
testSet = cbind(yTest,subjectTest,xTest);

## combine the two data sets into one data set
mergedDataSet = rbind(trainingSet,testSet);

## create columns vector to prepare data for subsetting
columns <- colnames(mergedDataSet);



# 2. extract only the measurements on the mean and standard deviation for each measurement

## create a vector that indentifies the ID, mean & stddev columns as TRUE
vector <- (grepl("activity..", columns) | grepl("subject..", columns) | grepl("-mean..", columns) &
             !grepl("-meanFreq..", columns) & !grepl("mean..-", columns) | 
             grepl("-std..", columns) & !grepl("-std()..-", columns));

## update MergedDataSet based on previously identified columns
mergedDataSet <- mergedDataSet[vector==TRUE];



# 3. use descriptive activity names to name the activities in the data set

## add in descriptive activity names to MergedDataSet & update columns vector
mergedDataSet <- merge(mergedDataSet,activityLabels,by='activityId',all.x=TRUE);
mergedDataSet$activityId <-activityLabels[,2][match(mergedDataSet$activityId, activityLabels[,1])] 
columns <- colnames(mergedDataSet);



# 4. appropriately label the data set with descriptive activity names.

## tidy column names
for (i in 1:length(columns)) {
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
};

## update mergedDataSet with new descriptive column names
colnames(mergedDataSet) <- columns;

## remove activityType column
mergedDataSet <- mergedDataSet[,names(mergedDataSet) != 'activityType'];



# 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## averaging each activity and each subject as Tidy Data
tidyData <- aggregate(mergedDataSet[,names(mergedDataSet) 
                                    != c('activityId','subjectId')],by=list
                      (activityId=mergedDataSet$activityId,
                        subjectId=mergedDataSet$subjectId),mean);

## Export tidyData set 
write.table(tidyData, 'FinalTidyData.txt',row.names=FALSE,sep='\t')
