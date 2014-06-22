#Getting and Cleaning Data - Course Project
#Done By- Dishan S. Rao

#working directory is set to the location where Dataset was unzipped
#unziping was done manualy 
setwd("C:/Users/dishansrao/Desktop/data/UCI HAR Dataset")

# Reading the data from files
features     <- read.table("features.txt",header=FALSE); 
act_labels <-read.table("activity_labels.txt",header=FALSE); 
subject_train <-read.table("subject_train.txt",header=FALSE); 
xtrain       <-read.table("x_train.txt",header=FALSE); 
ytrain      <- read.table("y_train.txt",header=FALSE); 

# column names is assigned to the data imported above
colnames(act_labels)  <- c("activityId","activityType");
colnames(subject_train)  <- "subjectId";
colnames(xtrain)        <- features[,2]; 
colnames(ytrain)        <- "activityId";

# the final training set by merging yTrain, subjectTrain, and xTrain
trainingData <- cbind(ytrain,subject_train,xtrain);

# Reading the test data
subject_test <- read.table("subject_test.txt",header=FALSE); #imports subject_test.txt
xtest       <- read.table("x_test.txt",header=FALSE); #imports x_test.txt
ytest       <- read.table("y_test.txt",header=FALSE); #imports y_test.txt

# Assigning column names to the test data imported above
colnames(subject_test) <- "subjectId";
colnames(xtest)       <- features[,2]; 
colnames(ytest)       <- "activityId";

# Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(ytest,subject_test,xtest);

# Combine training and test data to create a final data set
finalData <- rbind(trainingData,testData);

### Data Extraction: 
# Extract only the measurements on the mean and standard deviation for each measurement.
# keep the activity column as well
# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns

colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,act_labels,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,act_labels,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');