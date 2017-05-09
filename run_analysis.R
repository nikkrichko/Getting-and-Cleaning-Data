# import libraries
library(data.table)
library(reshape2)

# need to prepare date. Un zip it to folder and setup 'UCI HAR Dataset' as working directory
setwd("./UCI HAR Dataset")


###################################################
# read activitity and features
#read activity
activity_labels_col_names <- c("classLabels", "activityName")
activityLabel <- fread("activity_labels.txt", col.names = activity_labels_col_names)

# read features
features_col_names <- c("index", "featureName")
feature <- fread("features.txt", col.names = features_col_names)

# get only only names contains mean and std
featuresWanted <- grep("(mean|std)\\(\\)", feature[, featureName])

# combining measurements
measurements <- feature[featuresWanted, featureName]
measurements <-  gsub('[()]', '', measurements)

#############################################
# read train dataset
# read train
trainMeasurements <- fread("train/X_train.txt")

## redefine only measurements which contains mean and std
trainMeasurements <- trainMeasurements[,featuresWanted, with = FALSE]
## set names for columns
colnames(trainMeasurements) <- measurements

# read Train activities
train_activities_col_names <- c("Activity")
trainActivities <- fread("train/Y_train.txt", col.names = train_activities_col_names) 

# read train subjects
train_Subjects_col_names <- c("SubjectNum")
trainSubjects <- fread("train/subject_train.txt", col.names = train_Subjects_col_names) 

#concatenate train dataSet from trainMeasurements, trainActivities and trainSubjects
trainDataSet <- cbind(trainSubjects, trainActivities,trainMeasurements)

#############################################

# read test dataset
# read test
testMeasurements <- fread("test/X_test.txt")

## redefine only measurements which contains mean and std
testMeasurements <- testMeasurements[,featuresWanted, with = FALSE]
## set names for columns
colnames(testMeasurements) <- measurements

# read Train activities
test_activities_col_names <- c("Activity")
testActivities <- fread("test/Y_test.txt", col.names = test_activities_col_names) 

# read train subjects
test_Subjects_col_names <- c("SubjectNum")
testSubjects <- fread("test/subject_test.txt", col.names = test_Subjects_col_names) 

#concatenate train dataSet from testMeasurements, testActivities and testSubjects
testDataSet <- cbind(testSubjects, testActivities,testMeasurements)


############################################

# mergeDatasets: create combinedDataSet from testDataSet and trainDataSet
combinedDataSet <- rbind(testDataSet, trainDataSet)

# rename activities id to more readeble factor name from dataset "activityLabel"
combinedDataSet[["Activity"]] <- factor(combinedDataSet[, Activity]
                                 , levels = activityLabel[["classLabels"]]
                                 , labels = activityLabel[["activityName"]])

# transfer SubjectNum to factor
combinedDataSet[["SubjectNum"]] <- as.factor(combinedDataSet[, SubjectNum])

# mentind data for next step
meltedcombinedDataSet <- reshape2::melt(data = combinedDataSet, id = c("SubjectNum", "Activity"))

# calculating mean for each activity and for each subject
dCastedcombinedDataSet <- reshape2::dcast(data = meltedcombinedDataSet, SubjectNum + Activity ~ variable, fun.aggregate = mean)

#saving to data to file
write.table(x=dCastedcombinedDataSet, file = "tidyData.txt", row.name=FALSE )
