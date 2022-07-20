# extract UCI HAR Dataset zip file to current working directory
# first import necessary files to r studion
# we start by import test data set
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
#import train data set
xtrain <- read.table("./UCI HAR Dataset/train/x_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
#import features file
features <- read.table("./UCI HAR Dataset/features.txt")
#import activities labels file get activity names
activitylables <- read.table("./UCI HAR Dataset/activity_labels.txt")
#create testdata table by combining xtest table, ytest table new column indicating record
# subject is test
testdata <- data.frame(xtest,ytest)
testdata$subject <-rep("test",nrow(xtest))
#create traindata table by combining xtest table, ytest table new column indicating record
# subject is train
traindata <- data.frame(xtrain,ytrain)
traindata$subject <-rep("train",nrow(xtrain))


# Merging the training and the test sets to create one data set
alldata <- rbind(testdata,traindata)
#remove unnecessary character in features list
features$V2 <- gsub("[-(),]","",features$V2) 
# create variable use for variable names

varnames <- c(features$V2, c("activity", "subject"))
names(alldata) <- varnames
#Extracts only the measurements on the mean and standard deviation for each measurement.
extracteddata <- alldata[,grepl(pattern = "mean|std|activity|subject", colnames(alldata))]

#Uses descriptive activity names to name the activities in the data set
for(y in extracteddata$activity){
  extracteddata$activity[extracteddata$activity==y] <- activitylables$V2[activitylables$V1==y]}

#creates  data set with the average of each variable for each activity and each subject

summarybyactivitysubject <-extracteddata %>%
  group_by(activity, subject)%>%
  summarise_if(is.numeric, mean)
