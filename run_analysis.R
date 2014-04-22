#inicio----
getwd()
PATH <- "C:/Users/jpuigdellivol/Desktop/Coursera Data science Specializations/03 - Getting and Cleaning Data/Week 4/04 - Assigment/UCI HAR Dataset"
setwd(PATH)

#extract the raw datas
X_data_train_raw <- read.table("./train/X_train.txt")
X_data_test_raw  <- read.table("./test/X_test.txt")
#binding train and test datas

#....1.Merges the training and the test sets to create one data set.----
X_data_raw       <- rbind(X_data_train_raw,X_data_test_raw)

#cleaning memory
rm(X_data_train_raw)
rm(X_data_test_raw)

#Geting all the features
feat <- read.table("./features.txt",sep=" ")

#giving apropiate names
colnames(X_data_raw) <- feat$V2

#geting only mean and standard deviation features
#....2.Extracts only the measurements on the mean and standard deviation for each measurement. ----
indAveSdFeat <- grep("[Mm]ean|[Ss]td()",feat$V2)
Results1 <- X_data_raw[,indAveSdFeat]

#cleaning memory
rm(feat)
rm(indAveSdFeat)

#Extracting activity data
activity_train <- read.table("./train/y_train.txt",sep=" ")
activity_test  <- read.table("./test/y_test.txt",sep=" ")
#binding train and test datas
activity       <- rbind(activity_train,activity_test)
dim(activity)

#cleaning memory
rm(activity_train)
rm(activity_test)

#....3.Uses descriptive activity names to name the activities in the data set-----
activity_labels <- read.table("activity_labels.txt",sep=" ")
activity_names  <- merge(activity,activity_labels)

#cleaning memory
rm(activity_labels)


#....4.Appropriately labels the data set with descriptive activity names.---- 
Results1$activity<-activity_names

#showing results
dim(Results1)
colnames(Results1)
head(Results1)
summary(Results1)

#...5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. ----

#Extracting subjects data
subject_train <- read.table("./train/subject_train.txt",sep=" ")
subject_test  <- read.table("./test/subject_test.txt",sep=" ")
#binding train and test datas
subject       <- rbind(subject_train,subject_test)
dim(subject)

#cleaning memory
rm(subject_train)
rm(subject_test)

# get all the possible conbinations beetwin subject and activity_names
splited_SA<-paste(subject[,1],activity_names$V2)


#Split data column by column,by subjects and activity
X_data_SA      <- split  ( X_data_raw[[1]] , splited_SA )

#geting the mean of each split
X_data_mean_SA <- sapply ( X_data_SA , mean , na.rm = T)

#the first time we need to creat the Results2 dataframe
Results2           <- data.frame(X_data_mean_SA) 
names(Results2)[1] <- names(X_data_raw[1])

#From here we repeat the prosses by ading one colum on Results2
for(i in 2:length(X_data_raw))
{
        X_data_SA      <- split  ( X_data_raw[[i]] ,splited_SA )
        X_data_mean_SA <- sapply ( X_data_SA , mean , na.rm = T)

        Results2[i]        <- X_data_mean_SA
        names(Results2)[i] <- names(X_data_raw[i])
}

#showing results
dim(Results2)
colnames(Results2)
head(Results2)
summary(Results2)

#...............Final results...............----
dim(Results1)
colnames(Results1)
head(Results1)
summary(Results1)
class(Results1)

dim(Results2)
colnames(Results2)
head(Results2)
tail(Results2)
summary(Results2)

library(foreign)
write.table(result, "./mydata.txt")


colnames(Results1$activity)<-c("ActivityCode","ActivityName")
write.table(data.frame(subset(Results1,select=-c(activity)),unclass(Results1$activity)),file = "Results1.txt")
write.table(Results2, file = "Results2.txt")

#...............end...............----
