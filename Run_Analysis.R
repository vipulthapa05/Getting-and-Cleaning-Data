#run_analysis.R
library(plyr)

if (!file.exists("./data")) {
  dir.create("./data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
localFile <- file.path("./data" , "dataset.zip")
dataDir <- file.path("./data" , "UCI HAR Dataset")
# load file to local filesystem
if (!file.exists(localFile)) {
  download.file(fileUrl, destfile = localFile ,method="curl");
}

#unzip if not already unzipped
if (!file.exists(dataDir)) {
  unzip(localFile,exdir='./data')  
}

# load all data
activity <- read.table(file.path(dataDir,"activity_labels.txt"), header=FALSE, stringsAsFactors=FALSE)
x_train <- read.table(file.path(dataDir,"train", "X_train.txt"), header=FALSE, stringsAsFactors=FALSE)
x_test <- read.table(file.path(dataDir,"test", "X_test.txt"), header=FALSE, stringsAsFactors=FALSE)
y_train <- read.table(file.path(dataDir,"train", "Y_train.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("activity"))
y_test <- read.table(file.path(dataDir,"test", "Y_test.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("activity"))
subject_train <- read.table(file.path(dataDir,"train", "subject_train.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("subjectid"))
subject_test <- read.table(file.path(dataDir,"test", "subject_test.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("subjectid"))
featureNames <- read.table(file.path(dataDir,"features.txt"), header=FALSE,stringsAsFactors=FALSE)

# merge data
train<-cbind(y_train,subject_train,x_train)
test<-cbind(y_test,subject_test,x_test)
data<-rbind(train,test)

#extracting mean and std data
measure<-data[,c(c(1:2),grep(".*mean|std.*",featureNames$V2)+2)]
names<-grep(".*mean|std.*",featureNames$V2,value=TRUE)

#naming activities
measure$activity<-as.factor(measure$activity)
levels(measure$activity)<-activity$V2

#naming columns
names(measure)<-c("activity","subjectid", names)

#creating tidy dataset
tidy<-ddply(measure, c(.(activity),.(subjectid)),summarize,Means=colMeans(measure[3:(length(names(measure)))]))
activitynames<-rep( names(measure[,3:(length(names(measure)))]),nrow(activity)*length(c(unique(subject_train$subjectid),unique(subject_test$subjectid))))
tidy$measuredfeature<-activitynames
write.table(tidy, "tidy.txt",row.names=FALSE)
