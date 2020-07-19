run_analysis<-function(){
  
# setting path for various files in working directory
  
  pathwd<-getwd()
  pathX_train<-paste(pathwd,"/X_train.txt",sep="")
  pathsubject_train<-paste(pathwd,"/subject_train.txt",sep="")
  pathy_train<-paste(pathwd,"/y_train.txt",sep="")
  pathX_test<-paste(pathwd,"/X_test.txt",sep="")
  pathsubject_test<-paste(pathwd,"/subject_test.txt",sep="")
  pathy_test<-paste(pathwd,"/y_test.txt",sep="")
  pathfeatures<-paste(pathwd,"/features.txt",sep="")
  pathactivity_labels<-paste(pathwd,"/activity_labels.txt",sep="")
  
  #reading all data files basis the path established above
  
  trainreading<-read.table(pathX_train,header = FALSE)
  trainsubject<-read.table(pathsubject_train,header = FALSE)
  trainactivities<-read.table(pathy_train,header = FALSE)
  testreading<-read.table(pathX_test,header = FALSE)
  testsubject<-read.table(pathsubject_test,header = FALSE)
  testactivities<-read.table(pathy_test,header = FALSE)
  featuredata<-read.table(pathfeatures,header = FALSE)
  activitycode<-read.table(pathactivity_labels,header = FALSE)
  
  
  # Giving a column name to the subject data in Train and Test data sets
 names(trainsubject)<-"Subject"
 names(testsubject)<-"Subject"
 
 # Giving a column name to the Activity data in Train and Test data sets
 names(trainactivities)<-"Activity"
 names(testactivities)<-"Activity"
 
 # Giving a column name to Features data
 names(featuredata)<-c("SNO","feature")
 
 # Populating feautures data as column names in Train and Test data measurements
 names(trainreading)<-featuredata[,"feature"]
 names(testreading)<-featuredata[,"feature"]
 
 # combining Subject and Activities with measurements for Train and Test data
  traindata<-cbind(trainsubject,trainactivities,trainreading)
  testdata<-cbind(testsubject,testactivities,testreading)
  
  # combining all data 
  dataAll<-rbind(traindata,testdata)
  
  #Finding columns having mean and std
  meancolnumbers<-grep("mean",names(dataAll))
  stdcolnumbers<-grep("std",names(dataAll))
  
  #Selecting only columns having mean and std along with Subject and Activity
  dataAll<-select(dataAll,"Subject","Activity",all_of(meancolnumbers),all_of(stdcolnumbers))
  
  #Extracting activity labels
  activitycode1<-activitycode[1,2];activitycode2<-activitycode[2,2]
  activitycode3<-activitycode[3,2];activitycode4<-activitycode[4,2]
  activitycode5<-activitycode[5,2];activitycode6<-activitycode[6,2]
  
  #Replacing codes 1-6 in activity column with corresponding labels
  dataAll[,"Activity"][dataAll[,"Activity"]==1]<-activitycode1
  dataAll[,"Activity"][dataAll[,"Activity"]==2]<-activitycode2
  dataAll[,"Activity"][dataAll[,"Activity"]==3]<-activitycode3
  dataAll[,"Activity"][dataAll[,"Activity"]==4]<-activitycode4
  dataAll[,"Activity"][dataAll[,"Activity"]==5]<-activitycode5
  dataAll[,"Activity"][dataAll[,"Activity"]==6]<-activitycode6
  
  
#Used "group_by_" which is counterpart of "group_by" for programming
#grouped the data by subject and activity
  
  
  grpby<-group_by_(dataAll,"Subject","Activity")
  
 # Summarized the data by mean and returned it
  
 seconddata<-summarize_all(grpby,list(mean))
  
 seconddata
  
  
}