run <- function()
{
  library(dplyr)
  
  #TRAIN DATA-----------------------------------------------------------------------------
  
  #get subjects
  subjects_train<-readLines("UCI HAR Dataset/train/subject_train.txt")
  subjects_train<-data.frame(t(data.frame(lapply(subjects_train, type.convert), stringsAsFactors=FALSE)))

  #sequence of ids for further use
  sequence<-seq(1,nrow(subjects_train),1)
  
  #subjects with id+subject
  subjects_train<-data.frame(cbind(sequence,subjects_train))
  
  #get train X
  x_train<-as.data.frame(read.table("UCI HAR Dataset/train/X_train.txt"))
  #train x with id
  x_train<-data.frame(cbind(sequence,x_train))
  
  #get train Y
  y_train<-as.data.frame(read.table("UCI HAR Dataset/train/Y_train.txt"))
  #train y with id
  y_train<-data.frame(cbind(sequence,y_train))
  
  #names of features
  names(subjects_train)<-c("id","subject")
  
  x_names<-readLines("UCI HAR Dataset/features.txt")
  names(x_train)<-c("id",x_names)
  
  names(y_train)<-c("id","activity")
    
  #merge 
  train<-merge(subjects_train,x_train,by.x="id",by.y="id")
  train<-merge(train,y_train,by.x="id",by.y="id")
  
  
  #TEST DATA-------------------------------------------------------------
  
  #get subjects
  subjects_test<-readLines("UCI HAR Dataset/test/subject_test.txt")
  subjects_test<-data.frame(t(data.frame(lapply(subjects_test, type.convert), stringsAsFactors=FALSE)))
  
  #sequence of ids for further use
  sequence<-seq(1,nrow(subjects_test),1)
  
  #subjects with id+subject
  subjects_test<-data.frame(cbind(sequence,subjects_test))
  
  #get train X
  x_test<-as.data.frame(read.table("UCI HAR Dataset/test/X_test.txt"))
  #train x with id
  x_test<-data.frame(cbind(sequence,x_test))
  
  #get train Y
  y_test<-as.data.frame(read.table("UCI HAR Dataset/test/Y_test.txt"))
  #train y with id
  y_test<-data.frame(cbind(sequence,y_test))
  
  #names of features
  names(subjects_test)<-c("id","subject")
  
  names(x_test)<-c("id",x_names)
  
  names(y_test)<-c("id","activity")
  
  
  #merge 
  test<-merge(subjects_test,x_test,by.x="id",by.y="id")
  test<-merge(test,y_test,by.x="id",by.y="id")
  
  
  #STEP 1: MERGE TRAIN AND TEST DATA-------------------------------------------------------------
  completeData<-rbind(train,test)
  
  #STEP2: EXTRACT ONLY MEAN AND STDV-------------------------------------------------------------
  idsSubjects<-completeData[,1:2]
  tBodyAccXYZ<-completeData[,3:8]
  tGravityAccXYZ<-completeData[,43:48]
  tBodyAccJerkXYZ<-completeData[,83:88]
  tBodyGyroXYZ<-completeData[,123:128]
  tBodyGyroJerkXYZ<-completeData[,163:168]
  tBodyAccMag<-completeData[,203:204]
  tGravityAccMag<-completeData[,216:217]
  tBodyAccJerkMag<-completeData[,229:230]
  tBodyGyroMag<-completeData[,242:243]
  tBodyGyroJerkMag<-completeData[,255:256]
  fBodyAccXYZ<-completeData[,268:273]
  fBodyAccJerkXYZ<-completeData[,347:352]
  fBodyGyroXYZ<-completeData[,426:431]
  fBodyAccMag<-completeData[,505:506]
  fBodyAccJerkMag<-completeData[,518:519]
  fBodyGyroMag<-completeData[,531:532]
  fBodyGyroJerkMa<-completeData[,544:545]
  activities<-completeData[,564]
  
  resumedData<-cbind(idsSubjects,tBodyAccXYZ, tGravityAccXYZ,tBodyAccJerkXYZ,tBodyGyroXYZ,tBodyGyroJerkXYZ,tBodyAccMag,tGravityAccMag,tBodyAccJerkMag,tBodyGyroMag,tBodyGyroJerkMag, fBodyAccXYZ,fBodyAccJerkXYZ,fBodyGyroXYZ, fBodyAccMag, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMa, activities)
  
  #STEP3: RENAME ACTIVITIES-------------------------------------------------------------
  resumedData$activities[resumedData$activities==1]<-"WALKING"
  resumedData$activities[resumedData$activities==2]<-"WALKING_UPSTAIRS"
  resumedData$activities[resumedData$activities==3]<-"WALKING_DOWNSTAIRS"
  resumedData$activities[resumedData$activities==4]<-"SITTING"
  resumedData$activities[resumedData$activities==5]<-"STANDING"
  resumedData$activities[resumedData$activities==6]<-"LAYING"
  

  #STEP4: APPROPIATE LABELING-------------------------------------------------------
  namesArray<-c("id","subject")
  
  for(i in 3:68)
  {
    namesArray<-c(namesArray,substr(names(resumedData)[i],regexpr(" ",names(resumedData)[i])[1]+1,nchar(names(resumedData)[i])))
  }
  
  namesArray<-c(namesArray,"activity")
  namesArray<-gsub("\\(\\)", "", namesArray)
  namesArray<-gsub("-", "_", namesArray)
  
  names(resumedData)<-namesArray
  
  #STEP5: TIDY DATASET
  groupedData<-group_by(resumedData, subject, activity)
  summarizedData<-summarise(groupedData, tBodyAcc_mean_X=mean(tBodyAcc_mean_X), tBodyAcc_mean_Y=mean(tBodyAcc_mean_Y), tBodyAcc_mean_Z=mean(tBodyAcc_mean_Z), tBodyAcc_std_X=mean(tBodyAcc_std_X), tBodyAcc_std_Y=mean(tBodyAcc_std_Y), tBodyAcc_std_Z=mean(tBodyAcc_std_Z), tGravityAcc_mean_X=mean(tGravityAcc_mean_X), tGravityAcc_mean_Y=mean(tGravityAcc_mean_Y), tGravityAcc_mean_Z=mean(tGravityAcc_mean_Z), tGravityAcc_std_X=mean(tGravityAcc_std_X), tGravityAcc_std_Y=mean(tGravityAcc_std_Y), tGravityAcc_std_Z=mean(tGravityAcc_std_Z), tBodyAccJerk_mean_X=mean(tBodyAccJerk_mean_X), tBodyAccJerk_mean_Y=mean(tBodyAccJerk_mean_Y), tBodyAccJerk_mean_Z=mean(tBodyAccJerk_mean_Z), tBodyAccJerk_std_X=mean(tBodyAccJerk_std_X), tBodyAccJerk_std_Y=mean(tBodyAccJerk_std_Y), tBodyAccJerk_std_Z=mean(tBodyAccJerk_std_Z), tBodyGyro_mean_X=mean(tBodyGyro_mean_X), tBodyGyro_mean_Y=mean(tBodyGyro_mean_Y), tBodyGyro_mean_Z=mean(tBodyGyro_mean_Z), tBodyGyro_std_X=mean(tBodyGyro_std_X), tBodyGyro_std_Y=mean(tBodyGyro_std_Y), tBodyGyro_std_Z=mean(tBodyGyro_std_Z), tBodyGyroJerk_mean_X=mean(tBodyGyroJerk_mean_X), tBodyGyroJerk_mean_Y=mean(tBodyGyroJerk_mean_Y), tBodyGyroJerk_mean_Z=mean(tBodyGyroJerk_mean_Z), tBodyGyroJerk_std_X=mean(tBodyGyroJerk_std_X), tBodyGyroJerk_std_Y=mean(tBodyGyroJerk_std_Y), tBodyGyroJerk_std_Z=mean(tBodyGyroJerk_std_Z), tBodyAccMag_mean=mean(tBodyAccMag_mean), tBodyAccMag_std=mean(tBodyAccMag_std), tGravityAccMag_mean=mean(tGravityAccMag_mean), tGravityAccMag_std=mean(tGravityAccMag_std), tBodyAccJerkMag_mean=mean(tBodyAccJerkMag_mean), tBodyAccJerkMag_std=mean(tBodyAccJerkMag_std), tBodyGyroMag_mean=mean(tBodyGyroMag_mean), tBodyGyroMag_std=mean(tBodyGyroMag_std), tBodyGyroJerkMag_mean=mean(tBodyGyroJerkMag_mean), tBodyGyroJerkMag_std=mean(tBodyGyroJerkMag_std), fBodyAcc_mean_X=mean(fBodyAcc_mean_X), fBodyAcc_mean_Y=mean(fBodyAcc_mean_Y), fBodyAcc_mean_Z=mean(fBodyAcc_mean_Z), fBodyAcc_std_X=mean(fBodyAcc_std_X), fBodyAcc_std_Y=mean(fBodyAcc_std_Y), fBodyAcc_std_Z=mean(fBodyAcc_std_Z), fBodyAccJerk_mean_X=mean(fBodyAccJerk_mean_X), fBodyAccJerk_mean_Y=mean(fBodyAccJerk_mean_Y), fBodyAccJerk_mean_Z=mean(fBodyAccJerk_mean_Z), fBodyAccJerk_std_X=mean(fBodyAccJerk_std_X), fBodyAccJerk_std_Y=mean(fBodyAccJerk_std_Y), fBodyAccJerk_std_Z=mean(fBodyAccJerk_std_Z), fBodyGyro_mean_X=mean(fBodyGyro_mean_X), fBodyGyro_mean_Y=mean(fBodyGyro_mean_Y), fBodyGyro_mean_Z=mean(fBodyGyro_mean_Z), fBodyGyro_std_X=mean(fBodyGyro_std_X), fBodyGyro_std_Y=mean(fBodyGyro_std_Y), fBodyGyro_std_Z=mean(fBodyGyro_std_Z), fBodyAccMag_mean=mean(fBodyAccMag_mean), fBodyAccMag_std=mean(fBodyAccMag_std), fBodyBodyAccJerkMag_mean=mean(fBodyBodyAccJerkMag_mean), fBodyBodyAccJerkMag_std=mean(fBodyBodyAccJerkMag_std), fBodyBodyGyroMag_mean=mean(fBodyBodyGyroMag_mean), fBodyBodyGyroMag_std=mean(fBodyBodyGyroMag_std), fBodyBodyGyroJerkMag_mean=mean(fBodyBodyGyroJerkMag_mean), fBodyBodyGyroJerkMag_std=mean(fBodyBodyGyroJerkMag_std))
  
  names(summarizedData)[1]<-"subject"
  names(summarizedData)[2]<-"activity"
  
  write.table(summarizedData,"tidyDataSet.txt",sep=",", row.names=FALSE)
  summarizedData
}