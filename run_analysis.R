run_analysis <- function(){
    
    #load data
    X_train <- read.table("X_train.txt")
    y_train <- read.table("y_train.txt")
    subject_train <-read.table("subject_train.txt")
    X_test <- read.table("X_test.txt")
    y_test <- read.table("y_test.txt")
    subject_test <-read.table("subject_test.txt")
    
    #combine test and train sets 
    X <- rbind(X_test, X_train)
    y <- rbind(y_test, y_train)
    subject <- rbind(subject_test,subject_train)
    
    #load feature names
    features <- read.table("features.txt")[[2]]
    #load activity labels
    activity <- read.table("activity_labels.txt")
    
    # add headers
    colnames(X) <- features
    colnames(y) <- "Activity"
    colnames(subject) <- "Subject_id"
    colnames(activity) <-c("Code","Activity")
    
    
    #extracts only the measurements on the mean and standard deviation for each measurement 
    extract <- grep("mean|std", names(X))
    extracted <- X[extract]
    complete <- cbind(extracted, y, subject)
    
    #rename variables appropriately with descriptive names 
    newnames <- names(complete)
    newnames<- gsub("^f", "Frequency", newnames)
    newnames<- gsub("^t", "Time", newnames)
    newnames<- gsub("Acc", "Accelerometer", newnames)
    newnames<- gsub("Gyro", "Gyroscope", newnames)
    newnames<- gsub("Mag", "Magnitude", newnames)
    newnames<- gsub("BodyBody", "Body", newnames)
    newnames<- gsub("\\)", "", newnames)
    newnames<- gsub("\\(", "", newnames)
    newnames<- gsub("-", "_", newnames)
    names(complete) <- newnames
    
    #average of each variable for each activity and each subject.
    average_by_act_subj <- aggregate(complete, by=list(complete$Activity, complete$Subject_id), FUN = mean)
    
    
    #Uses descriptive activity names to name the activities in the data set
    average_by_act_subj$Activity <- factor(average_by_act_subj$Activity, levels=c(1,2,3,4,5,6), labels = activity$Activity)
    
    
    #remove old Activity and Subject_id columns
    average_by_act_subj$Group.1 <-NULL
    average_by_act_subj$Group.2 <-NULL
    
    #check dimension: there should be 180 rows (30 subjects* 6 activities) and 81 columns (79 features + Activity + Subject_id)
    dim(average_by_act_subj)
    
    #write file
    write.table(average_by_act_subj, file = "tidydata.txt", row.names=FALSE)

}
