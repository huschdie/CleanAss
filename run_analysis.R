# function run_analysis.R returns the tidy data, if you have the original .zip-Samsung file in your working directory.

run_analysis<- function() {

        library(plyr);library(dplyr)
        
        # get the directory to work in
        dir <- getwd()
        
        # create a temporary directory to extract there the result
        td <- tempdir()
        unlink(td, recursive=TRUE) #delete the directory and create again in case there was something initially.
        td <- tempdir()
        
        # unzip the raw data in the temporary directory
        link<-paste(getwd(),"getdata-projectfiles-UCI HAR Dataset.zip",sep="/")
        unzip(link, ex = td)
        
        # open all the needed tables in .txt format
        to_open2<- list.dirs(td)
        
        dir1 <- to_open2[2]
        list_files_1 <- list.files(dir1,".txt")
        features <- read.table(paste(dir1,"features.txt",sep="/"))
        activity.labels <-read.table(paste(dir1,"activity_labels.txt",sep="/"))
        
        dir2<-to_open2[3]
        list_files_2 <- list.files(dir2,".txt")
        subject_test <- read.table(paste(dir2,"subject_test.txt",sep="/"))
        X_test <- read.table(paste(dir2,"X_test.txt",sep="/"))
        y_test <- read.table(paste(dir2,"y_test.txt",sep="/"))
        
        
        dir3 <- to_open2[5]
        list_files_3 <- list.files(dir3,".txt")
        subject_train <- read.table(paste(dir3,"subject_train.txt",sep="/"))
        X_train <- read.table(paste(dir3,"X_train.txt",sep="/"))
        y_train <- read.table(paste(dir3,"y_train.txt",sep="/"))
        
        # delete the temporary directory
        unlink(td, recursive=TRUE)
        
        # Solving questions 1, 3
                # Merging 3 sub databases. Total merge when solving question 4.
        
                # Merge training X and test X
                total.X <- rbind(X_train,X_test) 
                # Merge training Y and test Y
                total.Y <- rbind(y_train, y_test)
                
                # assign activity names to all the Y values (question 3)
                activities <- unique(total.Y[order(total.Y[,1]),])
                for (i in 1:length(activities)) {
                        
                        total.Y[total.Y==activities[i],] <- as.character(activity.labels[i,2])
                }
                
                # Merge training subject and test subject
                total.subject <- rbind(subject_train, subject_test)
        
        # Solving question 2       
                # Extract only observations that contain 'mean' or 'std'
                rel.names <- rbind(features[grepl("mean", features[,2]),],features[grepl("std", features[,2]),])
                rel.names <- rel.names[order(rel.names[,1]),]
                # X-databse with only mean and std observations
                mean.std.only <- total.X[,rel.names[,1]]
        
        
        # Solving question 4
                # Assign column names to all parts of the future big database.
        
                names(mean.std.only)<-rel.names[,2] # the names in the features file are descriptive enough as discussed on the forum.
                names(total.subject)<-"Subject"
                names(total.Y)<-"activity"
                
                #Merge all the data in one big table
                raw.table.4 <- data.frame(total.subject,total.Y,mean.std.only)
                
        # Solving question 5
                # Creating tidy data
                
                n_subj <- order(unique(total.subject))
                # order the raw data by activity and subject
                tab5<-raw.table.4[order(raw.table.4$activity,raw.table.4$Subject),]
                # Group by activity and Subject
                by_Y_sub <- tab5 %>% group_by(activity,Subject)
        
                # Summarize each using mean as the summarizing function
                result<-by_Y_sub %>% summarise_each(funs(mean))

}



