run_analysis <- function(){
  #Loading required datasets
  X_train <- read.table("../UCI HAR Dataset/train/X_train.txt", header = FALSE)
  Y_train <- read.table("../UCI HAR Dataset/train/Y_train.txt", header = FALSE)
  names(Y_train) <- "activity_id"
  X_test <- read.table("../UCI HAR Dataset/test/X_test.txt", header = FALSE)
  Y_test <- read.table("../UCI HAR Dataset/test/Y_test.txt", header = FALSE)
  names(Y_test) <- "activity_id"
  activities <- read.table("../UCI HAR Dataset/activity_labels.txt", header = FALSE)
  features <- read.table("../UCI HAR Dataset/features.txt", header = FALSE)
  subject_train <- read.table("../UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  subject_test <- read.table("../UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  
  # Merging Activities sets
  
  XY_train <- cbind(Y_train, X_train) #For training
  XY_test <- cbind(Y_test, X_test) # For test
  XY_train_f1 <- merge(XY_train, activities, by.x = "activity_id", by.y = "V1")
  XY_test_f1 <- merge(XY_test, activities, by.x = "activity_id", by.y = "V1")
  XY_train_f2 <- cbind(subject_train, XY_train_f1)
  XY_test_f2 <- cbind(subject_test, XY_test_f1)
  
  # binding headers
  
  tetes <- as.character(features$V2)
  tetes <- c("subjects", "activity_id", tetes, "activity")
  names(XY_train_f2) <- tetes
  names(XY_test_f2) <- tetes
  
  # Merging Training and test into on data set (Step 1)
  XY <- rbind(XY_train_f2, XY_test_f2)
  
  # Selecting from the merged dataframe only measurements on the mean and standard deviation: 
  # mean() and std()
  needed_vars <- grep(pattern = "[Mm]ean\\(\\)|[Ss]td\\(\\)", names(XY), value = TRUE)
  needed_vars <- c("subjects", "activity", needed_vars) # Step 3 Done !
  step_two <- XY[,needed_vars] # Step 2 Done!
  
  # Cleanning headers according to tidy principles
  a <- gsub(pattern = "\\(\\)|-", "", names(step_two))
  names(step_two) <- a
  library(dplyr)
  step_four <- tbl_df(step_two) # Step 4 Done!
  
  # Summarizing groups
  
  grouped_xy <- group_by(step_four, subjects, activity)
  step_five <- summarise_each(grouped_xy, funs(mean))
  names(step_five)[3:length(step_five)] <- paste0("Avg", names(step_five)[3:length(step_five)])
  
  # Delievering the tidy final data set: Step 5 Done !
  
  step_five
}