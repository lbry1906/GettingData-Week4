library(dplyr)
# Download the file
if (!file.exists("./Week4/Week4Proj")) {
  dir.create("./Week4/Week4Proj")
}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./Week4/Week4Proj/Dataset.zip")
download <- date()

unzip(zipfile = "./Week4/Week4Proj/Dataset.zip", exdir = "./Week4/Week4Proj")

path <- file.path("./Week4/Week4Proj", "UCI HAR Dataset")
files <- list.files(path, recursive = T)

# Assign the files
features <- read.table("./Week4/Week4Proj/UCI HAR Dataset/features.txt",col.names = c("N","Functions"))
activities <- read.table("./Week4/Week4Proj/UCI HAR Dataset/activity_labels.txt",col.names = c("Code","Activity"))
subject_test <- read.table("./Week4/Week4Proj/UCI HAR Dataset/test/subject_test.txt",col.names = "Subject")
subject_train <- read.table("./Week4/Week4Proj/UCI HAR Dataset/train/subject_train.txt",col.names = "Subject")
x_test <- read.table("./Week4/Week4Proj/UCI HAR Dataset/test/x_test.txt", col.names = features$Functions)
y_test <- read.table("./Week4/Week4Proj/UCI HAR Dataset/test/y_test.txt",col.names = "Code")
x_train <- read.table("./Week4/Week4Proj/UCI HAR Dataset/train/X_train.txt",col.names = features$Functions)
y_train <- read.table("./Week4/Week4Proj/UCI HAR Dataset/train/y_train.txt",col.names = "Code")

# Merge the training and the test sets to create one data set
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subjcomb <- rbind(subject_train, subject_test)
merge <- cbind(subjcomb,y,x)

# Extract only the measurements on the mean and standard deviation
meanstdcols <- grep(".*[Mm]ean.*|.*[Ss]td.*", names(merge), ignore.case = T)
required <- c(1,2,meanstdcols)
extracted <- merge[,required]

# Use descriptive activity names to name the activities in the data set
extracted$Code <- activities[extracted$Code,2]

# Label the data set with descriptive variable names
names(extracted)[2] <- "Activity"
names(extracted) <- gsub("Acc", "Accelerometer", names(extracted))
names(extracted) <- gsub("Gyro", "Gyroscope", names(extracted))
names(extracted) <- gsub("BodyBody", "Body", names(extracted))
names(extracted) <- gsub("Mag", "Magnitude", names(extracted))
names(extracted) <- gsub("^t", "Time", names(extracted))
names(extracted) <- gsub("^f", "Frequency", names(extracted))
names(extracted) <- gsub("tbody|.tBody", "TimeBody", names(extracted))
names(extracted) <- gsub(".mean.*()", "Mean", names(extracted), ignore.case = T)
names(extracted) <- gsub(".std.*()", "STD", names(extracted), ignore.case = T)
names(extracted) <- gsub(".freq.*()", "Frequency", names(extracted), ignore.case = T)
names(extracted) <- gsub("angle", "Angle", names(extracted))
names(extracted) <- gsub("gravity", "Gravity", names(extracted))

# Create a tidy data set with the average of each variable for each activity and each subject
names(extracted) <- make.unique(names(extracted))
FinalData <- extracted %>% group_by(Subject, Activity) %>% summarize_all(funs(mean))

write.table(FinalData, "./Week4/Week4Proj/FinalData.txt", row.names = F)
