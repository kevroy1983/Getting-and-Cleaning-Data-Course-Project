title: "Getting and Cleaning Data Course Project"
author: "Kevin Roy"
date: "01/07/2020"
output: html_document

#Download file
"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


setwd("~/R/R Directory/John Hopkins/UCI HAR Dataset/test")
xtest <- read.delim("x_test.txt", sep = "", header =FALSE)
ytest <- read.delim("y_test.txt", sep = "", header = F)
subjecttest <- read.delim("subject_test.txt", header = F)


setwd("~/R/R Directory/John Hopkins/UCI HAR Dataset/train")
xtrain <- read.delim("x_train.txt", sep = "", header = FALSE)
ytrain <- read.delim("y_train.txt", sep = "", header = FALSE)
subjecttrain <- read.delim("subject_train.txt", header = FALSE)

#Combine data sets
tasks <- rbind(ytrain, ytest)

#rename variables
tasks <- as.vector(tasks$V1)
tasks <- mapvalues(tasks, from = 1:6, to = c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

tasks <- as.data.frame(tasks)

# I combined the subjects from the two data sets and renamed the header to "subject"

all_subjects <- rbind(subjecttrain, subjecttest)

all_subjects <- rename(all_subjects, subject = V1)


# I read in the following text document as it contains all the  header for a data set of "x_train.txt" and "x_test.txt"

setwd("~/R/R Directory/John Hopkins/UCI HAR Dataset")

variables <- read.delim("features.txt", header = FALSE)

# I split the character vector into 2 parts. I kept the second element of it's name and removed the first part

splitNames <- strsplit(variables$V1," " )
splitNames[[1]][2]

secondElement <- function(x){x[2]}
variables <- sapply(splitNames,secondElement)

# The above function change the data frame into a character vector, so I returned it to a data frame

variables <- as.data.frame(variables)

#I updated the header names to make it easier to read

variables$variables <- sub("^t", "Time", variables$variables)
variables$variables <- sub("^f", "Frequency", variables$variables)
variables$variables <- sub("[Aa]cc", "Accelerometer", variables$variables)
variables$variables <- sub("[Mm]ag", "Magnitude", variables$variables)
variables$variables <- sub("[Gg]yro", "Gyroscope", variables$variables)
variables$variables <- sub("jerk", "Jerk", variables$variables)
variables$variable <- sub("tBody", "TimeBody", variables$variables)
variables$variables <- gsub("gravity", "Gravity", variables$variables)
variables$variables <- sub("BodyBody", "Body", variables$variables)
variables$variables <- gsub("mean", "Mean", variables$variables)
variables$variables <- gsub("std", "Std", variables$variables)
variables$variables <- gsub("freq", "Freq", variables$variables)
variables$variables <- gsub("[[:punct:]]", "", variables$variables, perl = TRUE)


# I combined the x data sets together, which contains all the data values and attached the column names from the variables data set.

totalresults <- rbind(xtrain,xtest)
colnames(totalresults) <- variables$variables

# I only want the data containing the mean and the standard deviation. I therefore selected the columns that had the name "mean" and "std" in the header

totalresults<- select(totalresults, grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]", names(totalresults), value = TRUE))

# I then combined these three data frames I have created.

humanactivityrecognition <- cbind(all_subjects, tasks, totalresults)


# Lastly I took the average of each column based on the subject and task, then ordered them by task and subject.

averagehumanactivityrecognition <- humanactivityrecognition %>% 
      group_by(subject, tasks) %>% 
      summarise_all(list(mean)) %>% 
      arrange(tasks, subject)

