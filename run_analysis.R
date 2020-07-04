title: "Getting and Cleaning Data Course Project"
author: "Kevin Roy"
date: "01/07/2020"
output: html_document

# Explanation of the data set can be found in the "README.txt".
 
# The variable headers can be explained as follows:
# t - time 
# f - frequency 
# acc - accelaration
# mag - magnetism
# gyro - orientation and angular velocity
# x,y,z represents movement along x, y and z axis
# For more details on variables, please read the "features_info.txt" and "README.txt" in data set

# Folder containing the data set from - "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 
# I moved the UCI HAR folder to my working directory and out of the zip file

# I created the following data frames from this file. x contains the data, y contains the activity performed (numbered 1 to 6) and z contains the subject number who performed the activity (numbered 1 to 30)

# I plan to combine these as the original data set was split into two data sets, with 70% of the data in the training dataset and 30% in the test dataset.In order to not mix up the data, I will combine the training data set above the test data set.

setwd("~/R/R Directory/John Hopkins/UCI HAR Dataset/test")
xtest <- read.delim("x_test.txt", sep = "", header =FALSE)
ytest <- read.delim("y_test.txt", sep = "", header = F)
subjecttest <- read.delim("subject_test.txt", header = F)



setwd("~/R/R Directory/John Hopkins/UCI HAR Dataset/train")
xtrain <- read.delim("x_train.txt", sep = "", header = FALSE)
ytrain <- read.delim("y_train.txt", sep = "", header = FALSE)
subjecttrain <- read.delim("subject_train.txt", header = FALSE)


# I combined the tasks data frame by row. 
# I alllowed this up by changing the numbers of the task to the verb describing the task being measured. I returned it back to a data frame after converting it to a vector to perform the function.

tasks <- rbind(ytrain, ytest)

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

# The variable headers can be explained as follows:
# t - time
# f - frequency
# acc - accelaration
# mag - magnetism
# gyro - orientation and angular velocity
# x,y,z represents movement along x, y and z axis
# For more details on variables, please read the "features info" in data set

#I therefore updated the headers to make it easier to read

variables$variables <- sub("^t", "time", variables$variables)
variables$variables <- sub("^f", "frequency", variables$variables)
variables$variables <- sub("[Aa]cc", "acceleration", variables$variables)
variables$variables <- sub("[Mm]ag", "magnitude", variables$variables)
variables$variables <- sub("[Gg]yro", "Gyroscope", variables$variables)


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
