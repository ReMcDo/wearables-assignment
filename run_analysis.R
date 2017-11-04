
## GETTING AND CLEANING DATA, WEEK 4 PROGRAMMING ASSIGNMENT

library(dplyr) # Because I <3 Hadley
library(readr)
library(tidyr)
library(magrittr)

## STEP 1: MERGE TRAINING AND TEST SETS 

## Make a nice home for the data 
if(!file.exists("./data")){dir.create("./data")}

## Download data 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/wearables.zip")

## Unzip it and put it in the same home as the zipped file
unzip("./data/wearables.zip",exdir="./data")

## Grab labels files
labs <- read_table("./data/UCI HAR Dataset/activity_labels.txt",col_names=c("activitynumber","activity")) # Activity labels to go with the ytrain and ytest 
feat <- read.table("./data/UCI HAR Dataset/features.txt",stringsAsFactors = F) # Features labels (column names) to go with xtrain and xtest

## Grab training data (assuming we are to ignore Inertial Signals)
ytrain <- read_table("./data/UCI HAR Dataset/train/y_train.txt",col_names="activitynumber") # Activity Labels
xtrain <- read_table("./data/UCI HAR Dataset/train/x_train.txt",col_names=feat[,2]) # Data; use features labels for colnames
subtrain <- read_table("./data/UCI HAR Dataset/train/subject_train.txt",col_names="subject") # People ID

## Grab test data: Need to grab same as above, but with test. Smaller dataset. 
ytest <- read_table("./data/UCI HAR Dataset/test/y_test.txt",col_names="activitynumber") # Activity Labels
xtest <- read_table("./data/UCI HAR Dataset/test/X_test.txt",col_names=feat[,2]) # Data
subtest <- read_table("./data/UCI HAR Dataset/test/subject_test.txt",col_names="subject") # People ID

## Bind columns to create a training set and test set, each with subject,activity number, and data, then bind rows to create one dataset
together <- bind_rows(bind_cols(subtrain,ytrain,xtrain),bind_cols(subtest,ytest,xtest))

## STEP 2: EXTRACT MEAN AND STANDARD DEVIATION
## Assuming we want  mean() and std() for each variable only, not meanFreq, gravityMean, etc
extracted <- select(together,subject,activitynumber,matches("mean\\(\\)|std\\(\\)"))

## STEP 3: MAKE DESCRIPTIVE ACTIVITY NAMES
described <- right_join(labs,extracted,by="activitynumber") %>% # join activity labels onto extracted dataset by activity number
  select(-activitynumber)  # drop the numbered activity column

## STEP 4: DESCRIPTIVE VARIABLE NAMES
# Done in Step 1 with read_table

## STEP 5: NEW TIDY DATASET WITH MEAN OF EACH VARIABLE FOR EACH ACTIVITY/SUBJECT COMBINATION
groupedmeans <- group_by(described,subject,activity) %>%
  summarize_all(mean,na.rm=T)

write.table(groupedmeans,"tidywear.txt",row.name=F)





