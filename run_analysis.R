# Step 1: Combine the Test and Train sets to create one
# I will assume you have collected your data files into
# one folder and set it as your working directory in order
# to expedite this step

# Reads all files in and then combines the y and subject data
# with the x data for both train and test cases, then
# row binds the test data to the bottom of the train data
# This is because they have corresponding variables and
# We are just trying to get info on all observations taken
# (At least as far as my understanding of this assignment goes)

trainx <- read.table("X_train.txt")
trainy <- read.table("y_train.txt")
trainsubject <- read.table("subject_train.txt")
testx <- read.table("X_test.txt")
testy <- read.table("y_test.txt")
testsubject <- read.table("subject_test.txt")
dftest <- cbind(testsubject,testy,testx)
dftrain <- cbind(trainsubject,trainy,trainx)
dfall <- rbind(dftrain,dftest)


# Cleanup
rm(list=c('dftest','dftrain', 'testsubject','testx','testy','trainsubject'
          ,'trainx','trainy'))

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement

# Grepping the "features.txt" file to find all mean and std rows, then added 2 to each
# account for the two additional variables I have added. 
features <- read.table('features.txt')
rows <- grep('mean\\(\\)|std', features$V2)
dfall <- dfall[,c(1,2,rows+2)]

# Step 3: Uses descriptive activity names to name the activities in the data set

# Uses gsub for each activity type and replaces it with it's
# corresponding activity

gsub('1', 'WALKING', dfall$V1.1) -> dfall$V1.1
gsub('2', 'WALKING_UPSTAIRS', dfall$V1.1) -> dfall$V1.1
gsub('3', 'WALKING_DOWNSTAIRS', dfall $V1.1) -> dfall$V1.1
gsub('4', 'SITTING', dfall$V1.1) -> dfall$V1.1
gsub('5', 'STANDING', dfall$V1.1) -> dfall$V1.1
gsub('6', 'LAYING', dfall$V1.1) -> dfall$V1.1

# Step 4: Appropriately labels the data set with descriptive variable names

# Uses the rows we had previous calculated using grep to pull labels from
# features file, turns them from factor to character then
# applies them along with "Subject" and "Activity" to the data frame

labeldf <- as.character(features$V2[rows])
names(dfall) <- c("Subject","Activity", labeldf)

# Step 5: From the data set in step 4, creates a second, 
# independent tidy data set with the average of each 
# variable for each activity and each subject.

#Setup: Create a character vector for each of the activity types
# and empty new dataframe

activities <- factor(c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS',
                'SITTING','STANDING','LAYING'))
dffinal <- data.frame(NULL)

# We need to do this for all 30 subjects so we'll use a loop
# to iterate easily through each of them.
for(i in 1:30){
  # And again for each of the activities
  for(j in activities){
    #subset each column, calculate each one's mean with apply
    #then bind it to the new data frame.
    row <- unname(sapply(subset(dfall, dfall$Subject == i & dfall$Activity == j)[,c(-1,-2)], 
                          mean, FUN.VALUE=1))
    dffinal <- rbind(dffinal, row)
  }
}
# Adding the missing columns...
dffinal <- cbind(rep(1:30, each=6),rep(activities,30),dffinal)
# Renaming cols for clarity
names(dffinal) <- c("Subject","Activity", labeldf) 

# Writing to a file
write.table(dffinal, file = "dffinal.rds", row.name=FALSE)

