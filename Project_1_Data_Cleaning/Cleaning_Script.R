### Before we start writing the script here are some info on extactly what are we trying to do with our this data set
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#The "Job Description" (The GitHub Requirements)
#You asked where the "official" rules are. For this specific project, they are found in the README.md file of the Coursera/Johns Hopkins "Getting and Cleaning Data" repository.
#Here is your exact "To-Do" list:
#Merge the training and the test sets.
#Extract only the measurements on the mean and standard deviation (This is what we do next)
#Use descriptive activity names to name the activities (Changing numbers 1-6 into words).
#Label the data set with descriptive variable names (Cleaning up the column titles).
#Create a second, tidy data set with the average of each variable for each activity and each subject.
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# A. Load the necessary library
library(dplyr)
#----------------------------
# Assigning the list of 561 names to a variable called 'variable_names'
variable_names <- read.table("./UCI HAR Dataset/features.txt")

# NOW you can run this to see the first few names
head(variable_names)

# Load the 'X' measurements for the training group
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

# Load the 'Y' activity labels (the numbers 1-6)
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")

# Load the 'Subject' IDs (the person 1-30)
Sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# 1. Load the dictionary
variable_names <- read.table("./UCI HAR Dataset/features.txt")

# 2. Load the raw data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

# 3. Look at the match
nrow(variable_names)  # This will show 561
ncol(X_train)         # This will also show 561
#--------------------------------------------------
# 1. Load the Test Measurements (X)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

# 2. Load the Test Activity Labels (Y)
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")

# 3. Load the Test Subject IDs (People)
Sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")


# Merge the Measurements (X)
X_total <- rbind(X_train, X_test)

# Merge the Activities (Y)
Y_total <- rbind(Y_train, Y_test)

# Merge the Subjects (People)
Sub_total <- rbind(Sub_train, Sub_test)

# Check the final count of rows
nrow(X_total)

## STARTING REQ.02------------------------------------------------

# 1. Identify which rows in the dictionary have 'mean()' or 'std()'
# variable_names[, 2] is the column containing the actual names
target_columns <- grep("mean\\(\\)|std\\(\\)", variable_names[, 2])

# 2. Extract only those columns from your big X_total
X_filtered <- X_total[, target_columns]

# 3. Give those 66 columns their professional names immediately
names(X_filtered) <- variable_names[target_columns, 2]


# 4. Verify the reduction
ncol(X_filtered) # Should show 66

#-----------------REQ.03------------------------------
# Load the translation table
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Look at it
activity_labels

# 1. Replace the number with the word
# We are looking at the 2nd column of activity_labels (the words)
# and matching them to the numbers in Y_total
Y_total[, 1] <- activity_labels[Y_total[, 1], 2]

# 2. Give the column a human-readable name
names(Y_total) <- "Activity"
# 3. Check the result
head(Y_total)

# 1. Map the numbers to the words
# We use the numbers in Y_total as an index to pick the word from activity_labels
Y_total[, 1] <- activity_labels[Y_total[, 1], 2]

# 2. Give the column a professional name
names(Y_total) <- "Activity"

# 3. Peek at the result to see if it worked
head(Y_total, 10)

# had an error because Mistakenly ran a code twice which changed the desired values to NA
# Reloading the original numbers from the files
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")

# Merge them again
Y_total <- rbind(Y_train, Y_test)


# Now translate the numbers to words using the labels
Y_total[, 1] <- activity_labels[Y_total[, 1], 2]

# Rename the column
names(Y_total) <- "Activity"

head(Y_total, 10)

# 1. Glue the Subject, Activity, and Measurements side-by-side
final_dataset <- cbind(Sub_total, Y_total, X_filtered)

# 2. Rename the first column (currently V1) to "Subject"
names(final_dataset)[1] <- "Subject"

# 3. Look at the dimensions and a preview
dim(final_dataset)
head(final_dataset[, 1:5])


#-----------------REQ.05------------------

# 1. Use pipe operators (%>%) to group and average the data
tidy_data <- final_dataset %>%
  group_by(Subject, Activity) %>%
  summarise_all(list(mean = mean))

# 2. Check the dimensions
dim(tidy_data)

# 3. Save your hard work to a file!
write.table(tidy_data, "TidyData.txt", row.name=FALSE)


# Read the file you just saved
check_tidy <- read.table("TidyData.txt", header = TRUE)

# View the first few rows and columns
head(check_tidy[, 1:5])














