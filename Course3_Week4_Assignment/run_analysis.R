library(dplyr)
library(tidyr)

# Read in feature names for X
path <- '../UCI HAR Dataset/'
features <- readLines(paste0(path, 'features.txt'))

# Read in train data
path <- '../UCI HAR Dataset/train/'
subject_train <- read.table(paste0(path, 'subject_train.txt'))
y_train <- read.table(paste0(path, 'y_train.txt'))
X_train <- read.table(paste0(path, 'X_train.txt'))

# Create dataframe for train data
df_subject_train <- as.data.frame(subject_train)
names(df_subject_train) <- c('id')
df_y_train <- as.data.frame(y_train)
names(df_y_train) <- c('activity')
df_train <- cbind(df_subject_train, df_y_train)
df_X_train <- as.data.frame(X_train)
names(df_X_train) <- features
df_train <- cbind(df_train, df_X_train)

# Read in test data
path <- '../UCI HAR Dataset/test/'
subject_test <- read.table(paste0(path, 'subject_test.txt'))
y_test <- read.table(paste0(path, 'y_test.txt'))
X_test <- read.table(paste0(path, 'X_test.txt'))

# Create dataframe for test data
df_subject_test <- as.data.frame(subject_test)
names(df_subject_test) <- c('id')
df_y_test <- as.data.frame(y_test)
names(df_y_test) <- c('activity')
df_test <- cbind(df_subject_test, df_y_test)
df_X_test <- as.data.frame(X_test)
names(df_X_test) <- features
df_test <- cbind(df_test, df_X_test)

# 1. Combine dataframes for train and test
df <- rbind(df_train, df_test)

# 2. Extract measurements of mean and std

meanColumns <- grepl('mean\\(\\)', names(df))
stdColumns <- grepl('std\\(\\)', names(df))

selectedColumns <- (meanColumns | stdColumns)
selectedColumns[1:2] <- TRUE
columnNames <- names(df)
selectedColumnNames <- columnNames[selectedColumns]
df <- df[, selectedColumnNames]

# 3. Rename activities
df$activity <- as.factor(df$activity)
levels(df$activity) <- c('walking', 'walkingupstairs', 'walkingdownstairs', 
                            'sitting', 'standing', 'laying')

# 4. Relabel variables for measurements of mean and std
newNames <- gsub("([0-9])+ ", "", names(df))
newNames <- gsub("^f", "FourierTransform-", newNames)
newNames <- gsub("^t", "", newNames) 
newNames <- gsub("\\(\\)", "", newNames)
newNames <- gsub("mean", "Mean", newNames)
newNames <- gsub("std", "StandardDeviation", newNames)
newNames <- gsub("-X", "_X-Axis", newNames)
newNames <- gsub("-Y", "_Y-Axis", newNames)
newNames <- gsub("-Z", "_Z-Axis", newNames)
names(df) <- newNames

# 5. Create average of each variable for each activity and each subject
dfSummary <- df %>% group_by(id, activity) %>% summarise_all(funs(mean))

write.csv(dfSummary, file = "MeansByIdAndActivity.csv", row.names = FALSE)