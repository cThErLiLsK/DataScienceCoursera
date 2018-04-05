## Codebook

### Data sources for the original data

The original data for this assignment is available at https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

For details on the data file contained in the zip folder see the text files which are also included. 

### Transformation on the data

There were four major transformation steps performed on the data set:

1. The training and the test sets were loaded and merged. 
2. Only columns that include the mean and standard deviation valuation for each measurement were selected.
3. Activities and variables were relabeled / renamed for easier understanding and better readability.
4. A new data set was created that contains the average of each variable for each activity and each subject.

### Variables 

The new data set (see 4.) contains 180 rows and 68 columns. The 180 rows contain the means for each variable for 30 subjects and six different activities. 

The columns contain the following data:
* id:  
  Unique identifier for each of 30 subject (column 1)
* activity:  
  Name of each of six activities (column 2)
* BodyAcc.Mean_X.Axis - FourierTransform.BodyBodyGyroJerkMag.StandardDeviation (colums 3-68):  
  Mean and standard devation values for each of 33 measurements

Details on the individual measurements are available in the zip folder of the original data.