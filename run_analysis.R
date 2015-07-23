run_analysis <- function() {
##This course project for Getting and Cleaning Data is based on the UCI HAR Datasets.
##The author would like to acknowledge the contribution made by the following:
##Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
##Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 
##International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

##STEP 1: Merges the training and the test sets to create one data set.
##Read the train data sets consist of subject_train.txt, X_train.txt and y_train.txt
subj_train <- read.table("./train/subject_train.txt")
XtrainDat <- read.table("./train/X_train.txt")
YtrainDat <- read.table("./train/y_train.txt")

##Read the test data sets consist of subject_test.txt, X_test.txt and y_test.txt
subj_test <- read.table("./test/subject_test.txt")
XtestDat <- read.table("./test/X_test.txt")
YtestDat <- read.table("./test/y_test.txt")
##Read the activity_labels.txt, features.txt and features_info.txt
act_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

##Converts data.frame to data.table for faster and efficient access
library(data.table)
library(dplyr) ##for efficient data manipulation
subj_trainDT = data.table(subj_train)
XtrainDT = data.table(XtrainDat)
YtrainDT = data.table(YtrainDat)
subj_testDT = data.table(subj_test)
XtestDT = data.table(XtestDat)
YtestDT = data.table(YtestDat)
act_labelsDT = data.table(act_labels)
featuresDT = data.table(features)

##Housekeeping to remove unwanted data.frames (optional for memory management)
##rm("subj_train")
##rm("XtrainDat")
##rm('YtrainDat')
##rm("subj_test")
##rm("XtestDat")
##rm("YtestDat")
##rm("act_labels")
##rm("features")

##merge subj_trainDT and YtrainDT
subj_Y_train <- mutate(subj_trainDT, YtrainDT) ##merge/join subject and activity code for train dataset
subj_Y_test <- mutate(subj_testDT, YtestDT) ##merge/join subject and activity code for test dataset

##STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement.
meanstd <- featuresDT[V2 %like% "mean()" | V2 %like% "std()"] ##captures mean and std variables only
ms <- meanstd[[1]] ##an integer vector that stores the positions of mean/std of featuresDT
setnames(subj_Y_train, "V1", "Subject")##Use descriptive name for Subject column
setnames(subj_Y_test, "V1", "Subject")##Prepare for merging between the train and test datasets
for (i in 1:length(ms)) {##use cbind
  subj_Y_train <- cbind(subj_Y_train, XtrainDT[,ms[i], with = FALSE])##add columns for all mean/std in train dataset
  subj_Y_test <- cbind(subj_Y_test, XtestDT[,ms[i], with = FALSE])##add columns for all mean/std in test dataset
}
##Clean dataset using mean to collapse each variable to measure in 1 column and different observation of subject and activity in different row.
##STEP 3: Appropriately labels the data set with descriptive variable names.
result1 <- subj_Y_train %>%
  group_by(Subject, YtrainDT) %>%
  summarize(Count = n(),
            tBodyAccMeanX = mean(V1),
            tBodyAccMeanY = mean(V2),
            tBodyAccMeanZ = mean(V3),
            tBodyAccstdX  = mean(V4),
            tBodyAccstdY  = mean(V5),
            tBodyAccstdZ  = mean(V6),
            tGravityAccmeanX = mean(V41),
            tGravityAccmeanY = mean(V42),
            tGravityAccmeanZ = mean(V43),
            tGravityAccstdX  = mean(V44),
            tGravityAccstdY  = mean(V45),
            tGravityAccstdZ  = mean(V46),
            tBodyAccJerkmeanX = mean(V81),
            tBodyAccJerkmeanY = mean(V82),
            tBodyAccJerkmeanZ = mean(V83),
            tBodyAccJerkstdX  = mean(V84),
            tBodyAccJerkstdY  = mean(V85),
            tBodyAccJerkstdZ  = mean(V86),
            tBodyGyromeanX = mean(V121),
            tBodyGyromeanY = mean(V122),
            tBodyGyromeanZ = mean(V123),
            tBodyGyrostdX  = mean(V124),
            tBodyGyrostdY  = mean(V125),
            tBodyGyrostdZ  = mean(V126),
            tBodyGyroJerkmeanX = mean(V161),
            tBodyGyroJerkmeanY = mean(V162),
            tBodyGyroJerkmeanZ = mean(V163),
            tBodyGyroJerkstdX  = mean(V164),
            tBodyGyroJerkstdY  = mean(V165),
            tBodyGyroJerkstdZ  = mean(V166),
            tBodyAccMagmean    = mean(V201),
            tBodyAccMagstd     = mean(V202),
            tGravityAccMagmean = mean(V214),
            tGravityAccMagstd  = mean(V215),
            tBodyAccJerkMagmean = mean(V227),
            tBodyAccJerkMagstd  = mean(V228),
            tBodyGyroMagmean    = mean(V240),
            tBodyGyroMagstd     = mean(V241),
            tBodyGyroJerkMagmean = mean(V253),
            tBodyGyroJerkMagstd  = mean(V254),
            fBodyAccmeanX        = mean(V266),
            fBodyAccmeanY        = mean(V267),
            fBodyAccmeanZ        = mean(V268),
            fBodyAccstdX         = mean(V269),
            fBodyAccstdY         = mean(V270),
            fBodyAccstdZ         = mean(V271),
            fBodyAccmeanFreqX    = mean(V294),
            fBodyAccmeanFreqY    = mean(V295),
            fBodyAccmeanFreqZ    = mean(V296),
            fBodyAccJerkmeanX    = mean(V345),
            fBodyAccJerkmeanY    = mean(V346),
            fBodyAccJerkmeanZ    = mean(V347),
            fBodyAccJerkstdX     = mean(V348),
            fBodyAccJerkstdY     = mean(V349),
            fBodyAccJerkstdZ     = mean(V350),
            fBodyAccJerkmeanFreqX = mean(V373),
            fBodyAccJerkmeanFreqY = mean(V374),
            fBodyAccJerkmeanFreqZ = mean(V375),
            fBodyGyromeanX        = mean(V424),
            fBodyGyromeanY        = mean(V425),
            fBodyGyromeanZ        = mean(V426),
            fBodyGyrostdX         = mean(V427),
            fBodyGyrostdY         = mean(V428),
            fBodyGyrostdZ         = mean(V429),
            fBodyGyromeanFreqX    = mean(V452),
            fBodyGyromeanFreqY    = mean(V453),
            fBodyGyromeanFreqZ    = mean(V454),
            fBodyAccMagmean       = mean(V503),
            fBodyAccMagstd        = mean(V504),
            fBodyAccMagmeanFreq   = mean(V513),
            fBodyBodyAccJerkMagmean = mean(V516),
            fBodyBodyAccJerkMagstd  = mean(V517),
            fBodyBodyAccJerkMagmeanFreq = mean(V526),
            fBodyBodyGyroMagmean        = mean(V529),
            fBodyBodyGyroMagstd         = mean(V530),
            fBodyBodyGyroMagmeanFreq    = mean(V539),
            fBodyBodyGyroJerkMagmean    = mean(V542),
            fBodyBodyGyroJerkMagstd     = mean(V543),
            fBodyBodyGyroJerkMagmeanFreq = mean(V552)
            )
result2 <- subj_Y_test %>%
  group_by(Subject,YtestDT) %>%
  summarize(Count = n(),
            tBodyAccMeanX = mean(V1),
            tBodyAccMeanY = mean(V2),
            tBodyAccMeanZ = mean(V3),
            tBodyAccstdX  = mean(V4),
            tBodyAccstdY  = mean(V5),
            tBodyAccstdZ  = mean(V6),
            tGravityAccmeanX = mean(V41),
            tGravityAccmeanY = mean(V42),
            tGravityAccmeanZ = mean(V43),
            tGravityAccstdX  = mean(V44),
            tGravityAccstdY  = mean(V45),
            tGravityAccstdZ  = mean(V46),
            tBodyAccJerkmeanX = mean(V81),
            tBodyAccJerkmeanY = mean(V82),
            tBodyAccJerkmeanZ = mean(V83),
            tBodyAccJerkstdX  = mean(V84),
            tBodyAccJerkstdY  = mean(V85),
            tBodyAccJerkstdZ  = mean(V86),
            tBodyGyromeanX = mean(V121),
            tBodyGyromeanY = mean(V122),
            tBodyGyromeanZ = mean(V123),
            tBodyGyrostdX  = mean(V124),
            tBodyGyrostdY  = mean(V125),
            tBodyGyrostdZ  = mean(V126),
            tBodyGyroJerkmeanX = mean(V161),
            tBodyGyroJerkmeanY = mean(V162),
            tBodyGyroJerkmeanZ = mean(V163),
            tBodyGyroJerkstdX  = mean(V164),
            tBodyGyroJerkstdY  = mean(V165),
            tBodyGyroJerkstdZ  = mean(V166),
            tBodyAccMagmean    = mean(V201),
            tBodyAccMagstd     = mean(V202),
            tGravityAccMagmean = mean(V214),
            tGravityAccMagstd  = mean(V215),
            tBodyAccJerkMagmean = mean(V227),
            tBodyAccJerkMagstd  = mean(V228),
            tBodyGyroMagmean    = mean(V240),
            tBodyGyroMagstd     = mean(V241),
            tBodyGyroJerkMagmean = mean(V253),
            tBodyGyroJerkMagstd  = mean(V254),
            fBodyAccmeanX        = mean(V266),
            fBodyAccmeanY        = mean(V267),
            fBodyAccmeanZ        = mean(V268),
            fBodyAccstdX         = mean(V269),
            fBodyAccstdY         = mean(V270),
            fBodyAccstdZ         = mean(V271),
            fBodyAccmeanFreqX    = mean(V294),
            fBodyAccmeanFreqY    = mean(V295),
            fBodyAccmeanFreqZ    = mean(V296),
            fBodyAccJerkmeanX    = mean(V345),
            fBodyAccJerkmeanY    = mean(V346),
            fBodyAccJerkmeanZ    = mean(V347),
            fBodyAccJerkstdX     = mean(V348),
            fBodyAccJerkstdY     = mean(V349),
            fBodyAccJerkstdZ     = mean(V350),
            fBodyAccJerkmeanFreqX = mean(V373),
            fBodyAccJerkmeanFreqY = mean(V374),
            fBodyAccJerkmeanFreqZ = mean(V375),
            fBodyGyromeanX        = mean(V424),
            fBodyGyromeanY        = mean(V425),
            fBodyGyromeanZ        = mean(V426),
            fBodyGyrostdX         = mean(V427),
            fBodyGyrostdY         = mean(V428),
            fBodyGyrostdZ         = mean(V429),
            fBodyGyromeanFreqX    = mean(V452),
            fBodyGyromeanFreqY    = mean(V453),
            fBodyGyromeanFreqZ    = mean(V454),
            fBodyAccMagmean       = mean(V503),
            fBodyAccMagstd        = mean(V504),
            fBodyAccMagmeanFreq   = mean(V513),
            fBodyBodyAccJerkMagmean = mean(V516),
            fBodyBodyAccJerkMagstd  = mean(V517),
            fBodyBodyAccJerkMagmeanFreq = mean(V526),
            fBodyBodyGyroMagmean        = mean(V529),
            fBodyBodyGyroMagstd         = mean(V530),
            fBodyBodyGyroMagmeanFreq    = mean(V539),
            fBodyBodyGyroJerkMagmean    = mean(V542),
            fBodyBodyGyroJerkMagstd     = mean(V543),
            fBodyBodyGyroJerkMagmeanFreq = mean(V552)
            )

##STEP 4: Uses descriptive activity names to name the activities in the data set.
setnames(result1, "YtrainDT", "Activity")##set names of column for both datasets to be the same
setnames(result2, "YtestDT", "Activity")
bindTrainTest <- rbind(result1, result2, fill = TRUE)##combine both train and test data sets together
bindTrainTest <- mutate(bindTrainTest, Activity_Name = act_labelsDT$V2[Activity])
write.table(bindTrainTest, file = "tidy1.txt", row.names = FALSE)
##print(bindTrainTest)

##STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
result3 <- bindTrainTest %>%
  group_by(Activity_Name, Subject) %>%
  summarize(
            tBodyAccMeanX = mean(tBodyAccMeanX),
            tBodyAccMeanY = mean(tBodyAccMeanY),
            tBodyAccMeanZ = mean(tBodyAccMeanZ),
            tBodyAccstdX  = mean(tBodyAccstdX),
            tBodyAccstdY  = mean(tBodyAccstdY),
            tBodyAccstdZ  = mean(tBodyAccstdZ),
            tGravityAccmeanX = mean(tGravityAccmeanX),
            tGravityAccmeanY = mean(tGravityAccmeanY),
            tGravityAccmeanZ = mean(tGravityAccmeanZ),
            tGravityAccstdX  = mean(tGravityAccstdX),
            tGravityAccstdY  = mean(tGravityAccstdY),
            tGravityAccstdZ  = mean(tGravityAccstdZ),
            tBodyAccJerkmeanX = mean(tBodyAccJerkmeanX),
            tBodyAccJerkmeanY = mean(tBodyAccJerkmeanY),
            tBodyAccJerkmeanZ = mean(tBodyAccJerkmeanZ),
            tBodyAccJerkstdX  = mean(tBodyAccJerkstdX),
            tBodyAccJerkstdY  = mean(tBodyAccJerkstdY),
            tBodyAccJerkstdZ  = mean(tBodyAccJerkstdZ),
            tBodyGyromeanX = mean(tBodyGyromeanX),
            tBodyGyromeanY = mean(tBodyGyromeanY),
            tBodyGyromeanZ = mean(tBodyGyromeanZ),
            tBodyGyrostdX  = mean(tBodyGyrostdX),
            tBodyGyrostdY  = mean(tBodyGyrostdY),
            tBodyGyrostdZ  = mean(tBodyGyrostdZ),
            tBodyGyroJerkmeanX = mean(tBodyGyroJerkmeanX),
            tBodyGyroJerkmeanY = mean(tBodyGyroJerkmeanY),
            tBodyGyroJerkmeanZ = mean(tBodyGyroJerkmeanZ),
            tBodyGyroJerkstdX  = mean(tBodyGyroJerkstdX),
            tBodyGyroJerkstdY  = mean(tBodyGyroJerkstdY),
            tBodyGyroJerkstdZ  = mean(tBodyGyroJerkstdZ),
            tBodyAccMagmean    = mean(tBodyAccMagmean),
            tBodyAccMagstd     = mean(tBodyAccMagstd),
            tGravityAccMagmean = mean(tGravityAccMagmean),
            tGravityAccMagstd  = mean(tGravityAccMagstd),
            tBodyAccJerkMagmean = mean(tBodyAccJerkMagmean),
            tBodyAccJerkMagstd  = mean(tBodyAccJerkMagstd),
            tBodyGyroMagmean    = mean(tBodyGyroMagmean),
            tBodyGyroMagstd     = mean(tBodyGyroMagstd),
            tBodyGyroJerkMagmean = mean(tBodyGyroJerkMagmean),
            tBodyGyroJerkMagstd  = mean(tBodyGyroJerkMagstd),
            fBodyAccmeanX        = mean(fBodyAccmeanX),
            fBodyAccmeanY        = mean(fBodyAccmeanY),
            fBodyAccmeanZ        = mean(fBodyAccmeanZ),
            fBodyAccstdX         = mean(fBodyAccstdX),
            fBodyAccstdY         = mean(fBodyAccstdY),
            fBodyAccstdZ         = mean(fBodyAccstdZ),
            fBodyAccmeanFreqX    = mean(fBodyAccmeanFreqX),
            fBodyAccmeanFreqY    = mean(fBodyAccmeanFreqY),
            fBodyAccmeanFreqZ    = mean(fBodyAccmeanFreqZ),
            fBodyAccJerkmeanX    = mean(fBodyAccJerkmeanX),
            fBodyAccJerkmeanY    = mean(fBodyAccJerkmeanY),
            fBodyAccJerkmeanZ    = mean(fBodyAccJerkmeanZ),
            fBodyAccJerkstdX     = mean(fBodyAccJerkstdX),
            fBodyAccJerkstdY     = mean(fBodyAccJerkstdY),
            fBodyAccJerkstdZ     = mean(fBodyAccJerkstdZ),
            fBodyAccJerkmeanFreqX = mean(fBodyAccJerkmeanFreqX),
            fBodyAccJerkmeanFreqY = mean(fBodyAccJerkmeanFreqY),
            fBodyAccJerkmeanFreqZ = mean(fBodyAccJerkmeanFreqZ),
            fBodyGyromeanX        = mean(fBodyGyromeanX),
            fBodyGyromeanY        = mean(fBodyGyromeanY),
            fBodyGyromeanZ        = mean(fBodyGyromeanZ),
            fBodyGyrostdX         = mean(fBodyGyrostdX),
            fBodyGyrostdY         = mean(fBodyGyrostdY),
            fBodyGyrostdZ         = mean(fBodyGyrostdZ),
            fBodyGyromeanFreqX    = mean(fBodyGyromeanFreqX),
            fBodyGyromeanFreqY    = mean(fBodyGyromeanFreqY),
            fBodyGyromeanFreqZ    = mean(fBodyGyromeanFreqZ),
            fBodyAccMagmean       = mean(fBodyAccMagmean),
            fBodyAccMagstd        = mean(fBodyAccMagstd),
            fBodyAccMagmeanFreq   = mean(fBodyAccMagmeanFreq),
            fBodyBodyAccJerkMagmean = mean(fBodyBodyAccJerkMagmean),
            fBodyBodyAccJerkMagstd  = mean(fBodyBodyAccJerkMagstd),
            fBodyBodyAccJerkMagmeanFreq = mean(fBodyBodyAccJerkMagmeanFreq),
            fBodyBodyGyroMagmean        = mean(fBodyBodyGyroMagmean),
            fBodyBodyGyroMagstd         = mean(fBodyBodyGyroMagstd),
            fBodyBodyGyroMagmeanFreq    = mean(fBodyBodyGyroMagmeanFreq),
            fBodyBodyGyroJerkMagmean    = mean(fBodyBodyGyroJerkMagmean),
            fBodyBodyGyroJerkMagstd     = mean(fBodyBodyGyroJerkMagstd),
            fBodyBodyGyroJerkMagmeanFreq = mean(fBodyBodyGyroJerkMagmeanFreq)
  )
write.table(result3, file = "tidy2.txt", row.names = FALSE)
print(result3)
}