## README
This README describes the script contains in the run_analysis.R source code. Before the script can be run, install the data.table package since the script uses data.table to access the datasets for fast and efficient access. Another package need to be installed is dplyr for efficient data manipulation that helped in cleaning the datasets, which is the main objective of the run_analysis.R script.  
  
The run_analysis.R script reads data collected from experiments carried out with a group of 30 volunteers each performing six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. A full description is available at the following site where the data was obtained:  
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones  
  
The author would like to acknowledge the contribution made by Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto and Xavier Parra from the Università degli Studi di Genova, Genoa (I-16145), Italy and Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain in carrying out the experiments and providing the data.  
  
To run the script, data can be downloaded from the following site to your working directory:  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
  
Once downloaded, the following datasets will be made available and access by run_analysis.R:  
1. activity_labels.txt - lists the six activities and their corresponding codes used by the train and test datasets.  
2. features.txt - a list of 561 features with time and frequency domain variables.  
3. subject_train.txt - the training dataset as identifier of the subject involved in the experiment.  
4. X_train.txt - measurements of the 561 features recorded on the subjects in the subject_train.txt.  
5. y_train.txt - coded activities performed by the subject in the subject_train.txt.  
6. subject_test.txt - the test dataset as identifier of the subject involved in the experiment.  
7. X_test.txt - measurements of the 561 features recorded on the subjects in the subject_test.txt.  
8. y_test.txt - coded activities performed by the subject in the subject_test.txt.  
  
For the purpose of this project, run_analysis.R uses features related to mean and standard deviation only. Hence, a wild card search was conducted on the features list extracting feature labels that have the word "mean" and "std" in them. The R expression used is: featuresDT[V2 %like% "mean()" | V2 %like% "std()"], where V2 is the feature label variable in the featureDT data table. A total of 79 features were extracted out of the 561 features related to mean and std.  
  
Please refer to the Code Book for the meaning of the codes that represent the activities and the codes that represent the 79 features for measuring the mean and standard deviation. Additionally, an R version of the Code Book is also included as an option in this repository which can be downloaded to your working directory and "knit" from the RStudio.  
  
The run_analysis.R script performs the following 5 steps:  
1.  Merges the training and the test sets to create one data set.  
2.  Extracts only the measurements on the mean and standard deviation for each measurement.  
3.  Uses descriptive activity names to name the activities in the data set.  
4.  Creates a tidy data set (tidy1.txt) and appropriately labels the data set with descriptive variable names.  
5.  From the data set in step 4, creates a second, independent tidy data set (tidy2.txt) with the average of each variable for each activity and each subject.  
  
