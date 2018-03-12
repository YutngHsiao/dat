Trivago Data Science Code Documentation
YutingHsiao

March 11, 2018

Code Documentation
This is an R Markdown document. Codes are done in R and Python tensorflow, tensorflow is for performance comparism of different models (SVM & Neural network). For more details please check my github https://github.com/YutngHsiao/dat.
  

Data formatting and transform characters to numerical
The first section is data evaluation, after reviewing data structure and formats, the categorical string data has been transform into quantative for analytical purpose.

# 1. Load data from .csv and transform text into meaningful figures

Manipulation of data column path_id_set

There are total 33216 locations in this set, assuming the path_id_set is a click-stream like points collection, the number of locations been visited in a session and specific location points should associated with the predictive target: hits.

path_id_set is been aggregated and splitted into a vector, the occurance of locations as

There are 4889 sessions without visiting any location, which is unusual. Meanwhile, over 90% of overall has visited location 0. I decide to remove those record without location log, due to: 1. The duration of 90% of non-path records is 0 2. The distribution of session_duration of the rest 10% shows very high figures, which is unusual.

 
# Data Pre-process 2 : Scaling features into 0:1, transform skewed session_durantion to a better distribution by applying log().

 Replace entry page with continous values

# Data Pre-process 3 : Split dataset and process target (hits)

 Feature extraction

### Remove unnecessary & low-impacted columns and get training data ready

 Correlation matrix

### corrplot 0.84 loaded

A plot of PCA result 

 Support Vector Machine
 library(e1071)

 10-fold C.V.
 RMSE 4.925165 4.936650 4.870721 4.900076 4.910934 4.882775 4.898672 4.885214 4.907764 4.916612
  

Predict hits valus by using model_6

Appendix: Pyhton tensorflow for training Neural Networks

Training result
step: 0, RMSE: 12.874874
step: 1000, RMSE: 6.834079
step: 2000, RMSE: 5.838896
step: 3000, RMSE: 5.255543 
step: 4000, RMSE: 5.153911
step: 5000, RMSE: 5.115760
step: 6000, RMSE: 5.094140
step: 7000, RMSE: 5.077186
step: 8000, RMSE: 5.062540
step: 9000, RMSE: 5.050345

Summary of predicted hits

count  416174.000000
mean        9.330908
std         5.758918
min         1.014817
25%         3.872907
50%         8.058168
75%        14.373281
max        25.231972
