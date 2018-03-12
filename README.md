Trivago Data Science Code Documentation
YutingHsiao

March 11, 2018

Code Documentation
This is an R Markdown document. Codes are done in R and Python tensorflow, tensorflow is for performance comparism of different models (SVM & Neural network). For more details please check my github https://github.com/YutngHsiao/dat.

  

Data formatting and transform characters to numerical
The first section is data evaluation, after reviewing data structure and formats, the categorical string data has been transform into quantative for analytical purpose.

  

Load data from .csv and transform text into meaningful figures

source_ <- read.csv("~/Documents/source.csv", sep = ";", stringsAsFactors = F)
source_$locale <- gsub("L", "", source_$locale) # Remove char L from locale
source_$locale <- as.numeric(source_$locale)
source_$day_of_week <- as.character(source_$day_of_week)
require(plyr)
source_$day_of_week <- revalue(x = source_$day_of_week, c("Monday"=1, "Thursday"=4, "Saturday"=6, "Tuesday"=2, "Friday"=5, "Sunday"=7, "Wednesday"=3)) # Change day to numerical for analysis
source_$day_of_week <- as.numeric(source_$day_of_week)
  

Manipulation of data column path_id_set

There are total 33216 locations in this set, assuming the path_id_set is a click-stream like points collection, the number of locations been visited in a session and specific location points should associated with the predictive target: hits.

path_id_set is been aggregated and splitted into a vector, the occurance of locations as

str <- paste(source_$path_id_set, collapse = ';')
str <- strsplit(str, ";")
head(table(str))
## str
##             0      1     10    100   1000 
##   4889 898419    917     69     18      1
from above, there are 4889 sessions without visiting any location, which is unusual. Meanwhile, over 90% of overall has visited location 0. I decide to remove those record without location log, due to: 1. The duration of 90% of non-path records is 0 2. The distribution of session_duration of the rest 10% shows very high figures, which is unusual.

 

source_$num_loc <- 0
source_[nchar(source_$path_id_set)<1,]$path_id_set <- NA
require(stringr)
# Add a new attribue as how many locations are been visited during a session
source_$num_loc <- ifelse(nchar(source_$path_id_set)>0, str_count(source_$path_id_set, ";")+1, 0)

tmp <- source_[is.na(source_$num_loc),]
nrow(tmp)
[1] 4889
nrow(tmp[tmp$session_durantion > 0,])
[1] 598
summary(as.numeric(tmp[tmp$session_durantion > 0,]$session_durantion))
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    1.00    10.25   239.00  7406.55  4101.75 82462.00 

source_[is.na(source_$num_loc),]$num_loc <- 0 # Set non-path records' total location count as 0
These sessions are been removed due to not qualified web connections.

   

Data Pre-process 1 : Formatting and cleaning
tb1 <- as.data.frame(table(str)) # Remove non-path records
tb1 <- tb1[order(-tb1$Freq),]
paths <- as.character(tb1[1:10,]$str)
# Add new columns for top 10 frequent locations, the datatype is binary
# If a location is been visited during a session, insert 1, otherwise 0
for (i in paths){
  #  Sample regex pattern  
  #  ifelse(grepl("38715|^38715;|;38715$|;38715;", source_$path_id_set), 1, 0)
  pattern <- paste0(i,"|^",i,";|;",i,"$|;",i,";")
  source_[, ncol(source_) + 1] <- ifelse(grepl(pattern, source_$path_id_set), 1, 0)
  names(source_)[ncol(source_)] <- paste0("p_", i)
}

# Remove outliers of continous variable
source_$session_durantion <- as.numeric(source_$session_durantion)
source_ <- source_[!is.na(source_$session_durantion),] 
source_ <- source_[!source_$session_durantion %in% boxplot.stats(source_$session_durantion)$out,]
source_ <- source_[source_$session_durantion>0,]
# Rows contain NA and zero session_durantion are also been removed (identified as brocken connections)
 

A summary of formatted dataset

##     row_num           locale       day_of_week     hour_of_day   
##  Min.   :     2   Min.   :1.000   Min.   :1.000   Min.   : 0.00  
##  1st Qu.:247170   1st Qu.:3.000   1st Qu.:2.000   1st Qu.: 9.00  
##  Median :494702   Median :3.000   Median :4.000   Median :15.00  
##  Mean   :494544   Mean   :3.648   Mean   :3.854   Mean   :13.29  
##  3rd Qu.:741898   3rd Qu.:5.000   3rd Qu.:6.000   3rd Qu.:19.00  
##  Max.   :988680   Max.   :6.000   Max.   :7.000   Max.   :23.00  
##     agent_id        entry_page   path_id_set         traffic_type   
##  Min.   : 0.000   Min.   :2100   Length:730187      Min.   : 1.000  
##  1st Qu.: 6.000   1st Qu.:2113   Class :character   1st Qu.: 1.000  
##  Median : 9.000   Median :2113   Mode  :character   Median : 2.000  
##  Mean   : 7.419   Mean   :2174                      Mean   : 2.807  
##  3rd Qu.:10.000   3rd Qu.:2116                      3rd Qu.: 4.000  
##  Max.   :15.000   Max.   :8101                      Max.   :10.000  
##  session_durantion     hits              num_loc            p_0        
##  Min.   :  1.0     Length:730187      Min.   : 1.000   Min.   :0.0000  
##  1st Qu.:  6.0     Class :character   1st Qu.: 2.000   1st Qu.:1.0000  
##  Median : 51.0     Mode  :character   Median : 2.000   Median :1.0000  
##  Mean   :133.9                        Mean   : 2.067   Mean   :0.9745  
##  3rd Qu.:188.0                        3rd Qu.: 2.000   3rd Qu.:1.0000  
##  Max.   :809.0                        Max.   :29.000   Max.   :1.0000  
##     p_38715           p_34741           p_34812           p_34390       
##  Min.   :0.00000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :0.00000   Median :0.00000   Median :0.00000   Median :0.00000  
##  Mean   :0.02076   Mean   :0.01632   Mean   :0.01493   Mean   :0.00921  
##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :1.00000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
##     p_78506            p_34227             p_8514          p_31490        
##  Min.   :0.000000   Min.   :0.000000   Min.   :0.0000   Min.   :0.000000  
##  1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:0.000000  
##  Median :0.000000   Median :0.000000   Median :0.0000   Median :0.000000  
##  Mean   :0.008314   Mean   :0.008468   Mean   :0.0099   Mean   :0.007784  
##  3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:0.000000  
##  Max.   :1.000000   Max.   :1.000000   Max.   :1.0000   Max.   :1.000000  
##     p_31965        
##  Min.   :0.000000  
##  1st Qu.:0.000000  
##  Median :0.000000  
##  Mean   :0.007557  
##  3rd Qu.:0.000000  
##  Max.   :1.000000
  

Data Pre-process 2 : Scaling features into 0:1, transform skewed session_durantion to a better distribution by applying log().
range01 <- function(x){(x-min(x))/(max(x)-min(x))} # Scaling function

source_$locale <- range01(source_$locale)
source_$day_of_week <- range01(source_$day_of_week)
source_$hour_of_day <- range01(source_$hour_of_day)
source_$agent_id <- range01(source_$agent_id)
source_$traffic_type <- range01(source_$traffic_type)
source_$session_durantion <- log(source_$session_durantion) # Log transformation of skewed  session_durantion
source_$session_durantion <- range01(source_$session_durantion)
source_$num_loc <- range01(source_$num_loc)
source_$hits <- as.numeric(source_$hits)

# Replace entry page with continous values
entry_id <- 1:length(unique(source_$entry_page))
names(entry_id) <- unique(source_$entry_page)[order(unique(source_$entry_page))]
source_$entry_id <- entry_id[as.character(source_$entry_page)]
source_$entry_page <- range01(source_$entry_id)
 



 



 

Data Pre-process 3 : Split dataset and process target (hits)
data_ <- source_[!is.na(source_$hits),-c(7, ncol(source_))] # Remove attributes as type character
pred_set <- source_[is.na(source_$hits),-c(7, ncol(source_))] # Split prection set (without hits value)
data_ <- data_[!data_$hits %in% boxplot.stats(data_$hits)$out,]
#> min(data_$hits)
#[1] 2
#> max(data_$hits)
#[1] 34
 

Feature extraction
library(corrplot)
corrplot(cor(data_[,-1]))
cor_marix <- cor(data_[,-1])
pca <- prcomp(data_[,-1], center = TRUE, scale = TRUE)
biplot(pca,  xlabs = rep("", nrow(data_[,-1])))

### Remove unnecessary & low-impacted columns and get training data ready
data_ready <- data_[,-c(2:4, (ncol(data_)-8):ncol(data_))]
pred_ready <- pred_set_[,-c(2:4, (ncol(pred_set_)-8):ncol(pred_set_))]
pred_ready$hits <- 0
 

Correlation matrix

## corrplot 0.84 loaded


 

A plot of PCA result



  

Sample of processed dataset for training
row_num	agent_id	entry_page	traffic_type	session_durantion	hits	num_loc	p_0
2	988680	0.6666667	0.0152672	0.1111111	0.5812332	14	0.0357143	1
5	988677	0.6666667	0.0381679	0.0000000	0.1035197	3	0.0357143	1
8	988674	0.0666667	0.0152672	0.5555556	0.1035197	3	0.0357143	1
9	988673	0.6666667	0.0076336	0.5555556	0.2070394	3	0.0357143	1
12	988670	0.6000000	0.0152672	0.1111111	0.7678940	15	0.0357143	1
13	988669	0.5333333	0.0152672	0.1111111	0.7978636	24	0.0357143	1
  

Support Vector Machine
library(e1071)
RMSE <- vector()
for(i in 1:10){ # 10 fold
  set.seed(round(runif(1, 1, 10000)))
  smp <- sample(c(0,1), nrow(data_ready), c(.3, .7), replace = T) # Split data to 30% sample testing, 70% training
  train_set <- data_ready[smp == 1,-1]
  test_set <- data_ready[smp == 0,-1]
  model_svm <- svm(hits ~ . , train_set)
  assign(paste0("model_", model_svm)) # Save trained models
  pred <- predict(model_svm, test_set) # Make prediction
  assign(paste0("pred_", i))
  err <- test_set$hits - pred
  RMSE <- c(RMSE, sqrt(mean(err^2)))
}
  

10-fold C.V.

RMSE [1] 4.925165 4.936650 4.870721 4.900076 4.910934 4.882775 4.898672 4.885214 4.907764 4.916612
  

Predict hits valus by using model_6
p_ <- predict(model_6, pred_ready[,-1])
pred_ready$hits <- p_
The result of prediction as below

row_num	locale	day_of_week	hour_of_day	agent_id	entry_page	path_id_set	traffic_type	session_durantion	hits
17	988665	L4	Sunday	16	10	2113	79148;0	4	5	3
19	988663	L3	Friday	20	10	2111	34287;0;60579	2	117	18
21	988661	L2	Friday	19	10	2113	46575;0	2	80	10
22	988660	L2	Thursday	20	1	2111	31467;0	2	26	7
28	988654	L6	Saturday	15	6	2100	0;95235	1	44	10
38	988644	L3	Friday	20	1	2113	34741;0	6	2	4
Appendix: Pyhton tensorflow for training Neural Networks
import tensorflow as tf
import matplotlib.pyplot as plt
import numpy as np
from numpy import genfromtxt
import pandas as pd
import math

train = genfromtxt("/content/trivago/data_ready.csv", delimiter=',')
train = np.delete(train, 0, 0)
train = np.delete(train, 0, 1)

x = train[:,0:6];x

y = train[:,6][:, np.newaxis];y
LR = 0.02
batchz = 1000
epoch = 100
datapoints = len(y)
steps = 10000

tf_x = tf.placeholder(tf.float32, x.shape)     # input x holder
tf_y = tf.placeholder(tf.float32, y.shape)

# Network layers
L = tf.layers.dense(tf_x, 6, tf.nn.relu)
L = tf.layers.dense(tf_x, 4, tf.nn.relu)
L = tf.layers.dense(L, 2, tf.nn.relu)
output = tf.layers.dense(L, 1)                     # output layer

loss = tf.losses.mean_squared_error(tf_y, output)   # compute cost
optimizer = tf.train.GradientDescentOptimizer(learning_rate=LR)
train_op = optimizer.minimize(loss)

saver = tf.train.Saver()

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    for step in range(steps):
        # train and net output
        _, loss_, pred = sess.run([train_op, loss, output], {tf_x: x, tf_y: y})
        if step % batchz == 0:
            print("step: {}, RMSE: {}".format(step, math.sqrt(loss_)))

    test = sess.run(output, {tf_x: x})
    save_model = saver.save(sess, "/content/dat/model.ckpt")
    test_hits = pd.DataFrame(test)
    print("Summary of predicted hits")
    print(test_hits.describe())
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
