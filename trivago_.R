source <- read.csv("~/Documents/source.csv", sep = ";", stringsAsFactors = F)
source_ <- source

source_$locale <- gsub("L", "", source_$locale) # Remove char L from locale
source_$locale <- as.numeric(source_$locale)
source_$day_of_week <- as.character(source_$day_of_week)
require(plyr)
source_$day_of_week <- revalue(x = source_$day_of_week, c("Monday"=1, "Thursday"=4, "Saturday"=6, "Tuesday"=2, "Friday"=5, "Sunday"=7, "Wednesday"=3)) # Change day to numerical for analysis
source_$day_of_week <- as.numeric(source_$day_of_week)

str <- paste(source_$path_id_set, collapse = ';')
str <- strsplit(str, ";")
head(table(str))

source_$num_loc <- 0
source_[nchar(source_$path_id_set)<1,]$path_id_set <- NA
require(stringr)
# Add a new attribue as how many locations are been visited during a session
source_$num_loc <- ifelse(nchar(source_$path_id_set)>0, str_count(source_$path_id_set, ";")+1, 0)

tmp <- source_[is.na(source_$num_loc),]
nrow(tmp)
nrow(tmp[tmp$session_durantion > 0,])

#source_ <- source_[!is.na(source_$num_loc),] # Remove non-path records
source_[which(is.na(source_$num_loc)),]$num_loc <- 0 

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

data_ <- source_[!is.na(source_$hits),-c(7, ncol(source_))] # Remove attributes as type character
pred_set <- source_[is.na(source_$hits),-c(7, ncol(source_))] # Split prection set (without hits value)
data_ <- data_[!data_$hits %in% boxplot.stats(data_$hits)$out,]
min(data_$hits)
#[1] 2
max(data_$hits)
#[1] 34

library(corrplot)
corrplot(cor(data_[,-1]))
cor_marix <- cor(data_[,-1])
pca <- prcomp(data_[,-1], center = TRUE, scale = TRUE)
biplot(pca,  xlabs = rep("", nrow(data_[,-1])))


### Remove unnecessary & low-impacted columns and get training data ready
data_ready <- data_[,-c(2:4, (ncol(data_)-8):ncol(data_))]
pred_ready <- pred_set[,-c(2:4, (ncol(pred_set)-8):ncol(pred_set))]
pred_ready$hits <- 0

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

