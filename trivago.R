ml <- read.csv("~/Documents/ML.csv", sep = ";", stringsAsFactors = F)

ml_ <- ml[,-1]
unique(ml_[,1])

ml_$locale <- gsub("L", "", ml_$locale)
ml_$locale <- as.numeric(ml_$locale)

ml_$day_of_week <- as.character(ml_$day_of_week)

require(plyr)
ml_$day_of_week <- revalue(x = ml_$day_of_week, c("Monday"=1, "Thursday"=4, "Saturday"=6, "Tuesday"=2, "Friday"=5, "Sunday"=7, "Wednesday"=3))
ml_$day_of_week <- as.numeric(ml_$day_of_week)


nchar(ml_$num_loc[1])
ml_$num_loc <- 0
ml_[nchar(ml_$path_id_set)<1,]$path_id_set <- NA
require(stringr)
ml_$num_loc <- ifelse(nchar(ml_$path_id_set)>0, str_count(ml_$path_id_set, ";")+1, 0)
ml_[is.na(ml_$num_loc),]$num_loc <- 0
tb1 <- as.data.frame(table(str))
tb1 <- tb1[order(-tb1$Freq),]
paths <- as.character(tb1[3:10,]$str)
for (i in paths){
  pattern <- paste0(i,"|^",i,";|;",i,"$|;",i,";")
  
  ml_[, ncol(ml_) + 1] <- ifelse(grepl(pattern, ml_$path_id_set), 1, 0)
  names(ml_)[ncol(ml_)] <- paste0("p_", i)
}
#ml_$p_38715 <- ifelse(grepl("38715|^38715;|;38715$|;38715;", ml_$path_id_set), 1, 0)
summary(ml_)
#length(ml_[is.na(ml_$session_durantion),]$session_durantion) #668
ml_1 <- ml_[!is.na(ml_$session_durantion),] #the removal of NA values
summary(ml_1)

ml_2 <- ml_1[!ml_1$session_durantion %in% boxplot.stats(ml_1$session_durantion)$out,]
ml_2 <- ml_2[ml_2$session_durantion>0,]

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ml_3 <- ml_2
ml_3$locale <- range01(ml_2$locale)
ml_3$day_of_week <- range01(ml_3$day_of_week)
ml_3$hour_of_day <- range01(ml_3$hour_of_day)
ml_3$agent_id <- range01(ml_3$agent_id)

entry_id <- 1:length(unique(ml_2$entry_page))
names(entry_id) <- unique(ml_2$entry_page)[order(unique(ml_2$entry_page))]
ml_3$entry_id <- entry_id[as.character(ml_2$entry_page)]
ml_3$entry_page <- range01(ml_3$entry_id)

ml_3$traffic_type <- range01(ml_3$traffic_type)

ml_3$session_durantion <- log(ml_2$session_durantion)
ml_3$session_durantion <- range01(ml_3$session_durantion)

ml_3$num_loc <- range01(ml_3$num_loc)

predict_set <- ml_3[ml_3$hits == "\\N", -c(6, ncol(ml_3))]
dat <- ml_3[ml_3[,"hits"] != "\\N", -c(6, 11:13, ncol(ml_3))]
dat$hits <- as.numeric(dat$hits)
dat <- dat[!dat$hits %in% boxplot.stats(dat$hits)$out,]
dat$hits <- range01(dat$hits)

corrplot(cor(dat))
cor_marix <- cor(dat)
pca <- prcomp(dat, center = TRUE, scale = TRUE)
biplot(pca,  xlabs = rep("·", nrow(dat)))

dat_ <- dat[,rownames(cor_marix[cor_marix$hits>.05,])]
summary(dat_)



############## SVM
library(e1071)
MSEs <- vector()
for(i in 1:10){
  set.seed(round(runif(1, 1, 10000)))
  smp <- sample(c(0,1), nrow(dat), c(.3, .7), replace = T)
  train_set <- dat[smp == 1,]
  test_set <- dat[smp == 0,]
  model_svm <- svm(hits ~ . , train_set)
  assign(paste0("model_svm_", i), model_svm)
  pred <- predict(model_svm, test_set)
  assign(paste0("pred_", i))
  err <- test_set$hits - pred
  MSEs <- c(MSE, mean(err^2))
}
#########################
confusionMatrix(as.numeric(pred), dat$hits[1:10000]) 
length(pred)


training[["V1"]] = factor(training[["V1"]]) #conversion of V1 integer variable to factor variable

#Training & Preprocessing 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(V1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit #knn classifier

#plot accuracy vs K Value graph 
plot(knn_fit) 

#predict classes for test set using knn classifier
test_pred <- predict(knn_fit, newdata = testing)
test_pred

#Test set Statistics 
confusionMatrix(test_pred, testing$V1 ) 
