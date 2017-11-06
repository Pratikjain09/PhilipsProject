#Importing the datsheet
datasheet <- read.csv('/home/prateekjn/sois/Philips mini project/Datasheet1.csv', header = TRUE, sep = ",", na.strings = "?")
write.csv(datasheet, file = "Data2.csv")
data <- read.csv('/home/prateekjn/sois/Philips mini project/Data2.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE)


#Removing thest extra columns
data$X <- NULL
data$NA. <- NULL


#To get the count of NA values
#colSums(is.na(data))

#To replace the numeric value
#na.aggregate(data$Age)
data$Age[is.na(data$Age)] <- 51
data$Blood.Pressure[is.na(data$Blood.Pressure)] <- 76
data$Specific.Gravity[is.na(data$Specific.Gravity)] <- 1.017
data$Albumin[is.na(data$Albumin)] <- 1
data$Sugar[is.na(data$Sugar)] <- 0
data$Blood.Glucose.Random[is.na(data$Blood.Glucose.Random)] <- 148
data$Blood.Urea[is.na(data$Blood.Urea)] <- 57
data$Serum.Creatinine[is.na(data$Serum.Creatinine)] <- 3.07
data$Sodium[is.na(data$Sodium)] <- 137
data$Potassium[is.na(data$Potassium)] <- 4.6
data$Hemoglobin[is.na(data$Hemoglobin)] <- 12.5
data$Packed.Cell.Volume[is.na(data$Packed.Cell.Volume)] <- 38
data$White.Blood.Cell.Count[is.na(data$White.Blood.Cell.Count)] <- 8406
data$Red.Blood.Cell.Count[is.na(data$Red.Blood.Cell.Count)] <- 4.7

#To replace the categorical value
# count(data, 'Red.Blood.Cells')
data$Red.Blood.Cells[is.na(data$Red.Blood.Cells)] <- "normal"
data$Pus.Cell[is.na(data$Pus.Cell)] <- "normal"
data$Pus.Cell.clumps[is.na(data$Pus.Cell.clumps)] <- "notpresent"
data$Bacteria[is.na(data$Bacteria)] <- "notpresent"
data$Hypertension[is.na(data$Hypertension)] <- "no"
data$Diabetes.Mellitus[is.na(data$Diabetes.Mellitus)] <- "no"
data$Coronary.Artery.Disease[is.na(data$Coronary.Artery.Disease)] <- "no"
data$Appetite[is.na(data$Appetite)] <- "good"
data$Pedal.Edema[is.na(data$Pedal.Edema)] <- "no"
data$Anemia[is.na(data$Anemia)] <- "no"

data[,c(25)] <- trimws(data[,c(25)])
data[,c(20)] <- trimws(data[,c(20)])

write.csv(data, file="/home/prateekjn/sois/Philips mini project/properdata.csv")

#Applying chi-square test
#Applyting independant t-test
#Using step-wise forward approach

data <- subset(data, select = -c(Age,Blood.Pressure,Sugar,Red.Blood.Cells,Pus.Cell,Pus.Cell.clumps,Bacteria,Blood.Glucose.Random,Blood.Urea,Sodium,Potassium,Hemoglobin,Packed.Cell.Volume,White.Blood.Cell.Count,Red.Blood.Cell.Count,Hypertension,Diabetes.Mellitus,Coronary.Artery.Disease,Appetite,Pedal.Edema,Anemia))
data$Class <- recode(data$Class,"'notckd'= 0; 'ckd'=1")

#Training and testing (70-30)
#caret - createDataPartition
sample_size <- floor(0.70 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size=sample_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

model <- glm(Class ~ Serum.Creatinine + Specific.Gravity + Albumin, family=binomial(logit), data=train)
summary(model)
predict <- predict(model, type = 'response')
#Confusion matrix
value <- table(train$Class, predict > 0.5)
#To calculate the accuracy of the model
(value[1,1]+value[2,2])/(value[1,1]+value[2,2]+value[1,2]+value[2,1])

#ROC curve (Receiver operation curve)
ROCRpred <- prediction(predict, train$Class)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#To check the model prediction with test data set
prediction<- predict(model, test)
output <- cbind(test, prediction)
#output


#KNN
euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}



knn_predict <- function(test_data, train_data, k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[6]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"eu_char"]) == "g"){
        good = good + 1
      }
      else
        bad = bad + 1
    }
    
    # Compares the no. of neighbors with class label good or bad
    if(good > bad){          #if majority of neighbors are good then put "g" in pred vector
      
      pred <- c(pred, "g")
    }
    else if(good < bad){
      #if majority of neighbors are bad then put "b" in pred vector
      pred <- c(pred, "b")
    }
    
  }
  return(pred) #return pred vector
}





accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,3] == test_data[i,4]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}




K = 4
predictions <- knn_predict(test, train, K) #calling knn_predict()

test[,4] <- predictions #Adding predictions in test data as 7th column
print(accuracy(test))