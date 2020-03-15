options(scipen = 999)

#Reading the Data from the CSV file
data = read.csv("E:\\train.csv")

#Data saving to another variable to avoid changes in the original data
data1 = data
 
#Seperating the numeric and character variables 
data_num = sapply(data1,is.numeric)    # Numeric Variables 
data_char = !sapply(data1,is.numeric) # Character variables


# Creating a function to define the missing value by just replacing -1 with NA
miss = function(x)
{
y =  ifelse(x==-1,"NA",x)
y = as.numeric(y)
}

#Applying the function on the numeric data using apply function
data1[,data_num] =   data.frame(apply(data1[,data_num], 2,miss))

#Defining User function for Descriptive Statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  total = n+nmiss
  miss_per = (nmiss/total)*100
  s <- sd(a)
  min <- min(a)
  pctls <-quantile(a,probs=c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))
  pctl_5 = quantile(a,probs = 0.05) 
  pctl_95 = quantile(a,probs = 0.95) 
  max <- max(a)
  uc = pctl_95
  lc = pctl_5
  flag = max>uc|min<lc
  return(c(n=n, nmiss=nmiss,miss_per=miss_per,  mean=m, stdev=s,pctls=pctls,pctl_5=pctl_5, pctl_95=pctl_95,min = min,  max=max,uc = uc,lc=lc,flag = flag))
}

#Finding the Descriptive Statistics for data set having Numeric Variables
stats = apply(data1[,data_num],2,mystats)
stats = t(stats)
stats = as.data.frame(stats)
View(stats)




# Defining user defined fuction for outlier treatment
outliers = function(x){
  up = quantile(x,probs= 0.95,na.rm = T)
  down = quantile(x,probs=0.05,na.rm = T) 
  x[x<down] = down
  x[x>up] = up
  x
}

# Treating outliers in the training dataset using the user defined function
data1[,data_num] = data.frame(apply(data1[,data_num],2,outliers)) 



#Missing Values Treatment

#Count of missing values column-wise
stats[stats$nmiss>1,]

#deleting the columns containing missing values more than 25%
data1[,c("page1_exits","page2_exits","page3_exits","page4_exits","page5_exits","page6_exits")] = list(NULL)

#Deleting the unwanted columns
data[,c("region","sourceMedium","country","dayHourMinute","unique_id")] = list(NULL)


#creating a model using logistics function
model = glm(target~.,family = "binomial",data=data1)
summary(model)

#Deleting the variables which are showing no contibution to the above model
data1[,c("page2_top","visited_page2","page5_top","visited_page5")] = list(NULL)


#creating a model1 using logistics function after deleting the varaibles  
model1 = glm(target~.,family = "binomial",data=data1)
summary(model1)

#vIF function to check the multicollinearity for the independent variables
car::vif(model1)

# Step AIC function to do the significant variable selection
library(MASS)
stepAIC(model1,direction = "both")

# Model created on the significant variables got from the Step AIC 
model2=glm(formula = target ~ metric1 + metric2 + metric6 + metric3 + 
  metric4 + metric5 + binary_var1 + device + binary_var2 + 
  visited_page1 + visited_page3 + visited_page4 + page6_top + 
  visited_page6,family = "binomial", data = data1)

summary(model2)
car::vif(model2)

# Model created after deleting the insignificant variables using the summary of the model2
model3 = glm(formula = target ~ metric1 + metric2 + metric6 +  
               metric4 + metric5 + binary_var1 + device + binary_var2 + 
               visited_page1 + visited_page3 + visited_page4 + page6_top + 
               visited_page6,family = "binomial", data = data1)

summary(model3)

# Using VIF function to check the multicollinearity of the independent variables
car::vif(model3)

#Splitting the data into two parts Train and Test with 70/30 split
set.seed(100)
library(caret)
split = createDataPartition(data1$target,p = 0.7,times = 1,list = FALSE)

traindata = data1[split,]
testdata = data1[-split,]


# ReSampling Techniques  as the Data is imbalanced i.e in target varaible Class 1 is only 10% and Class 0 is 90%   


dim(traindata)

table(traindata$target)

# Re- sampling function used balanced sampling technique
library(ROSE)
new_traindata = ovun.sample(target ~ metric1 + metric2 + metric6 +  
                    metric4 + metric5 + binary_var1 + device + binary_var2 + 
                    visited_page1 + visited_page3 + visited_page4 + page6_top + 
                    visited_page6,data=traindata,method = "both",p=0.5,seed = 200,N=243489)$data


table(new_traindata$target)

# ------------------- Final Model using XGBoost ---------------------------#

# Final Model is created using XGBoost with on the new dataset having the balanced classes. 
library(caret)
library(plyr)

new_traindata$target = as.factor(new_traindata$target)

params = trainControl(method = "repeatedcv",number  = 2 ,search = "random",classProbs = FALSE,verboseIter = T)

model6 = train(target ~ meftric1 + metric2 + metric6 +  
                 metric4 + metric5 + binary_var1 + device + binary_var2 + 
                 visited_page1 + visited_page3 + visited_page4 + page6_top + 
                 visited_page6, data = new_traindata,method = "xgbTree",objective = "binary:logistic",
               trControl=params,verbose=T, tuneLength = 10)

summary(model6)


# Prediction for Training dataset
pred_train_data=predict(model6,new_traindata,type = "prob")
new_traindata$pred_prob = pred_train_data$`1`


# Predictions for Test Dataset 
pred_test_data=predict(model6,testdata,type = "prob")
testdata$pred_prob = pred_test_data$`1`


# ------------------- Final Model Created using XGBoost ---------------------#



#-------------------------------- Evalution Metrics ---------------------------------#


# Decile Analysis

# decile for training (can also find the threshold probability according to business domains)
deloc_train = quantile(new_traindata$pred_prob,probs = seq(0.1,0.9,by=0.1))
new_traindata$decile = findInterval(new_traindata$pred_prob,c(-Inf,deloc_train,Inf))
new_traindata$decile = as.factor(new_traindata$decile)
library(dplyr)
library(varhandle)
new_traindata$target = unfactor(new_traindata$target)
gr_tr = group_by(new_traindata,decile)
decile_train = summarise(gr_tr,counts = n(),max_prob = max(pred_prob),min_prob = min(pred_prob),count_ones=sum(target),count_zeros = counts - count_ones)
decile_train  = data.frame(decile_train)
decile_train = mutate(decile_train,ones_percentage = (count_ones/sum(count_ones)*100))
decile_train = mutate(decile_train,ones_cumilative = cumsum(ones_percentage))
decile_train


#decile for test (can also find the threshold probability according to business domains)
deloc_test = quantile(testdata$pred_prob,probs = seq(0.1,0.9,by=0.1))
testdata$decile = findInterval(testdata$pred_prob,c(-Inf,deloc_train,Inf))
testdata$decile = as.factor(testdata$decile)
library(dplyr)
library(varhandle)
testdata$target = unfactor(testdata$target)
gr_tst = group_by(testdata,decile)
decile_test = summarise(gr_tst,counts = n(),max_prob = max(pred_prob),min_prob = min(pred_prob),count_ones=sum(target),count_zeros = counts - count_ones)
decile_test  = data.frame(decile_test)
decile_test = mutate(decile_test,ones_percentage = (count_ones/sum(count_ones)*100))
decile_test = mutate(decile_test,ones_cumilative = cumsum(ones_percentage))
decile_test

write.csv(decile_test,file = "decile_test.csv")


# KS Analysis 

# KS Analysis for Training Dataset
KS_train=decile_train
library(dplyr)
KS_train = KS_train[order(KS_train$decile,decreasing = T),]
KS_train = mutate(KS_train,ones_percentage = (count_ones/sum(count_ones)*100))
KS_train = mutate(KS_train,ones_cumilative = cumsum(ones_percentage))
KS_train = mutate(KS_train,zeros_percentage = (count_zeros/sum(count_zeros)*100))
KS_train = mutate(KS_train,zeros_cumilative = cumsum(zeros_percentage))
KS_train = mutate(KS_train,KS = (ones_cumilative - zeros_cumilative))
KS_train = arrange(KS_train,decile)
KS_train

write.csv(KS_train,file = "KS_train.csv")

# KS Analysis for Test Dataset
KS_test=decile_test
library(dplyr)
KS_test = KS_test[order(KS_test$decile,decreasing = T),]
KS_test = mutate(KS_test,ones_percentage = (count_ones/sum(count_ones)*100))
KS_test = mutate(KS_test,ones_cumilative = cumsum(ones_percentage))
KS_test = mutate(KS_test,zeros_percentage = (count_zeros/sum(count_zeros)*100))
KS_test = mutate(KS_test,zeros_cumilative = cumsum(zeros_percentage))
KS_test = mutate(KS_test,KS = (ones_cumilative - zeros_cumilative))
KS_test = arrange(KS_test,decile)
KS_test

write.csv(KS_test,file = "KS_test.csv")

# Confusion Metrics for Accuracy check

# Confusion Matrix for Training Dataset
library(caret)
new_traindata$predicted_ones = ifelse(new_traindata$pred_prob>0.50,1,0)
new_traindata$predicted_ones = as.factor(new_traindata$predicted_ones)
new_traindata$target = as.factor(new_traindata$target)
library(caret)
confusionMatrix(new_traindata$predicted_ones,new_traindata$target,positive = "1")

# Confusion Matrix for Test Dataset
testdata$predicted_ones = ifelse(testdata$pred_prob>0.50,1,0)
testdata$predicted_ones = as.factor(testdata$predicted_ones)
testdata$target = as.factor(testdata$target)
library(caret)
confusionMatrix(testdata$predicted_ones,testdata$target,positive = "1")



#ROC curve
library(ROCR)
z = prediction(new_traindata$pred_prob,new_traindata$target)
i = performance(z,"tpr","fpr")
plot(i)
summary(i)


#AUROC values for Train and Test dataset
library(InformationValue)

#For Train 
InformationValue::AUROC(new_traindata$target,new_traindata$pred_prob)

#For Test dataset
InformationValue::AUROC(testdata$target,testdata$pred_prob)


#Sensitivity for Training Dataset
MLmetrics::Sensitivity(new_traindata$target,new_traindata$predicted_ones,positive = "1")

#Sensitivity for Test Dataset
MLmetrics::Sensitivity(testdata$target,testdata$predicted_ones,positive = "1")


#F1 Score
# For Training Data
library(MLmetrics)
F1_Score(y_pred = new_traindata$predicted_ones, y_true = new_traindata$target, positive = "1")


# For Test Data
library(MLmetrics)
F1_Score(y_pred = testdata$predicted_ones, y_true = testdata$target, positive = "1")


# KS plot
# For Training Dataset
InformationValue::ks_plot(new_traindata$target,new_traindata$pred_prob)
#For Test Dataset
InformationValue::ks_plot(testdata$target,testdata$pred_prob)

