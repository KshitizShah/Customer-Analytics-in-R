#Customer Analytics Group Member: Kshitiz Shah, Zedan Han

#clearing workspace
rm(list = ls())

#setting working directory to dataset path
setwd("D:\\JHU\\JHU-Spring 1\\Customer Analytics\\WEEK 2\\HW")

#libraries/packages
library(caret)
library(pROC)

#loading data

blue_apron_main <- read.csv("D2.3 Blue apron.csv")

#exploring data
blue_apron <- blue_apron_main
nrow(blue_apron)
dim(blue_apron)
View(blue_apron)
summary(blue_apron)

#checking structure of data set columns
str(blue_apron)

#data does not have NULL/Missing Values
table(is.na(blue_apron$churn))

set.seed(123)
#setting up data interaction terms
blue_apron$RatingPartySize =blue_apron$rating*blue_apron$partysize
blue_apron$RatingUrban =blue_apron$rating*blue_apron$urban
blue_apron$UrbanPartySize =blue_apron$urban*blue_apron$partysize
blue_apron$UrbanTenure =blue_apron$urban*blue_apron$tenure

#data partitioning
idx = sample(2,nrow(blue_apron),replace=TRUE,prob=c(.7,.3)) 
train = blue_apron[idx==1,]
test  = blue_apron[idx==2,]

################# Task_1 #################

#Specification 1: trained on training data
model_1 <- glm(churn~tenure+rating+partysize+urban+menu+frequency,
               family = "binomial",data=train)
summary(model_1)

#calculating probabilities
model_1_probablity = predict(model_1,test,type="response") 
roc(test$churn~model_1_probablity, plot = TRUE, print.auc = TRUE)

#Specification 2: 
model_2 <- glm(churn~tenure+rating+partysize+urban+menu+frequency+RatingPartySize
               +RatingUrban+UrbanTenure+UrbanPartySize,family = "binomial",data=train)
summary(model_2)

#calculating probabilities for model 2
model_2_probablity = predict(model_2,test,type="response") 
roc(test$churn~model_2_probablity, plot = TRUE, print.auc = TRUE)

# in this case we see a increase in AUC for the second specification, hence we choose the 2nd model 
#Specification 1: trained on training data
final_model <- glm(churn~tenure+rating+partysize+urban+menu+frequency+RatingPartySize
                   +RatingUrban+UrbanTenure+UrbanPartySize ,family = "binomial",data=blue_apron)
summary(final_model)

probability_prediction = predict(final_model,blue_apron,type="response") 

# we see that most of the blue apron customers are predicted to have low probabilities to churn.
hist(probability_prediction)

################# Task_2 #################

#Specification 1: trained on training data
model_3 <- glm(monthlyaddons~tenure+rating+partysize+urban+menu+frequency,
               family="gaussian",data=train)
summary(model_3)

#calculating probabilities
model_3_pred = predict(model_3,test) 
hist(model_3_pred)
postResample(pred = model_3_pred, obs = test$monthlyaddons) 

#Specification 2: 
model_4 <- glm(monthlyaddons~tenure+rating+partysize+urban+menu+frequency
               +RatingPartySize+RatingUrban+UrbanTenure+UrbanPartySize,
               family="gaussian",data=train)
summary(model_4)

#calculating probabilities for model 2
model_4_pred= predict(model_4,test) 
hist(model_4_pred)
postResample(pred = model_4_pred, obs = test$monthlyaddons) 


#We will choose model 2 which has a smaller RMSE, comparing to model 1
#-> re-estimate using the full dataset  
lm_final = glm(monthlyaddons~tenure+rating+partysize+urban+menu+frequency
               +RatingPartySize+RatingUrban+UrbanTenure+UrbanPartySize,
               family="gaussian",data=blue_apron)   
expected_addons = predict(lm_final,blue_apron) 
#We can see that most of the blue apron customers are predicted to spend 
#around $9 on add-ons to plan during sample period.
hist(expected_addons)

################# Task 3 Export ################# 

export_df = cbind(blue_apron,probability_prediction,expected_addons)
write.csv(export_df,"BlueApron_Evaluation_Input_Updated.csv")


