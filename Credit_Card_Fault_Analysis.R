library(nlme)
require(caTools)
library(car)
library(plyr)
library("caret")

  #file link -  for the dataset
  link<-paste("C:/Users/jaide/Documents/default-of-credit-card-clients-dataset/","UCI_Credit_Card",".csv",sep="")
  
  #link to save correlation matrix output in file
  link2<-paste("C:/Users/jaide/Documents/default-of-credit-card-clients-dataset/","cor",".csv",sep="")
  
  #read dataset into  a frame
  dataset<-read.csv(link,header=TRUE)
  
  
  
  #split in training and test dataset 
  set.seed(101) 
  sample = sample.split(dataset, SplitRatio = .75)
  train = subset(dataset, sample == TRUE)
  test  = subset(dataset, sample == FALSE)
  
  tcount<-count(dataset,vars="default.payment.next.month")
  tc<-(sum(tcount[tcount$default.payment.next.month==1,])/sum(tcount))*100
  print(paste("Percentage defaulters in original dataset: ",tc))
  
  
  tcount<-count(train,vars="default.payment.next.month")
  tc<-(sum(tcount[tcount$default.payment.next.month==1,])/sum(tcount))*100
  print(paste("Percentage defaulters in Training dataset: ",tc))
  
  tcount<-count(test,vars="default.payment.next.month")
  tc<-(sum(tcount[tcount$default.payment.next.month==1,])/sum(tcount))*100
  print(paste("Percentage defaulters in Test dataset: ",tc))
  
  ##"Percentage defaulters in original dataset:  22.1225959134695"
  ##"Percentage defaulters in Training dataset:  21.9619462061942"
  ##"Percentage defaulters in Test dataset:  22.5449351267706"
  
  ##As shown, the percentage split of 0's vs 1's in the original
  ##and training/test datasets is kept constant at 20-25%.
  
  
  #Creating an additional split for testing
  set.seed(159) 
  sample = sample.split(dataset, SplitRatio = .75)
  train1 = subset(dataset, sample == TRUE)
  test1  = subset(dataset, sample == FALSE)
  
  t1count<-count(test1,vars="default.payment.next.month")
  tc1<-(sum(t1count[t1count$default.payment.next.month==1,])/sum(t1count))*100
  print(paste("Percentage defaulters in Test dataset: ",tc1))
  
  #Original model
  Model_All_Var= glm(train$default.payment.next.month~.,data=train,family = binomial)
  
  #summary
  summary(Model_All_Var)
  #AIC: 20117
  
  #append predictions performed on test dataset to test dataset
  test$defaultpred <- predict.glm(Model_All_Var,newdata=test,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.75,]
  
  #Comparing predicted to actual observations to calculate an accuracy percentage
  gcount<-count(gtr,vars="default.payment.next.month")
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  #Refactoring predictions for confusionMatrix
  test$defaultpred[test$defaultpred>=0.75]<-1
  test$defaultpred[test$defaultpred<0.75]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  #stepwise search for elimination
  
  step(Model_All_Var,direction="both")
  
  #Call:  glm(formula = train$default.payment.next.month ~ LIMIT_BAL + 
  #             SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + 
  #             PAY_4 + BILL_AMT1 + BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
  #             PAY_AMT4 + PAY_AMT5 + PAY_AMT6, family = binomial, data = train)
  
  
  
  #model 2 with only significant terms
  Model_Sig_Var= glm(formula = train$default.payment.next.month ~ LIMIT_BAL + 
            SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + 
            PAY_4 + BILL_AMT1 + BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
            PAY_AMT4 + PAY_AMT5 + PAY_AMT6, family = binomial, data = train)
  
  #summary
  summary(Model_Sig_Var)
  #AIC: 20106
  
  #append predictions performed on test dataset to test dataset
  test$defaultpred <- predict.glm(Model_Sig_Var,newdata=test,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.75,]
  
  
  #Comparing predicted to actual observations to calculate an accuracy percentage
  gcount<-count(gtr,vars="default.payment.next.month")
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  

  
  
  #significant terms in a data frame
  df<-data.frame(dataset$LIMIT_BAL,dataset$SEX,dataset$EDUCATION,dataset$MARRIAGE,dataset$AGE,
                 dataset$PAY_0,dataset$PAY_2,dataset$PAY_3,dataset$PAY_4,dataset$BILL_AMT1,
                 dataset$BILL_AMT5,dataset$PAY_AMT1,dataset$PAY_AMT2,dataset$PAY_AMT3,
                 dataset$PAY_AMT4,dataset$PAY_AMT5,dataset$PAY_AMT6)
  
  # finding correlation between statistically significant variables
  write.csv(cor(df),link2) 
  
  #Understanding if change in one term affects the relation between its correlated term and the output
  
  coplot(defaultpred ~ PAY_0 | LIMIT_BAL, data = test) #considerable
  
  coplot(defaultpred ~ PAY_4 | LIMIT_BAL, data = test)#considerable
  
  coplot(defaultpred ~ PAY_2 | LIMIT_BAL, data = test)#dontknow
  
  coplot(defaultpred ~ PAY_3 | LIMIT_BAL, data = test)#dontknow
  
  coplot(defaultpred ~ AGE | MARRIAGE, data = test) #not considerable
  
  coplot(defaultpred ~ PAY_0 | AGE, data = test)#Not considerable
  
  coplot(defaultpred ~ PAY_0 | PAY_2, data = test)# Considerable
  
  coplot(defaultpred ~ PAY_2 | PAY_3, data = test)#considerable
  
  coplot(defaultpred ~ PAY_3 | PAY_4, data = test)#considerable
  
  coplot(defaultpred ~ PAY_2 | PAY_4, data = test)#considerable
  
  coplot(defaultpred ~ AGE | PAY_0, data = test)#not considerble
  
  coplot(defaultpred ~ BILL_AMT1 | BILL_AMT5, data = test)#considerable
  
  coplot(defaultpred ~ PAY_0 | PAY_2*PAY_3*PAY_4, data = test)#dont know
  
  coplot(defaultpred ~ LIMIT_BAL | PAY_0*PAY_2*PAY_3*PAY_4, data = test)#dont know
  
  #Refactoring predictions for confusionMatrix
  test$defaultpred[test$defaultpred>=0.75]<-1
  test$defaultpred[test$defaultpred<0.75]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)

    #model with interaction terms as per inferred correlations
    Model_Int_Terms= glm(formula = train$default.payment.next.month ~ LIMIT_BAL*PAY_0 + LIMIT_BAL*PAY_4 +
            SEX + EDUCATION + MARRIAGE + AGE + PAY_0*PAY_2 + PAY_2*PAY_3 + PAY_2*PAY_4+ PAY_3*PAY_4 + BILL_AMT1*BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
            PAY_AMT4 + PAY_AMT5 + PAY_AMT6, family = binomial, data = train)
  
  summary(Model_Int_Terms)
  #AIC: 19869
  step(Model_Int_Terms,direction = "both")
  
  #append predictions performed on test dataset to test dataset
  test$defaultpred <- predict.glm(Model_Int_Terms,newdata=test,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.75,]
  
  
  #Comparing predicted to actual observations to calculate an accuracy percentage
  gcount<-count(gtr,vars="default.payment.next.month")
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  #Refactoring predictions for confusionMatrix
  test$defaultpred[test$defaultpred>=0.75]<-1
  test$defaultpred[test$defaultpred<0.75]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  #Model considering an interaction between all significant Pay terms
  Model_PayInt= glm(formula = train$default.payment.next.month ~ LIMIT_BAL*PAY_0 + LIMIT_BAL*PAY_4 +
            SEX + EDUCATION + MARRIAGE + AGE + PAY_0*PAY_2*PAY_3*PAY_4 + BILL_AMT1*BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
            PAY_AMT4 + PAY_AMT5 + PAY_AMT6, family = binomial, data = train)
  
  summary(Model_PayInt)
  #AIC: 19521
  
  #append predictions performed on test dataset to test dataset
  test$defaultpred <- predict.glm(Model_PayInt,newdata=test,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.75,]
  
  
  #Comparing predicted to actual observations to calculate an accuracy percentage
  gcount<-count(gtr,vars="default.payment.next.month")
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  #Refactoring predictions for confusionMatrix
  test$defaultpred[test$defaultpred>=0.75]<-1
  test$defaultpred[test$defaultpred<0.75]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  #Model considering an interaction between Credit Limit and the Pay terms
  Model_LimitPay_Int= glm(formula = train$default.payment.next.month ~ SEX + EDUCATION + MARRIAGE + AGE + LIMIT_BAL* PAY_0*PAY_2*PAY_3*PAY_4 + BILL_AMT1*BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
            PAY_AMT4 + PAY_AMT5 + PAY_AMT6, family = binomial, data = train)
  
  summary(Model_LimitPay_Int)
  #AIC: 19452
  step(Model_LimitPay_Int,direction = "both")
  
  #append predictions performed on test dataset to test dataset
  test$defaultpred <- predict.glm(Model_LimitPay_Int,newdata=test,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.75,]
  
  
  #Comparing predicted to actual observations to calculate an accuracy percentage
  gcount<-count(gtr,vars="default.payment.next.month")
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  #Refactoring predictions for confusionMatrix
  test$defaultpred[test$defaultpred>=0.75]<-1
  test$defaultpred[test$defaultpred<0.75]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  #Attempt to filter predictors just on inference
  Model_Overfit= glm(formula = train$default.payment.next.month ~ SEX + EDUCATION + MARRIAGE + AGE + LIMIT_BAL: PAY_0:PAY_2:PAY_3:PAY_4 + BILL_AMT1:BILL_AMT5 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
            PAY_AMT4 + PAY_AMT5 + PAY_AMT6, family = binomial, data = train)
  
  summary(Model_Overfit)
  #AIC: 22189
  
  #append predictions performed on test dataset to test dataset
  test$defaultpred <- predict.glm(Model_Overfit,newdata=test,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.75,]
  
  
  #Comparing predicted to actual observations to calculate an accuracy percentage
  gcount<-count(gtr,vars="default.payment.next.month")
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  #Refactoring predictions for confusionMatrix
  test$defaultpred[test$defaultpred>=0.75]<-1
  test$defaultpred[test$defaultpred<0.75]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  #########################Cross Validation for threshold =0.50#############################
  ####Fault Model####
  test$defaultpred <- predict.glm(Model_All_Var,newdata=test,type = 'response')
  
  #set threshold as 0.50 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.50,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.50]<-1
  test$defaultpred[test$defaultpred<0.50]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_Sig_Var####
  test$defaultpred <- predict.glm(Model_Sig_Var,newdata=test,type = 'response')
  
  #set threshold as 0.50 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.50,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.50]<-1
  test$defaultpred[test$defaultpred<0.50]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_Int_Terms####
  test$defaultpred <- predict.glm(Model_Int_Terms,newdata=test,type = 'response')
  
  #set threshold as 0.50 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.50,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.50]<-1
  test$defaultpred[test$defaultpred<0.50]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_PayInt####
  test$defaultpred <- predict.glm(Model_PayInt,newdata=test,type = 'response')
  
  #set threshold as 0.50 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.50,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.50]<-1
  test$defaultpred[test$defaultpred<0.50]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_LimitPay_Int####
  test$defaultpred <- predict.glm(Model_LimitPay_Int,newdata=test,type = 'response')
  
  #set threshold as 0.50 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.50,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.50]<-1
  test$defaultpred[test$defaultpred<0.50]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  
  ####Model_Overfit ####
  test$defaultpred <- predict.glm(Model_Overfit,newdata=test,type = 'response')
  
  #set threshold as 0.50 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.50,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.50]<-1
  test$defaultpred[test$defaultpred<0.50]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  #########################Cross Validation for threshold=0.90#############################
  ####Fault Model####
  test$defaultpred <- predict.glm(Model_All_Var,newdata=test,type = 'response')
  
  #set threshold as 0.90 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.90,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.90]<-1
  test$defaultpred[test$defaultpred<0.90]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_Sig_Var####
  test$defaultpred <- predict.glm(Model_Sig_Var,newdata=test,type = 'response')
  
  #set threshold as 0.90 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.90,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.90]<-1
  test$defaultpred[test$defaultpred<0.90]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_Int_Terms####
  test$defaultpred <- predict.glm(Model_Int_Terms,newdata=test,type = 'response')
  
  #set threshold as 0.90 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.90,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.90]<-1
  test$defaultpred[test$defaultpred<0.90]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_PayInt####
  test$defaultpred <- predict.glm(Model_PayInt,newdata=test,type = 'response')
  
  #set threshold as 0.90 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.90,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.90]<-1
  test$defaultpred[test$defaultpred<0.90]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  ####Model_LimitPay_Int####
  test$defaultpred <- predict.glm(Model_LimitPay_Int,newdata=test,type = 'response')
  
  #set threshold as 0.90 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.90,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.90]<-1
  test$defaultpred[test$defaultpred<0.90]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  
  ####Model_Overfit ####
  test$defaultpred <- predict.glm(Model_Overfit,newdata=test,type = 'response')
  
  #set threshold as 0.90 probability
  gtr<-test[as.numeric(test$defaultpred)>=0.90,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test$defaultpred[test$defaultpred>=0.90]<-1
  test$defaultpred[test$defaultpred<0.90]<-0
  confusionMatrix(test$defaultpred, test$default.payment.next.month)
  
  
  
  #########################Overfit Validation(Testing on new validation set)#############################
  ####Fault Model####
  test1$defaultpred <- predict.glm(Model_All_Var,newdata=test1,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test1[as.numeric(test1$defaultpred)>=0.75,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test1$defaultpred[test1$defaultpred>=0.75]<-1
  test1$defaultpred[test1$defaultpred<0.75]<-0
  confusionMatrix(test1$defaultpred, test1$default.payment.next.month)
  
  ####Model_Sig_Var####
  test1$defaultpred <- predict.glm(Model_Sig_Var,newdata=test1,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test1[as.numeric(test1$defaultpred)>=0.75,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test1$defaultpred[test1$defaultpred>=0.75]<-1
  test1$defaultpred[test1$defaultpred<0.75]<-0
  confusionMatrix(test1$defaultpred, test1$default.payment.next.month)
  
  ####Model_Int_Terms####
  test1$defaultpred <- predict.glm(Model_Int_Terms,newdata=test1,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test1[as.numeric(test1$defaultpred)>=0.75,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test1$defaultpred[test1$defaultpred>=0.75]<-1
  test1$defaultpred[test1$defaultpred<0.75]<-0
  confusionMatrix(test1$defaultpred, test1$default.payment.next.month)
  
  ####Model_PayInt####
  test1$defaultpred <- predict.glm(Model_PayInt,newdata=test1,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test1[as.numeric(test1$defaultpred)>=0.75,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test1$defaultpred[test1$defaultpred>=0.75]<-1
  test1$defaultpred[test1$defaultpred<0.75]<-0
  confusionMatrix(test1$defaultpred, test1$default.payment.next.month)
  
  ####Model_LimitPay_Int####
  test1$defaultpred <- predict.glm(Model_LimitPay_Int,newdata=test1,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test1[as.numeric(test1$defaultpred)>=0.75,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test1$defaultpred[test1$defaultpred>=0.75]<-1
  test1$defaultpred[test1$defaultpred<0.75]<-0
  confusionMatrix(test1$defaultpred, test1$default.payment.next.month)
  
  
  
  ####Model_Overfit ####
  test1$defaultpred <- predict.glm(Model_Overfit,newdata=test1,type = 'response')
  
  #set threshold as .75 probability
  gtr<-test1[as.numeric(test1$defaultpred)>=0.75,]
  
  
  gcount<-count(gtr,vars="default.payment.next.month")
  
  acc<-(sum(gcount[gcount$default.payment.next.month==1,])/sum(gcount))*100
  print(paste("Accuracy:",acc))
  
  test1$defaultpred[test1$defaultpred>=0.75]<-1
  test1$defaultpred[test1$defaultpred<0.75]<-0
  confusionMatrix(test1$defaultpred, test1$default.payment.next.month)
