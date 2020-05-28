install.packages("AER")
library(AER)
library(plyr)

creditcard1 <- read.csv(file.choose())
summary(creditcard1)
str(creditcard1)
table(creditcard1$card)

#Change Character to numeric
creditcard1$owner <- as.factor(revalue(creditcard1$owner,c("yes"=1,"no"=0)))
creditcard1$selfemp <- as.factor(revalue(creditcard1$selfemp,c("yes"=1,"no"=0)))
attach(creditcard1)
View(creditcard1)

sum(is.na(creditcard1))
dim(creditcard1)
colnames(creditcard1)

##model linear regression
LM1<- lm(card~.,data = creditcard1)
### predict
pred1 <- predict(LM1,creditcard1)
plot(creditcard1$card,pred1)
plot(pred1)

### logistic regression
max_log1<- glm(formula = card~., data = creditcard1, family = "binomial")
summary(max_log1)
## odd ratio
exp(coef(max_log1))

# confusio matrix table
prob <- predict(max_log1, creditcard1,type = "response")
prob

# confusion matrix and considering the threshold value as 0.5
confusion1  <-table(prob>0.5,creditcard1$card)
confusion1

# model accuracy
accuracy <- sum(diag(confusion1)/sum(confusion1))
accuracy  #### 87%

# creating the empty vectors based on the threshold vlaue
predice_values <- NULL
yes_no < NULL

predice_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,1,0)

#### creating new column to store the new value
creditcard1[,"prob"] <- prob
creditcard1[,"predic_values"] <- predice_values
creditcard1[,"yes_no"] <- yes_no
View(creditcard1)

table(creditcard1$card,creditcard1$predic_values)

# ROCR curve
library(ROCR)
ROCpred<- prediction(prob,creditcard1$card)
ROCpref<- performance(ROCpred,'tpr','fpr')
str(ROCpref)
plot(ROCpref,colorize = T,text.adj=c(-0.2,1.7))

## geting cutt off or threshold value 
ROC_cutoff <- data.frame(cut_off = ROCpref@alpha.values[[1]],fpr=ROCpref@x.values,tpr=ROCpref@y.values)
colnames(ROC_cutoff) <- c("cut_off","FPR","TPR")
View(ROC_cutoff)

library(dplyr)
ROC_cutoff$cut_off < round(ROC_cutoff$cut_off,5)
ROC_cutoff <- arrange(ROC_cutoff,desc(TPR))
View(ROC_cutoff)
