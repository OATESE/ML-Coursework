##
options(scipen = 10)
library(car)
library(MASS)
library(class)
library(pROC)
library(boot)
###Loading and Exploring Data###
data <- read.csv("labour.csv")
attach(data)

head(data)
summary(data)
str(data)


###[Question 1, 5 Marks]###
correlation_matrix <- cor(data) #Computing Correlation Matrix for all variables in data
top_correlated_vars <- names(sort((correlation_matrix["wage",]),decreasing=TRUE)[2:5]) ###Finding which 4 variables are the most correlated with wage
print(top_correlated_vars) # Education, Year, Marrige status and Experience are the variables most correlated, other than wage itself
pairs(data[c("wage",top_correlated_vars)]) #Producing Pairwise Scatter Plot, See PDF for interpretation


###[Question 2, 5 Marks]###
indicators <- c("black", "hisp", "married", "union", "occ1", "occ2", "occ3", "occ4", "occ5", "occ6", "occ7", "occ8", "occ9") #Create list of Indicator Variables
data[indicators] <- lapply(data[indicators], as.factor) #Convert Indicators to factor variables
str(data) #Check to see if they are now factors 
# Assuming 'married' is a factor variable and 'wage' is numeric

attach(data)
plot(married, wage,xlab = "Married", ylab = "Wage (Income earned £1000)",names = c("No", "Yes")) #Graph a box plot with the variable married on the x-axis and wage on the y-axis. 
#See PDF for Interpretation


###[Question 3, 8 Marks]###
lm.fit <- lm(wage ~ . - occ9,data) #Estimate a linear regression model with wage as DV and all other variables as IDV except occ9
summary(lm.fit) # Produce regression output
#See PDF for Answers to 3(a),3(b),3(c)


###[Question 4, 25 marks]###
lm.fit1 <- lm(wage ~ . - nr - hisp - occ8 -occ9,data) #Exclude nr, hisp, occ8 as they where not deemed significantly associated and exclude occ9 
summary(lm.fit1) #Produce regression output

###[4a]###
par(mfrow=c(2,2))
plot(lm.fit1) #See PDF for interpretation of plots 

###[4b]###
index.outlier <- which(rstudent(lm.fit1) > 3 | rstudent(lm.fit1) < -3) #identify observations with standardized residuals greater than 3 or less than -3.
length(index.outlier) #48 observations considered outlier
highlev_cutoff <- 3*(length(coef(lm.fit1)))/nobs(lm.fit1) #Highlev cuttof = 3*(p+1)/n, length(coef(lm.fit1))=p+1
index.highlev <- which(hatvalues(lm.fit1) > highlev_cutoff) #Identify observations with leverage values > Cuttoff 
length(index.highlev)#3 observation considered high leverage points 
#See PDF for inference on difference between High Leverage points and 

###[4c]###
vif(lm.fit1) #Use vif function from car package, to create variance inflation factors for all predictor variables
#See PDF for inference of VIFs 

###[4d]###
coef(lm.fit1)["hours"]  #Check to see if coefficient is negative 
#See PDF for explanation. 



###[Question 5, 37 marks]###
data$rich <- as.factor(ifelse(wage > mean(wage), 1, 0)) #Create Variable based on ifelse condition, and convert it to factor 
test <- data[year <= 1982, ]
train<- data[year > 1982, ]

###[5a]###
logit.fit <- glm(rich ~ educ + hours + exper + black,train, family = 'binomial')
summary(logit.fit) #Produce Regression output info

logit.pred <- predict(logit.fit, test, type = 'response') #predict probabilities for Rich on test data

logit.pred.40 <- rep(0, dim(test)[1]) #Create variable full of 0s, length = length test data
logit.pred.40[logit.pred > 0.4] <- 1 #Assign values of 1, if probability >0.4
table(logit.pred.40,test$rich) #Create confusion matrix with predicted in rows, actuall in collumns 
mean(logit.pred.40 != test$rich) #0.3834862 is test error rate for 0.4 cuttoff. Equivilant to (458+169)/1635

logit.pred.60 <- rep(0, dim(test)[1]) #Create variable full of 0s, length = length test data
logit.pred.60[logit.pred > 0.6] <- 1 #Assign values of 1, if probability >0.6
table(logit.pred.60,test$rich) #Create confusion matrix with predicted in rows, actuall in collumns 
mean(logit.pred.60 != test$rich) #0.3107034 is test error rate for 0.8 cuttoff. Equivilant to (127+381)/1635

logit.pred.80 <- rep(0, dim(test)[1]) #Create variable full of 0s, length = length test data
logit.pred.80[logit.pred > 0.8] <- 1 #Assign values of 1, if probability > 0.8
table(logit.pred.80,test$rich) #Create confusion matrix with predicted in rows, actuall in collumns 
mean(logit.pred.80 != test$rich) #0.2966361 test error rate for 0.8 cuttoff. Equivilant to (12+473)/1635

#Preffered Cuttoff is 0.8, it has the lowest overall test error. 

table(logit.pred.40,test$rich)[1, 2] / sum(table(logit.pred.40,test$rich)[,2 ]) # 0.4 false negative rate = False Negatives / Total Actual Positives
table(logit.pred.60,test$rich)[1, 2] / sum(table(logit.pred.60,test$rich)[,2 ]) # 0.6 false negative rate = False Negatives / Total Actual Positives
table(logit.pred.80,test$rich)[1, 2] / sum(table(logit.pred.80,test$rich)[,2 ]) # 0.8 false negative rate = False Negatives / Total Actual Positives
#0.4 has the lowest false negative error rate. See PDF for explanation why this may be the most preffered. 

###[5b]###
lda.fit <- lda(rich ~ educ + hours + exper + black, train) #Fit LDA model on training data 

lda.pred <- predict(lda.fit,test) #Predict Rich on test data with lda model

lda.pred.40 <- rep(0,dim(test)[1]) #Create variable full of 0s, length = length test data
lda.pred.40[lda.pred$posterior[,2] > 0.4] <- 1 #Assign value of 1 if posterior probability >0.4
table(lda.pred.40,test$rich) #Create confusion matrix 
mean(lda.pred.40 != test$rich) #Calculate overall test error rate = 0.3816514

lda.pred.60 <- rep(0,dim(test)[1]) #Create variable full of 0s, length = length test data
lda.pred.60[lda.pred$posterior[,2] > 0.6] <- 1 #Assign value of 1 if posterior probability >0.6
table(lda.pred.60,test$rich) #Create confusion matrix 
mean(lda.pred.60 != test$rich) #Calculate overall test error rate = 0.3094801

lda.pred.80 <- rep(0,dim(test)[1]) #Create variable full of 0s, length = length test data
lda.pred.80[lda.pred$posterior[,2] > 0.8] <- 1 #Assign value of 1 if posterior probability >0.6
table(lda.pred.80,test$rich) #Create confusion matrix 
mean(lda.pred.80 != test$rich) #Calculate overall test error rate = 0.2954128
#0.8 cuttoff has lowest overall test error rate for lda model


###[5c]###
qda.fit <- qda(rich ~ educ + hours + exper + black, train) #Fit QDA model on training data 

qda.pred <- predict(qda.fit,test) #Predict Rich on test data with qda model

qda.pred.40 <- rep(0,dim(test)[1]) #Create variable full of 0s, length = length test data
qda.pred.40[qda.pred$posterior[,2] > 0.4] <- 1 #Assign value of 1 if posterior probability >0.4
table(qda.pred.40,test$rich) #Create confusion matrix 
mean(qda.pred.40 != test$rich) #Calculate overall test error rate = 0.3321101

qda.pred.60 <- rep(0,dim(test)[1]) #Create variable full of 0s, length = length test data
qda.pred.60[qda.pred$posterior[,2] > 0.6] <- 1 #Assign value of 1 if posterior probability >0.4
table(qda.pred.60,test$rich) #Create confusion matrix 
mean(qda.pred.60 != test$rich) #Calculate overall test error rate = 0.3003058

qda.pred.80 <- rep(0,dim(test)[1]) #Create variable full of 0s, length = length test data
qda.pred.80[qda.pred$posterior[,2] > 0.8] <- 1 #Assign value of 1 if posterior probability >0.4
table(qda.pred.80,test$rich) #Create confusion matrix 
mean(qda.pred.80 != test$rich) #Calculate overall test error rate = 0.2892966
#0.8 cuttoff has lowest overall test error rate for qda model 

###[5d]###
set.seed(10)

knn.pred.1 <- knn(as.matrix(train), as.matrix(test), train$rich, k = 1) #Fit KNN model with k = 1 
table(knn.pred.1, test$rich) #Confusion matrix 
mean(knn.pred.1!= test$rich) #Calculate overall test error rate = 0.3492355

knn.pred.10 <- knn(as.matrix(train), as.matrix(test), train$rich, k = 10) #Fit KNN model with k = 10 
table(knn.pred.10, test$rich) #Confusion matrix 
mean(knn.pred.10!= test$rich) #Calculate overall test error rate = 0.3785933

knn.pred.20 <- knn(as.matrix(train), as.matrix(test), train$rich, k = 20) #Fit KNN model with k = 20
table(knn.pred.20, test$rich) #Confusion matrix 
mean(knn.pred.20!= test$rich) #Calculate overall test error rate = 0.4024465

knn.pred.50 <- knn(as.matrix(train), as.matrix(test), train$rich, k = 50) #Fit KNN model with k = 20
table(knn.pred.50, test$rich) #Confusion matrix 
mean(knn.pred.50!= test$rich) #Calculate overall test error rate = 0.4458716

knn.pred.100 <- knn(as.matrix(train), as.matrix(test), train$rich, k = 100) #Fit KNN model with k = 100
table(knn.pred.100, test$rich) #Confusion matrix 
mean(knn.pred.100!= test$rich) #Calculate overall test error rate = 0.4525994

knn.pred.200 <- knn(as.matrix(train), as.matrix(test), train$rich, k = 100) #Fit KNN model with k = 200
table(knn.pred.200, test$rich) #Confusion matrix 
mean(knn.pred.200!= test$rich) #Calculate overall test error rate = 0.4574924

#k=1 is prefered value, as it minimizes overall error rate 

###[5e]###
#See PDF for explanation as to why this AUC of ROC curves can determine the best method. 
logiROC<-roc(test$rich,logit.pred)
ldaROC<-roc(test$rich,as.numeric(lda.pred$class))
qdaROC <- roc(test$rich,as.numeric(qda.pred$class))
knnROC<-roc(test$rich,as.numeric(knn.pred.1))

ggroc(list(Logistic=logiROC,LDA = ldaROC,QDA = qdaROC,KNN = knnROC))
auc(logiROC)
auc(ldaROC)
auc(qdaROC)
auc(knnROC)

###[Question 6, 8 marks]### 
boot.fn <- function(data,index){
  coef(lda(rich ~ educ + hours + exper + black, data,subset = index))
}#Create function to get coefficient values from the LDA model , This function accepts a dataset and index values as parameters when running the function

boot.fn(data,1:dim(data)[1]) #Test function on full data-set as specified in quesiton
boot(data,boot.fn,1000) # Use the boot function to get 1000 bootstrap estimates for the coefficients and standard errors

###[Question 6, 8 marks]### 
cost <- function(rich, pi = 0) mean(abs(rich-pi) > 0.6) #Since Response is binary variable, create cost function with 0.6 cuttoff

glm.fit <- glm(rich ~ educ + hours + exper + black, data, family = 'binomial') #Fit Logistic model on full dataset as per question

LOOCV.err <- cv.glm(data, glm.fit, cost)$delta #calculate Leave one out cross validation model, this is esentially k fold with n folds
LOOCV.err

kfoldCV.err<- cv.glm(data, glm.fit, cost, K = 10)$delta #calculate 10 fold cross validation error 
kfoldCV.err
#See PDF for full comparison and discussion of LOOCV and Kfold CV estimates of test error





