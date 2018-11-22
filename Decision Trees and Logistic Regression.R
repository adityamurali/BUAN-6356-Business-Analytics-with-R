library(readxl)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(plyr)
library(VIM)

#Read data from the excel file into R
data <- read_excel("organics.xlsx")

#Find the total number of rows in the dataset
numrow <- length(data$ID)

#Drop the variables that are to be rejected
data$ID <- NULL
data$DemCluster <- NULL
data$TargetAmt <- NULL

#Process the data to impute missing values for categorical variables
imputeddata <- kNN(data, variable = c("DemClusterGroup","DemGender","DemReg","DemTVReg"), k=6)

#Process the data to impute missing values for numerical variables
imputeddata <- kNN(imputeddata, variable = c("DemAffl","DemAge","PromTime"), k=6)

#Set the seed
set.seed(42)

#kNN algorithm creates extra columns for each variable that is imputed, where it shows a T/F value depending on whether a value in the column was imputed or not.
imputeddata <- imputeddata[,-c(11:17)]

#Randomize the data before partioning it
rand <- runif(nrow(imputeddata)) #22223 random numbers are created
datarand <- imputeddata[order(rand), ]
head(datarand)

#Q1: Partition the data into training(50%) and test(50%) 
datatrain <- datarand[1:ceil(numrow/2), ]
datatest <- datarand[(ceil(numrow/2)+1):numrow, ]

#Q2: Distribution of target variable - TargetBuy

#Frequency Distribution
count(imputeddata$TargetBuy)

#Histogram
hist(imputeddata$TargetBuy)

#Relative Frequency Distribution - Pie Chart
#First find the number of 1s
n <- length(subset(data$TargetBuy, data$TargetBuy == 1))
#Then find the number of 0s
n0 <- length(subset(data$TargetBuy, data$TargetBuy == 0))

slices <- c((n*100/numrow),(n0*100/numrow))
lbls <- c("1 - Yes","0 - No")
pie(slices, labels = lbls, main = "Proportion of customers who purchased organic products")

#Proportion of customers who purchased organic products?

#Total number of purchases = numrow
#Thus, proportion of 1s = n/numrow
#Proportion = 
n/numrow * 100

######################DECISION TREE#############################################

#Q4: Set seed = 42
set.seed(42)

#Build the decision tree
organictree <- rpart(TargetBuy ~ ., data = datatrain, method = "class")
organictree

#Create a decision tree
rpart.plot(organictree, cex = 0.82)

#Test the performance of this model

#First look at training data set
#Create a prediction using the decision tree
datatrain$pred <- predict(organictree, datatrain, type = "class") 

#Create a confusion matrix
confMatrix <- table(Actual = datatrain$TargetBuy, Predicted = datatrain$pred)

truePos <- confMatrix[2,2]
falseNeg <- confMatrix[2,1]
falsePos <- confMatrix[1,2]
trueNeg <- confMatrix[1,1]

confMatrix

accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
accuracy

sensitivity <- truePos/(truePos + falseNeg)
sensitivity

specificity <- trueNeg/(falsePos + trueNeg)
specificity

falsePosRate <- falsePos/(falsePos + trueNeg)
falsePosRate

falseNegRate <- falseNeg/(truePos + falseNeg)
falseNegRate

precision <- truePos/(truePos + falsePos)
precision

#Now look at test data set
#Create a prediction using the decision tree on the test data set
datatest$pred <- predict(organictree, datatest, type = "class") 

#Create a confusion matrix
confMatrix2 <- table(Actual = datatest$TargetBuy, Predicted = datatest$pred)
truePos2 <- confMatrix2[2,2]
falseNeg2 <- confMatrix2[2,1]
falsePos2 <- confMatrix2[1,2]
trueNeg2 <- confMatrix2[1,1]
confMatrix2


accuracy2 <- (truePos2 + trueNeg2)/(truePos2 + falseNeg2 + falsePos2 + trueNeg2)
accuracy2

sensitivity2 <- truePos2/(truePos2 + falseNeg2)
sensitivity2

specificity2 <- trueNeg2/(falsePos2 + trueNeg2)
specificity2

falsePosRate2 <- falsePos2/(falsePos2 + trueNeg2)
falsePosRate2

falseNegRate2 <- falseNeg2/(truePos2 + falseNeg2)
falseNegRate2

precision2 <- truePos2/(truePos2 + falsePos2)
precision2


##############################LOGISTIC REGRESSION############################################

datatrain$pred <- NULL

#Building logistic regression on training data set
OrganicsTrainLogit <- glm(TargetBuy ~ DemAffl + DemAge + DemClusterGroup + DemGender + DemReg + DemTVReg + PromClass + PromSpend + PromTime, data = datatrain, family = binomial(link = "logit"))
summary(OrganicsTrainLogit)

#Drop the insignificant variables
OrganicsTrainLogit2 <- glm(TargetBuy ~ DemAffl + DemAge + DemGender , data = datatrain, family = binomial(link = "logit"))
summary(OrganicsTrainLogit2)

### Test Model Performance - Creates a 2X2 confusion matrix and associated metrics
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(model))))
  writeLines(paste("Target:", deparse(substitute(target))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrixF <- table(Actual = target, Predicted = dataset$pred)
  truePosF <- confMatrixF[2,2]
  falseNegF <- confMatrixF[2,1]
  falsePosF <- confMatrixF[1,2]
  trueNegF <- confMatrixF[1,1]
  print(confMatrixF)
  writeLines("\n\n")
  
  accuracyF <- (truePosF + trueNegF)/(truePosF + falseNegF + falsePosF + trueNegF)
  sensitivityF <- truePosF/(truePosF + falseNegF)
  specificityF <- trueNegF/(falsePosF + trueNegF)
  falsePosRateF <- falsePosF/(falsePosF + trueNegF)
  falseNegRateF <- falseNegF/(truePosF + falseNegF)
  precisionF <- truePosF/(truePosF + falsePosF)
  
  writeLines(paste("Accuracy:", round(accuracyF, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivityF, digits = 4)))
  writeLines(paste("Specificity:", round(specificityF, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRateF, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRateF, digits = 4)))
  writeLines(paste("Precision:", round(precisionF, digits = 4)))
}

### Log Likelihood - Computes log likelihood 
llh <- function(y, py) {
  sum(y * log(py) + (1-y) * log(1-py))
}

#Calculate psuedo R square
datatrain$probsurv <- predict(OrganicsTrainLogit2, newdata = datatrain, type = "response")
resid.dev <- 2 * llh(datatrain$TargetBuy, datatrain$probsurv)
null.dev <- 2 * llh(datatrain$TargetBuy, mean(datatrain$TargetBuy))
pr2 <- 1-(resid.dev/null.dev)
paste("Psuedo R2: ", pr2)

#Convert probability in to a 0 or 1 prediction by rounding (cutoff = 0.5)
head(datatrain)
datatrain$logitpred <- round(datatrain$probsurv)
head(datatrain)

Logit_Regression_Perf <- testModelPerformance(OrganicsTrainLogit2, datatrain, datatrain$TargetBuy, datatrain$logitpred)

#Calculate Chi-Square
devdiff <- with(OrganicsTrainLogit2, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(OrganicsTrainLogit2, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

#Build confidence intervals
confint.default(OrganicsTrainLogit2) 

#Calculate odd sratio
exp(coef(OrganicsTrainLogit2))

#Evaluate performance of logistic regression on test data set

datatest$probsurv <- predict(OrganicsTrainLogit2, newdata = datatest, type = "response")
datatest$logitpred <- round(datatest$probsurv)
Logit_Regression_Perf_Test <- testModelPerformance(OrganicsTrainLogit2, datatest, datatest$TargetBuy, datatest$logitpred)

#Visualize the plot output of logistic regression model built on the training data set
par(mfrow=c(2,2))
plot(OrganicsTrainLogit2)
