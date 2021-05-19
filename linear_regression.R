###Simple Linear Rgegression

###i. Modeling Price of a House using Area

#Reading Hoousing Data with Area and Price
housing<-read.csv("data.csv")
#View(data)
#Building Simple Linear Regression (SLR) of Price on Area
model<-lm(price ~ area, data=housing)
summary(model)

model$coefficients
model$fitted.values
model$resid

###ii. Modeling Keynesian Consumption Function
#Reading Data
kcf<-read.csv("kcf.csv")
#Simple Linear Regression (SLR)
model<-lm(Consumption ~ Income, data=kcf)
summary(model)

###iii. Modeling Mileage of a Car using Displacement

#Simple Linear Regression (SLR)
View(mtcars)
model<-lm(mpg ~ disp, data=mtcars)
summary(model)

#Multiple Linear Regression (MLR)

###i. Modeling Mileage of a Car using Displacement, Horsepower and Rear Axle Ratio

model<-lm(mpg ~ disp + hp + drat, data = mtcars)
summary(model)

###Note that we can calculate the parameter estimates using the formula $\hat{\beta} = (X^TX)^{-1}X^Ty$ as follows:

y<-mtcars[,1]
y<-as.matrix(y)
X<-mtcars[,3:5]
X<-cbind(rep(1,dim(mtcars)[1]),as.matrix(X))

#install.packages("MASS")
library(MASS)
(solve(t(X) %*% X)) %*% t(X) %*% y

#MLR - Evaluating the formula for Beta using Code

#Reading Data
data<-read.csv("data1.csv")
model<-lm(price~area+bedrooms,data=data)
summary(model)

y<-data[,1]
y<-as.matrix(y)
X<-data[,2:4]
X<-as.matrix(X)

#install.packages("MASS")
library(MASS)
(solve(t(X) %*% X)) %*% t(X) %*% y

#studying interaction
data<-read.csv("data1.csv")
model<-lm(price~area+bedrooms+area/bedrooms,data=data)
summary(model)

#Cobb-Douglas Production Function
data<-read.csv("cdpf.csv")
model<-lm(lnY~lnL+lnK,data=data)
summary(model)

#Multicollinearity

#Example

X<-cbind(c(1:5),2*c(1:5)) #exact collinearity or collinearity
cor(X)

det(t(X)%*%X) #observe that det is 0, then inv(t(X)%*%X) will have all inf values because 
#det goes in denominator

#this explains that when det of t(X)%*%X is close to zero the coeffecients become inf. 

#if there is a near linear relation among the explanatory variables then we see the following:

X<-cbind(c(1:5),c(2,3.99,6,8,10))
cor(X)
det(t(X)%*%X) #observe that det is close to 0, then inv(t(X)%*%X) will have big values because 
#det goes in denominator, hence under multicollinearity situation beta values are 
#unnecessarily inflated


#1.Correlation Matrix
#install.packages("Hmisc")
library(Hmisc)
rcorr(cbind(mtcars[,2],mtcars[,6],mtcars[,9]))
cor(cbind(mtcars[,2],mtcars[,6],mtcars[,9]))

#2.VIF
#install.packages("usdm")
library(usdm)
vif(mtcars[,2:11])
vif(mtcars[,c(2,4:11)]) #dropping disp
vif(mtcars[,c(4:11)]) #dropping cyl

#Var 2, 3 and 6 are involved multicollinearity with threshold 10
summary(lm(mtcars[,2]~mtcars[,3]+mtcars[,6]))
summary(lm(mtcars[,3]~mtcars[,2]+mtcars[,6]))
summary(lm(mtcars[,2]~mtcars[,3]+mtcars[,6]))

vif(cbind(mtcars[,2],mtcars[,4:11]))

vif(mtcars[,4:11])


#Variable Selection - Parsimonious Modeling

#1. Forward Selection

#First make a model with just intercept
model<-lm(mpg~1,mtcars)

#Now add variables one by one
summary(step(model,direction = "forward",trace = 2,
             scope = ~ disp+drat+hp))

#2. Backward Elimination

#First make a model with all the predictors
model<-lm(mpg~.,mtcars)

#Now drop variables one by one
summary(step(model,direction = "backward",trace = 2))

#3. Stepwise Selection

#Starting with forward selection
summary(step(lm(mpg~1,mtcars),direction = "both",trace = 2), 
        scope = ~ disp+drat+hp) 

#Starting with backward elimination
summary(step(lm(mpg~.,mtcars),direction = "both",trace = 2)) #stepwise


#Residual Analysis
model<-lm(mpg~.,data=mtcars)
resid<-model$residual

#Tests for Normality of Residuals

#1. Histogram
hist(resid) #Normal

#2. QQ Plot
qqnorm(resid)

#3. KS Test
data1<-resid
x<-(data1-mean(data1))/sqrt(var(data1))
ks.test(x,rnorm(length(x)))

#Autocorrelation
#install.packages("lmtest")
library(lmtest)
dwtest(mpg~disp,data=mtcars)
dwtest(mpg~.,data=mtcars)

#Heteroscedasticity

#residual vs fits plot
plot(model$fit,model$residuals) # there should not be any pattern for homoscedasticity

#install.packages("Hmisc")
library(Hmisc)
rcorr(model$fit, abs(model$resid),type = "spearman")


#Outlier Detection
library(MASS)
outlier.statistics<-cbind(c(1:nrow(mtcars)),
                          studres(lm(mpg~.,mtcars)),
                          hatvalues(lm(mpg~.,mtcars)))

outliers1<-outlier.statistics[abs(outlier.statistics[,2])>3,]

outliers2<-outlier.statistics[abs(outlier.statistics[,2])<3,]

outliers2<-outliers2[abs(outliers2[,2])>2,]

outliers2<-outliers2[abs(outliers2[,3])>
                       2*(ncol(mtcars)-1)/nrow(mtcars)]

outliers<-rbind(outliers1,outliers2)


#Logistic Regression

#Reading Data 
data<-read.csv("cancer.csv")
model<-glm(y ~x, data = data, family = "binomial")
summary(model)

#odds ratio
odds_ratio<-exp(model$coefficients[2])
odds_ratio

model$fit
model$fit>0.5

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
View(mydata)

#Training Data and Test Data
train<-mydata[1:300,]
test<-mydata[301:400,]

#Modeling
model<-glm(admit ~ gre + gpa + rank, data = train, family = "binomial")
summary(model)
model$fit
pred<-model$fit>0.5

#Confusion Matrix - Training
ct<-table(pred,train$admit)
ct
cat("columns are observed and rows are predicted \n")

#percentage of misclassification
(ct[1,2]+ct[2,1])/sum(ct)

#percentage of misclassification for admit = 1 or sensitivity
ct[2,1]/sum(train$admit == 1)

#percentage of misclassification for admit = 1 or specificity
ct[1,2]/sum(train$admit == 0)

#Confusion Matrix - Testing
pred<-predict(model, newdata= test, type = "response")
pred<-pred>0.5
ct<-table(pred,test$admit)
ct
cat("columns are observed and rows are predicted \n")

#percentage of misclassification - test data
(ct[1,2]+ct[2,1])/sum(ct)

#percentage of misclassification for admit = 1
ct[2,1]/sum(test$admit == 1)

#percentage of misclassification for admit = 1
ct[1,2]/sum(test$admit == 0)

#Suppose gre=720, gpa=3.9, rank=3, I want to know if admission will be there or not
z<--4.05+.003*720+0.85*3.9-.64*1
prob<-1/(1+exp(-z))
admission<-prob>0.5
admission

#Hosmer and Lemeshow Goodness of Fit test

#H0: Model is significant
#H1: Model is insignificant

install.packages("ResourceSelection")
library("ResourceSelection")
hoslem.test(train$admit, fitted(model), g=10)

#Multiclass Classification - one vs. all method
data<-read.csv("iris_multiclass.csv")

#Model 1
model<-glm(y1 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data, family = "binomial")
summary(model)
pred1<-model$fit

#Model 2
model<-glm(y2 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data, family = "binomial")
summary(model)
pred2<-model$fit

#Model 3
model<-glm(y3 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data, family = "binomial")
summary(model)
pred3<-model$fit

data<-cbind(data,pred1,pred2,pred3)
write.csv(data,"pred_multiclass1.csv")

for(i in 1:nrow(data))
{
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred1[i]))
  
     data$pred[i]<-"setosa"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred2[i]))
    
     data$pred[i]<-"versicolor"

  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred3[i]))
     
     data$pred[i]<-"virginica"
}

#confusion matrix
table(data$pred,data$Species)

#multiclass classification - multinomial distribution 
data<-read.csv("iris_multiclass.csv")
library(nnet)
model<-multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data)
summary(model)

data<-cbind(data,fitted(model))

library(plyr)
data<-rename(data, c("setosa"="pred1", "versicolor"="pred2", "virginica" = "pred3"))
write.csv(data,"pred_multiclass2.csv")


for(i in 1:nrow(data))
{
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred1[i]))
    
    data$pred[i]<-"setosa"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred2[i]))
    
    data$pred[i]<-"versicolor"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred3[i]))
    
    data$pred[i]<-"virginica"
}

#confusion matrix
table(data$pred,data$Species)
