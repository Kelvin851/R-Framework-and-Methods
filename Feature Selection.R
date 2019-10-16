library(ISLR)
library(ggplot2)
library(caret)
library(caTools)
library(dplyr)
library(tidyr)

data = read.table('houses.csv', header = TRUE, sep = ',')
data

set.seed(1031)
split = createDataPartition(y=data$price,p = 0.7,list = F,groups = 100)
train = data[split,]
test = data[-split,]
nrow(train)
nrow(test)
mean(train$price)
mean(test$price)

model = lm(price~., train)
summary(model)
round(cor(train), 2)*100
library(corrplot)
corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)

cor(train$sqft_living, train$sqft_above + train$sqft_basement)

model1 = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data = train)
summary(model1)

library(car)
vif(model1)

#subset selection; part 2 question 1 and question2

subsets = regsubsets(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,data=train, nvmax=6)
summary(subsets)
names(summary(subsets))
subsets$adjr2
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              r2=summary(subsets)$rsq)
subsets_measures

# forward selection
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
summary(forwardStepwise)

#backward selection
start_mod = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,data=train)
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
summary(backwardStepwise)

#hybrid stepwise
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
summary(hybridStepwise)


#Lasso Prediction
library(glmnet)

x = model.matrix(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age,data=train)
y = train$price
lassoModel = glmnet(x,y, alpha=1) 
set.seed(1031)
cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso)
coef(cv.lasso)
summary(cv.lasso)
model_final = lm(formula = price~bathrooms+sqft_living+waterfront+view+grade+age, data = train)
summary(model_final)

#dimension reduction
library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

train_model = lm(price~.,trainComponents)
summary(train_model)

testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price

pred = predict(train_model,newdata=testComponents)
sse = sum((pred-testComponents$price)^2)
sst = sum((mean(trainComponents$price) - testComponents$price)^2)
r2_test = 1 - sse/sst
r2_test
