library(ISLR)
library(ggplot2)
library(caret)
library(caTools)
library(dplyr)
library(tidyr)

setwd('C:/Users/84545/Documents/R')
wages = read.csv("wages.csv")
wages = wages[wages$earn>0,]  # remove rows with negative earning 

#question 2
female = count(wages[wages$sex == 'female',])
total = count(wages)
male = count(wages[wages$sex == 'male',])
male + female - total
female / total

#questino 3
black = mean(wages[wages$race == 'black',]$earn)
hispanic = mean(wages[wages$race == 'hispanic',]$earn)
other = mean(wages[wages$race == 'other',]$earn)
white= mean(wages[wages$race == 'white',]$earn)
black; hispanic; other; white

#question 4
set.seed(1731)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]
nrow(train)/nrow(wages)
nrow(train)


#part 2
#question 1
model1 = lm(earn~.,data = train)
summary(model1)

#question 2
pred = predict(model1,newdata=train)
rmse1 = sqrt(mean((pred-train$earn)^2))
rmse1

#question 3 and 4
library(ggplot2)
ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+ 
  geom_bar(stat="summary",fun.y="mean",position="dodge")
ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))

males = train %>%
  filter(sex == 'male')
mean(males[males$ed == 16,]$earn) - mean(males[males$ed == 12,]$earn)

female= train %>%
  filter(sex == 'female')
mean(female[female$ed == 16,]$earn) - mean (female[female$ed == 12,]$earn)
#note that below call offers a good way to index based on multiple conditions
mean(train[train$sex == 'female'& train$ed == 16,]$earn)
mean(train[train$ed == 12 & train$sex == 'female',]$earn)

#question 5
model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)
summary(model_sex_ed)

#question 6
model2 = lm(earn~height+sex+race+ed+age+sex*ed, data=train)
summary(model2)
pred2 = predict(model2,newdata=train)
sse = sum((pred2-train$earn)^2)
sst = sum((mean(train$earn) - train$earn)^2)
rmse2 = sqrt(mean((pred2-train$earn)^2))
rmse2

#question 7 - 10
model3 = lm(earn~height+sex+race+ed+age+sex*ed+sex*age+age*ed, data=train)
summary(model3)
pred3 = predict(model3,newdata=train)
rmse3 = sqrt(mean((pred3-train$earn)^2))
rmse3

#questino 11; section 4 question 1
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
summary(model5)
pred5 = predict(model5, newdata = train)
rmse5 = sqrt(mean((pred5 - train$earn)^2))
rmse5

pred_t1 = predict(model5, newdata = test)
rmse_t1 = sqrt(mean((pred_t1 - test$earn)^2))
rmse_t1


#section 3 question 1
library(rpart); library(rpart.plot)
tree1 = rpart(earn~.,data=train, method = 'anova') 
prp(tree1,digits=5)   # tree plot method 1

#question 5
pred_tree1 = predict(tree1, newdata = train)
rmse_tree1 = sqrt(mean((pred_tree1 - train$earn)^2))
rmse_tree1

pred_t2 = predict(tree1, newdata = test)
rmse_t2 = sqrt(mean((pred_t2 - test$earn)^2))
rmse_t2

#question 6
treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
prp(treeSimp1, digits=5)
pred_treeSimp1 = predict(treeSimp1, newdata = train)
rmse_treeSimp1 = sqrt(mean((pred_treeSimp1 - train$earn)^2))
rmse_treeSimp1

#question 8
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
prp(treeSimp2, digits=5)
pred_treeSimp2 = predict(treeSimp2, newdata = train)
rmse_treeSimp2 = sqrt(mean((pred_treeSimp2 - train$earn)^2))
rmse_treeSimp2

pred_t3 = predict(treeSimp2, newdata = test)
rmse_t3 = sqrt(mean((pred_t3 - test$earn)^2))
rmse_t3


#question 10
treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
prp(treeComplex1, digits=5)
pred_comp1 = predict(treeComplex1, newdata = train)
rmse_comp1 = sqrt(mean((pred_comp1 - train$earn)^2))
rmse_comp1

#question 11
treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
prp(treeComplex2, digits=5)
pred_comp2 = predict(treeComplex2, newdata = train)
rmse_comp2 = sqrt(mean((pred_comp2 - train$earn)^2))
rmse_comp2

pred_t4 = predict(treeComplex2, newdata = test)
rmse_t4 = sqrt(mean((pred_t4 - test$earn)^2))
rmse_t4

