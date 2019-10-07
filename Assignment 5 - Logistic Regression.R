library(ISLR); 
library(ggplot2); 
library(caret); 
library(caTools)
library(ISLR) 
library(dplyr)
library(tidyr)

data = read.csv("eBayAssignment.csv")
summary(data)
head(data)
black = data %>%
  filter(color == 'Black')
count(black)
count(data[data$productline == 'iPad 3',])

max(data$startprice)
data[data$startprice == 999,]

set.seed(196)
split = sample.split(data$sold, SplitRatio = 0.8)
train = data[split,]
test = data[!split,]
nrow(train)
nrow(test)

median(train[train$sold == 0,]$startprice)
x = train %>% 
  filter(sold == 1) %>%
  median(startprice)
x
median(x$startprice)

model1 = glm(sold~biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end, data=train, family='binomial')
summary(model1)

100*(exp(summary(model1)$coef[3])-1)

model2 = glm(sold~productline, data=train, family = 'binomial')
summary(model2)

exp(summary(model1)$coef[12])
