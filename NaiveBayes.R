rm(list=ls())

library(e1071) #the naive bayes package
library(ISLR)

#My attempt at using a Naive Bayes classifier on the Smarket data

?naiveBayes

#First off, subselect the training data (pre-2005)

Pre2005 = Smarket$Year < 2005
trainDat = Smarket[Pre2005,]
testDat = Smarket[!Pre2005,]

#write the formula, but note that interactions are not allowed.
#not sure if that's just a limitation of NB, or of this package.

nb.fit = naiveBayes(Direction ~ Lag1+Lag2,
                    data = trainDat) 

#have a look
nb.fit

#what are the predictions on the test data
nb.pred = predict(nb.fit,testDat)

#have a look at some of the predictions
data.frame(nb.pred)[1:20,]

#Lets have a look at the confusion table
table(nb.pred,testDat$Direction)

#what's the mean accuracy
mean(nb.pred == testDat$Direction)

#mean is 0.59 - which is incredible!
