rm(list=ls())

library(class)
require(ISLR)

### K-nearest neighbours

#let's get some help on KNN
?knn

#as you'll see from the help doc, knn does not take a formula, unlike
#LDA, glm, lm...
#instead you have to put in the train data, the test data and the class
#labels yourself.

#it'll be much easier if we make the data variables directly available
#to R...
attach(Smarket)

#create a new variable, a matrix with n rows and the first two 
#lags for columns
XLag = cbind(Lag1,Lag2)

XLag[1:5,]
#if you just want to look at a few rows column, referenced by name or number
XLag[1:3,'Lag1']
XLag[1:3,1]

#now get a logical vector with 1's for all the points where the data
#was before 2005 and 0's otherwise.
train = Year < 2005

#Put this info into the knn algorithm
knn.pred = knn(train = XLag[train,],
               test = XLag[!train,],
               Direction[train],
               k=100)

#note how here it does the training and testing all in one fuction, 
#there's no explicit model fit step as before - you only get the 
#performance on the training data. I suppose that if you wanted to 
#see what the performance was on the training data, you could input 
#the same data into the test data argument.
#Also, here we're only using the single nearest neighbour, so whatever
#the nearest point in the training data is to the test point you're 
#considering in euclidean distance, that is what it will be classified
#as. That's not going to be very stable... larger number may be better!

#put this into a table
table(knn.pred,Direction[!train])

#and what's the overall accuracy
mean(knn.pred == Direction[!train])

#the accuracy was... 50%. May as well have flipped a coin
#if you use 100, for example you get almost 52% 
#however, using 10, actually gives you only 49%

