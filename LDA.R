rm(list=ls())

require(ISLR)
require(MASS)

###Linear Discriminant Analysis

#Get the help for lda, part of the MASS package which is a stats package from
#a couple of guys
?lda

#we're going to use the stock market data, like we did for log. regression

#fit the direction data using the first two lag period, using LDA
#only train on data before 2005, we'll test on 2005.
lda.fit = lda(Direction~Lag1+Lag2,
              data=Smarket, 
              subset = Year < 2005)

lda.fit

#as you can see, the prior probabilities are about 50:50 for up and down

#Have a look at the two histograms of the data
plot(lda.fit)

#The plots are of the discriminant function of the two classes
#The axis appears to come from the scale of Lag1 and Lag2. Not sure
#what would happen if the units of the different predictors were 
#different to each other.
#here the two distributions are more-or-less on top of each other, so
#you know that it's going to be difficult to discriminate data points
#between them. Generally, the less the overlap, the better the classifier
#will perform.

#now subset the data, selecting only the 2005 data
Smarket.2005 = subset(Smarket, Year=2005)

#make some predictions based on this data
lda.pred = predict(lda.fit,Smarket.2005)

#to inspect this, lad.pred[1,5] doesn't work because it's a list, so...
data.frame(lda.pred)[1:5,]

#note how posterior.Down + posterior.Up for each row = 1 (i.e. they're
#probailities). You also get the prediction in the fist column and
#the LDA score in the last column

#create a table that has the predicted class (up/down) against the actual
#class in the table. This is called the 'confusion matrix'
table(lda.pred$class, Smarket.2005$Direction)

#and just how accurate were we?
mean(lda.pred$class == Smarket.2005$Direction)

#0.52, i.e. 52%. Not huge but could make you some $$$.


#How do you get the cool lines?
#Well, this is actually not very elegant:
#
# First you need to reduce your data to two dimensions.
#
#1. You first of define a grid across the whole space.
#2. Choose a grid square and put in those values and figure out which 
#   class that grid square would belong too
#3. Do that for every square
#4. Use a contouring algorithm to find where there are boundaries
#   and draw the countours.
#
#The smaller the grid, the smoother the countours (but the longer it takes)
