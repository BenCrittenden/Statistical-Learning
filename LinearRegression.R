rm(list=ls())

library(MASS)
library(ISLR)

###How to do simple linear regression in R


#Get some info on the Boston data set
?Boston

#it's a load of socioeconomic info about different districts in Boston

#Get a list of the variables in the Boston dataset
names(Boston)

#We want to know how the median value of homes (medv) varies according to
#what percent of the population are of the lower status, within the Boston
#dataset.
#i.e. plot medv(y-axis) against lstat(x-axis)

plot(medv~lstat,Boston)

#now fit a linear model to the data with respect to these two variables

fit1 = lm(medv~lstat,data=Boston)

#print the (basic) parameter estimates to the console
fit1

#To get more detailed info about the model fit use summary()
summary(fit1)

#use the linear model fit to add a line of fit to the data. 
#R has a special function fot that purpose: abline()
abline(fit1,col='red')

#to get a list of all the different bits of data from the fit
names(fit1)

#now say I want to know what the values of the coefficients were
fit1$coefficients

#what was the confidence intervals for each of the parameter estimates?
confint(fit1)

#now lets say that I want predictions of medv for certain values of lstat
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")


###Multiple linear regression

#Now I want to know how medv varies as a function of both lstat and age
fit2 = lm(medv ~ lstat + age, data=Boston)

#and let's have a look at the results
summary(fit2)

#and now do a fit of the model of medv using all other variables as predictors
fit3 = lm(medv ~ ., data=Boston)

#and let's have a look at it
summary(fit3)

#interestingly, age is significant in fit2, but not in fit3 which includes
#a lot more predictors. This suggests that there are other predictors that
#are correlated with age, and when these are fit fist, age has less variance
#left to explain.

#now lets plot the data.
#there are four default plots from a full multiple regression plot:
#residuals vs fitted
#normal Q-Q
#scale-location
#residuals vs leverage.
#if you just call plot, it will plot these one at a time. To have them
#as subplots do the following
par = (mfrow = c(2,2))
plot(fit3)

#if you look at the residuals vs fitted plot, a good fitting model wuould
#have a cloud of data - i.e. not much order. If there is a clear trend, it
#suggests that there is still non-random variance that can be explained.

#Having looked at the model, you realise that you want to remove the age
#and indus predictors from the model.
fit4 = update(fit3, ~ . -age -indus)
summary(fit4)

###Now lets add some interaction and polynomial terms

#now lets add some interaction terms
fit5 = lm(medv ~ lstat*age,data=Boston)
summary(fit5)

#the main effects are included by default and the interaction is shown as
#lstat:age

#now lets try adding a polynomial.
#if we just raise the predictor to the power 2, something strange may
#happen, because it means something else in the syntax.
#so instead we encase it as in an identity using I(). 
fit6 = lm(medv ~ lstat + I(lstat^2), data=Boston)
summary(fit6)

#The boston data set isn't actually in the R search path, so to put it there:
#This way the database can be accessed by simply giving the names
attach(Boston)

#put the default back to plotting one graph at a time
par(mfrow=c(1,1))

#bring up the original plot of medv agaisnt lstat again
plot(medv~lstat)

#now, plot the line of best fit, but from the multiple regression
#i.e. not just using a straight line
#to do this, you basically need to plot what the medv values are from
#the estimated model
points(lstat,fitted(fit6),col='red',pch=20)

#pch=20 refers to which symbol you want to use to plot (20 = small filled circle)

#what about if you want to just have the best fitting polynomial of one of
#the predictors of a given polynomial order, say to the 4th power?
fit7 = lm(medv ~ poly(lstat,4))

#and plot that fit
points(lstat,fitted(fit7),col='blue',pch=20)

#it looks like blue line is probably overfit...

#so what other ploting characters are there and what are there codes?
#here's a useful little graph
plot(1:20,1:20,pch=1:20,cex=2)

#cex = 2 means that we want to make the characters twice the default size

###Qualitative Predictors

#now we're going to work with the Carseats dataframe.
#How can I look at the dataframe, such as in an excel-like table?
fix(Carseats)

#and some other ways to look at the data
names(Carseats)
summary(Carseats)

#note how ShelveLoc is a qualitative variable so it's summary is different

#fit a linear model to this data using all of the predictors with a 
#couple of additional interaction terms
fit10 = lm(Sales ~ . +Income:Advertising +Age:Price, data=Carseats)
summary(fit10)

#But ShelveLoc is qualitative, so just how did R treat ShelveLoc,
#i.e. what is the dummy variable make-up for ShelveLoc?
contrasts(Carseats$ShelveLoc)

#so looks like Bad is the baseline here

###Writing R functions to fit a linear model and plot the results

#lets put everything that we've learned together and write a little function

regplot = function(x,y){
        
        fit=lm(y~x)
        plot(x,y)
        abline(fit,col='red')
}

#and lets put it to the test with some of the Carseats data

attach(Carseats)
regplot(Price,Sales)

#now lets make regplot a bit more versatile using R's ... syntax
#... allows you to add in additional arguments which will be read exactly
#as you enter them by any other functions within your function

#so let's use ... to make the plots look better

regplot2 = function(x,y,...){
        
        fit = lm(y~x)
        plot(x,y,...)
        abline(fit,col='green')
}

#and let's test it

regplot2(Price,Sales,xlab='Pric',ylab='Sales',col='blue',pch=20)
