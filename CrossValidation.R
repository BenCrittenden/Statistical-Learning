rm(list=ls())

require(ISLR)
require(boot)

###Cross Validation 

#Lets have a look at the main function
?cv.glm

#if you have a look at the K argument, you'll notice that if this is not
#defined, the default is to use all the data - thus this is LOOCV.

#lets have a look at the data
plot(mpg~horsepower, data=Auto)

#First off we'll do leave one out cross validation (LOOCV)

#for CV you need to do the fit first and then the CV (at least according) to
#this approach.

#So let's fit the model, using a standard linear model (the default for glm)
glm.fit  = glm(mpg~horsepower, data=Auto)

#and now do the CV bit and get the delta values (the CV pred. error)
cv.glm(Auto,glm.fit)$delta

#the first number is the raw LOOCV result
#the second is a bias corrected version of the LOOCV value, which compensates
#for the (small) size of the data.

#let's write our own LOOCV function that uses the hat matrix (equation in notes)

loocv = function(fit){
        
        #get the diagonal elements of the hat matrix
        h = lm.influence(fit)$h
        
        #calculate the residuals
        #produces a n x 1 vector
        resid = residuals(fit)
        
        #now the term in the denominator which uses the hat matrix
        #produces a n x 1 vector
        denom = (1-h)
        
        #the equation then wants the sum over all data points, divided by
        #the number of data points - this is just the mean.
        mean((resid/denom)^2)
        
        #because the mean is the last quantity to be computed, that's what
        #the fuction will return by default
        
}

#let's test the function that we just wrote
loocv(glm.fit)

#note how our function produces the same delta number as cv.glm

#the relationship between horsepower and mpg were clearly not linear, so we
#should fit some non-linear function to it.

#let's try fitting a series of polynomials of increasing degree
#but first create a variable that will store the output

n_polys = 5
cv.error = rep(0,n_polys)

for (d in 1:n_polys){
        
        glm.fit = glm(mpg~poly(horsepower,d), data=Auto)
        cv.error[d] = loocv(glm.fit)
}

#let's plot the results
plot(1:n_polys,cv.error,type='b')

#looks like 2nd order does a lot better, but higher, doesn't make much difference

###K-fold (10-fold) CV


#let's produce the same plot as we just did for LOOCV but with K-fold

#for K-fold we use the cv.glm function as we did at the start and also specify
#the number of folds that we want to do. (remember the default is LOOCV)

n_polys = 5
cv.error10 = rep(0,n_polys)

for (d in 1:n_polys){
        
        glm.fit = glm(mpg~poly(horsepower,d), data=Auto)
        cv.error10[d] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}

#let's add a red line of these results to the existing plot
lines(1:n_polys, cv.error10, type='b', col='red')

#in this case, it looks like both 10-fold and LOOCV were about the same.
#10-fold tends to be more stable and is much faster (as long as there's less
#than 10 data-points!), so that's normally prefered.

