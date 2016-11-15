rm(list = ls())

###Bootstrap

#they like to use the example of a problem where you need to pick the optimal
#combination of two potential investments. This is a standard formula for this
#problem which we will write out now (called alpha)

alpha = function(x,y){
        
        vx = var(x)
        vy = var(y)
        cxy = cov(x,y)
        
        (vy-cxy) / ((vx + vy) - 2*cxy)
        #function returns the last computed value
        
}

#Now lets look at it with the portfolio data
alpha(Portfolio$X, Portfolio$Y)

#Here's where the bootstrap comes in...

#What is the standard error in our estimate of alpha? Sounds like a problem
#for bootstrap!

#lets write a little wrapper function 

alpha.fn = function(data, index){
        
        #here we use the function with
        #with takes the data, and then does some function on it
        #it automatically recognises the columns X and Y in the data.
        with(data[index,],alpha(X,Y))
}


#let's try it out with the portfolio data
alpha.fn(Portfolio,1:100)

# Portfolio has 100 observations of each of X and Y, so what we just did was
# 'boostrap' but picked each and every observation exactly once. i.e. not 
# really a bootstrap!

#let's do it again, but actually randomise the index

set.seed(1)

#define how we want to sample the index
#i.e. pick from the numbers 1 to 100. Do that 100 times, replacing each time.
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

#it appears as though in R, when you call a function like this, it will 
#remember the arguments that you give it, so that when the function itself
#is called by another function, you don't have to include all the arguments
#see the next line of code's reference to just alpha.fn to see what I mean.

#Now do that actual bootstrap bit, with 1000 permutations (repetitions)
boot.out = boot(Portfolio, alpha.fn, R=1000)

#Let's have a look at the summary statistics
boot.out

#and plot the data...
plot(boot.out)

#histogram gives us a nice gaussian looking Gaussian, which is confirmed by 
#the Q-Q plot (data lines on roughly a straight line)