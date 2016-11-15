rm(list=ls())

require(ISLR) #similar to library

#let's have a look at the data
names(Smarket)
summary(Smarket)
?Smarket

#looks like it's some stock market data

#lets make a scatter plot of all pairs of variables, colour coded by whether
#the market went up or down that day.
pairs(Smarket,col=Smarket$Direction)

###Logistic Regression

#you fit logistic regression by choosing family=binomial

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial)

summary(glm.fit)

#as you can see from the pvalues you can see that none of the predictors 
#are significant. Not surprising as if they were people would already be
#exploiting it to make money!
#The null deviance is the log likehood if you just use the mean
#The residual deviance is the log likehood of the model, and as you can see
#there's really not much of an improvement.

#now lets make some predictions from the model

#Let's make a prediction as to whether each stock will be up or down, based
#on the model.
glm.probs = predict(glm.fit,type='response')

#glm.probs is a 1x1250 vector (there are 1250 different stocks)
#Let's look at just the first 5.
glm.probs[1:5]

#Looks like they're all around 50%, i.e. a cointoss.

#Let's turn these probabilities directly into binary predictions
#e.g. 'up' or 'down'. Based on whether they're above/below the 0.5 threshol
#lets' look at the first 10.
glm.pred = ifelse(glm.probs > 0.5, 'Up', 'Down')
glm.pred[1:10]

#make the variables available by name
attach(Smarket)

#Lets see how many predicitons we got right in a table of
#hits / correct rejections / false positives / false negatives
table(glm.pred,Direction)

#What is the average performance of the classifier
mean(glm.pred==Direction)

#with an accuracy of 0.52, that suggests that we're only slightly better 
#than chance. Nonetheless, you could make £££ with a slight advantage like
#that in the long run. However, maybe we've overfit the data. Does it 
#generalize?

#Making training and test set

#Make a logical vector that gives us a ture whereever a data point if from
#a year prior to 2005
train = Year<2005

#Now fit the model using only the training data
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data = Smarket,
              family = binomial,
              subset = train)

#See what the fitted model predicts on the not trained data, i.e. the test
#data. Remember, in R, !train means, not train. e.g. ~train in matlab.
glm.probs = predict(glm.fit, 
                    newdata=Smarket[!train,],
                    type='response')

#and convert these probabilities into binary up/down predictions as before
glm.pred = ifelse(glm.probs > 0.5, 'Up','Down')

#Now get the data telling us whether the stocks did go up or down for the
#train data.
Direction.2005 = Smarket$Direction[!train]

#And lets put that into a table to see our false positives etc
table(glm.pred,Direction.2005)

#and the overall accuracy was...
mean(glm.pred==Direction.2005)

#NOTE - here originally I had mean of 0, but that was because the cases
#didn't match. originally in glm.pred I had the values becoming 'up' and
#'down' which didn't match 'Up and 'Down', so be careful with that!

#Now the mean is working and I get a value of 0.48, i.e. the model will 
#lose us money in the long run because it's worse than chance.

###Now lets try fitting a smaller model

#Maybe it's worse because we're overfitting
#Lets try removing some predictors
#Fit the model, but only use lag1 and lag2, and do all the other steps as
#before. Still using the same test/train split

glm.fit = glm(Direction~Lag1+Lag2,
              data = Smarket,
              family = binomial,
              subset = train)

glm.probs = predict(glm.fit, 
                    newdata=Smarket[!train,],
                    type='response')

glm.pred = ifelse(glm.probs > 0.5, 'Up','Down')

Direction.2005 = Smarket$Direction[!train]

table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)

#Now the fit is much better! 0.559!
#but...
summary(glm.fit)

#...none of the predictors (lag1 or 2) are significant, so I'd wouldn't
#go betting on the stock market with this model just yet...
