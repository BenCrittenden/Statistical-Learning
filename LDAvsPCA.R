rm(list=ls())

#LDA is often used for classification. However...

#LDA can also be used in a similar way as pricipal component analysis.
#i.e. it can be used to reduce the dimensionality of data
#this is particularly useful for plotting

require(MASS)

#Load the famous iris dataset
data("iris")

#have a look at it
head(iris)

#fit the model with all features and with 0.333 as the priror for 
#each class
L_mod = lda(formula = Species ~ ., 
            data = iris, 
            prior = c(1,1,1)/3)

#Inspect the data
L_mod$counts
L_mod$means

#what are the linear combination coefficients? There will always be k-1
#of these (i.e. one less than the number of classes)
L_mod$scaling

#here's the ones of interest to us now
L_mod$svd

#svd (singular value decomposition) gives the ratio of the between and
#within group standard deviations on the linear discriminant values.
#I think that there are k-1 svd values, but we only need 2 anyway.

#We can use the svd values to compute the amount of between-group 
#variance that is explained by each linear discriminant
#These values are actually shown in the print out, right at the bottom
#under the heading, 'proportion of trace'

L_mod
prop = L_mod$svd^2/sum(L_mod$svd^2)


#Do pca on the data
#need to remove the fifth column which is words (the species), so 
#that it's only numbers.
pca_mod <- prcomp(iris[,-5],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca_mod$sdev^2/sum(pca_mod$sdev^2)

#The first linear discriminant = 0.99% of variance
#The first principal component = 0.73 of variance

#it thus may be that using the 1st and 2nd LD's for plots is most useful

#This kind of difference is to be expected since PCA tries to retain 
#most of the variability in the data while LDA tries to retain most 
#of the between-class variance in the data. Note also that in this 
#example the first LD explains more than 99% of the between-group 
#variance in the data while the first PC explains 73% of the total 
#variability in the data.


###
#Below is code that someone else wrote to nicely visualise the difference
#between LD and PC dimensionality reduction for visualization.
#(using ggplot)
#
#Comments are mine, the script is the basis for what's above anyway
###

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

#do the PCA and find the PCs
pca <- prcomp(iris[,-5],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)

#pca.x gives the weighting of each exemplar on each of the PCs
#it is a (n x n_PCs) matrix

#do the LDA and find the LDs
r <- lda(Species ~ ., 
           iris, 
           prior = c(1,1,1)/3)

prop.lda = r$svd^2/sum(r$svd^2)

#use the model to predict (this would be bad as test=training, but it's
#just an example)
plda <- predict(object = r,
                newdata = iris)

#plda is a list with fields:
#class - i.e. which species. A row vector of names
#posterior - the prob. that that exemplar belongs to each of the three
#            classes. A (n x n_classes) matrix
#x - the values of each exemplar in units of the first and second LD.
#    A (n x n_LDs) matrix

#put the pca and lda data into a table with the species info

dataset = data.frame(species = iris[,"Species"],
                     pca = pca$x, lda = plda$x)

#plot the data using the first and second LDs, rather than choosing one
#of the features
p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
        labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
             y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

#plot the data using the first and second PCs
p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = species, shape = species), size = 2.5) +
        labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
             y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)

#from the graphs you can see that it'd be much easier to separate the 
#blues and greens using the LDs (particularly LD1) compared to using
#the PCs, where they're more jumbled up.
#Both do a good job of separating the reds though.


