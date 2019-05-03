#SCRIPT PER GOOGLE

# loading data
data("storms")
head(cars)

cohort <- read.csv("E:/contardi/egfr fracture/data _management/CSV/cohort.csv")
load("P:/SCREAM/SCREAM_CLEAN/Datasets/demo.RData")  # R objects
CARTEL1 <- read.delim("~/Cartel1.txt", header=TRUE)

# excel
library(readxl)
esportazione_sea2 <- read_excel("Z:/esportazione_sea2.xlsx")
View(esportazione_sea2)

#   workspace
ls()
rm()
rm(list=ls())  # remove all

# Dplyr package 
install.packages("dplyr")
require(dplyr)
library(dplyr)
library(help = "dplyr")
library(help = "stats")

arrange(flights, year, month, day)
order()

select(flights, year, month, day)  # select columns
select(flights, year:day)  # Select all columns between year and day (inclusive)
subset(airquality, select = Ozone:Wind)


filter(flights, month == 1, day == 1)   #dplyr
flights[flights$month == 1 & flights$day == 1, ]

subset(airquality, Temp > 80, select = c(Ozone, Temp))  #subset (base pack) e filter same thing  (e with?)
subset(airquality, Day == 1, select = -Temp)


rename(flights, tail_num = tailnum)

transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8) #base
mutate(airquality, new = -Ozone, Temp = (Temp-32)/1.8)  #dplyr
transmute(...)   #keeps only new variables

gruppi <- group_by(storms, year)
aggregate(...)


# Ggplot package 
install.packages("ggplot2")
require(ggplot2)
library(ggplot2)

#qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, 
#       facets=, xlim=, ylim= xlab=, ylab=, main=, sub=)
qplot(1:10, rnorm(10), colour = c("Rome", "Milan", rep("Other",8)))

ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3) #3 pezzi distinti

#descriptives
qq = quantile(obs, probs = seq(0, 1, .2)) 

#inferential
shapiro.test(x) #normality
t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)
oneway.test(extra ~ group, data = sleep)  #generalization of t-test

var.test(x, y) #F test to compare the variances of two samples from normal populations

prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE) #proportion

wilcox.test(x, y, paired = TRUE, alternative = "greater") # to determine whether two independent samples were 
 #selected from populations having the same distribution

#models
#lm is used to fit linear models. It can be used to carry out regression, single stratum analysis 
#of variance and analysis of covariance (although aov may provide a more convenient interface for these).
lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, ...)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

#analysis of variance model by a call to lm for each stratum
aov(formula, data = NULL, projections = FALSE, qr = TRUE,
    contrasts = NULL, ...)    #The main difference from lm is in the way print, summary and so on handle the fit

#glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor 
#and a description of the error distribution.
glm(formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart, offset,
    control = list(...), model = TRUE, method = "glm.fit",
    x = FALSE, y = TRUE, singular.ok = TRUE, contrasts = NULL, ...)
    # An offset is a component of a linear predictor that is known in advance (typically from theory, or from a mechanistic model of the process).
    # Because it is known, it requires no parameter to be estimated from the data
    # the offset is held constant while the other explanatory variables are evaluated

binomial(link = "logit")
gaussian(link = "identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")

#step   Select a formula-based model by AIC.
summary(lm1 <- lm(Fertility ~ ., data = swiss))
slm1 <- step(lm1)
summary(slm1)
slm1$anova

#after glm, summary.glm() or anovaglm(), coefficients(), effects(), fitted.values(), residuals()

#Log-Linear Models
#loglin is used to fit log-linear models to multidimensional contingency tables by Iterative Proportional Fitting.
loglin(table, margin, start = rep(1, length(table)), fit = FALSE,
eps = 0.1, iter = 20, param = FALSE, print = TRUE)

#anova
#This (generic) function returns an object of class anova. These objects represent analysis-of-variance 
#and analysis-of-deviance tables. When given a single argument it produces a table which tests whether 
#the model terms are significant.

#When given a sequence of objects, anova tests the models against one another in the order specified.
anova(object, ...)

#PCA
#Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp.
prcomp(x, ...)
#Produces a biplot (in the strict sense) from the output of princomp or prcomp
biplot(pr)


#Cluster Analysis
#Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
hclust(d, method = "complete", members = NULL) # method=single, complete, average, median, centroid
hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)
#identify.hclust reads the position of the graphics pointer when the (first) mouse button is pressed. 
#It then cuts the tree at the vertical position of the pointer and highlights the cluster containing 
#the horizontal position of the pointer. Optionally a function is applied to the index of data points 
#contained in the cluster.
identify(x, FUN = NULL, N = 20, MAXCLUSTER = 20, DEV.FUN = NULL,
         ...)
hca <- hclust(dist(USArrests))
plot(hca)
(x <- identify(hca)) ##  Terminate with 2nd mouse button !!
#Draws rectangles around the branches of a dendrogram highlighting the corresponding clusters. 
#First the dendrogram is cut at a certain level, then a rectangle is drawn around selected branches.
hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k = 3, border = "red")   #Cut the dendrogram such that either exactly k clusters are produced or by cutting at height h.
x <- rect.hclust(hca, h = 50, which = c(2,7), border = 3:4)
# Cuts a tree, e.g., as resulting from hclust, into several groups either by specifying the desired number(s) of groups or the cut height(s).
cutree(tree, k = NULL, h = NULL)


#Neural Network
require(nnet)
#Fit single-hidden-layer neural network, possibly with skip-layer connections.
nnet(x, ...)


#Function Definition
function( arglist ) expr
return(value)
norm <- function(x) sqrt(x%*%x) #%*% for matrix multiplication
norm(1:4)
# Declare function "f" with parameters "x", "y"
# that returns a linear combination of x and y.
f <- function(x, y) {
  z <- 3 * x + 4 * y
  return(z)
}
f(1, 2); f(c(1,2,3), c(5,3,4)); f(1:3, 4)

#ciclo for
for (i in names(train))
{print(nchar(i))}
#example from code Wars
printerError <- function(s) {
  l <- nchar(s)
  somma <- rep(0,l)
  for (i in 1:l) {
    somma[i] <- ifelse(substr(s, i, i) %in% letters[1:13],0,1)
  }
  paste(c(sum(somma), l), collapse = "/")
}

# Generate Factor Levels
gl(n, k, length = n*k, labels = seq_len(n), ordered = FALSE)

# grep() Pattern Matching and Replacement
grep("[a-z]", letters)

# grouping() permutation (disposizioni)
# ifelse(test, yes, no)
#match(x, table, nomatch = NA_integer_, incomparables = NULL)    x %in% table

# write(x, "", sep = "\t")

#aggregate: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
aggregate(state.x77, list(Region = state.region), mean)
aggregate(weight ~ feed, data = chickwts, mean)


y <- runif(200)
qqnorm(y)
abline(coef = c(0,1))

quantile(x,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)


par(mfrow=c(2,2))

