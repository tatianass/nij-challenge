### Five Number Summary of data

# To pull mtcars data in your session
data(mtcars)

# see general aspects of the data
str(mtcars)

# view top few records of mtcars
head(mtcars)

# Five Numbersummary & interqurtile range
# Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) for the input data.
fivenum(mtcars$mpg)

# https://www.r-bloggers.com/how-to-make-a-histogram-with-basic-r/
# https://en.wikipedia.org/wiki/Probability_density_function
hist(mtcars$mpg, 
     main = "Histogram for MPG", 
     xlab = "MPG",
     border = "blue",
     col = "green",
     xlim = c(10, 35),
     ylim = c(0, 0.12),
     breaks = 10,
     prob = T)

lines(density(mtcars$mpg))

# Interquartile range (75 percentile - 25 percentile) from fivenum
# Used to find outliers
# http://www.physics.csbsju.edu/stats/simple.box.defs.gif
IQR(mtcars$mpg)

# Association between fice number & Boxplot of mpg
# http://flowingdata.com/2008/02/15/how-to-read-and-use-a-box-and-whisker-plot/
boxplot(mtcars$mpg)

# Change somevalues in mtcars$mpg (edit function) to introduce outliers in data
mtcars <- edit(mtcars)

## Box plot to compare quantities or variables
data("iris")
str(iris)
head(iris)
boxplot(iris$Sepal.Length ~ iris$Species)
boxplot(iris$Sepal.Length ~ iris$Species, main = "Petal Length")
boxplot(iris$Sepal.Length ~ iris$Species, main = "Petal Length", xlab = "Species", ylab = "Petal Length", col = "red")
# colors()

# To change background of the plot
par(bg="green")
boxplot(iris$Sepal.Length ~ iris$Species, main = "Petal Length", xlab = "Species", ylab = "Petal Length", col = "red")

## box plot in lattice package
library(lattice)
bwplot(iris$Sepal.Length ~ iris$Species)
bwplot(iris$Sepal.Length ~ iris$Species, main = "Petal Length")
xyplot(iris$Sepal.Length ~ iris$Species)
xyplot(iris$Sepal.Length ~ iris$Species, main = "Petal Length", col = "red", pch=2)

## box plot in ggplot2 package
if(!require(ggplot2)){
  install.packages("ggplot2")
}
library(ggplot2)
qplot(iris$Species,iris$Sepal.Length, data=iris, geom="boxplot")

## Summary function for numeric value & categorical
summary(iris)
data(mtcars)
str(mtcars)
summary(mtcars)
boxplot(mtcars)

## Compation of mpg & cyl
boxplot(mtcars$mpg~mtcars$cyl, xlab = "mpg", ylab = "cyl")

# Describe function
if(!require(Hmisc)){
  install.packages("Hmisc")
}
library(Hmisc)
# used to missing values
describe(mtcars)
describe(mtcars$mpg)

# slice & dice of dataset
A1 <- summarize(mtcars$mpg, mtcars$cyl, summary)
A1

A2 <- summarize(mtcars$mpg, mtcars$gear, summary)
A2

A3 <- summarize(mtcars$mpg, mtcars$gear, mean)
A3

##https://www.r-bloggers.com/exploratory-data-analysis-quantile-quantile-plots-for-new-yorks-ozone-pollution-data/
##### Quantile-Quantile Plots of Ozone Pollution Data
##### By Eric Cai - The Chemical Statistician
# clear all variables
rm(list = ls(all.names = TRUE))

# view first 6 entries of the "Ozone" data frame 
head(airquality)

# extract "Ozone" data vector
ozone = airquality$Ozone

# sample size of "ozone"
length(ozone)

# summary of "ozone"
summary(ozone)

# remove missing values from "ozone"
ozone = ozone[!is.na(ozone)]

# having removed missing values, find the number of non-missing values in "ozone"
n = length(ozone)

# calculate mean, variance and standard deviation of "ozone"
mean.ozone = mean(ozone)
var.ozone = var(ozone)
sd.ozone = sd(ozone)
max.ozone = max(ozone, na.rm = T)

# set n points in the interval (0,1)
# use the formula k/(n+1), for k = 1,..,n
# this is a vector of the n probabilities
probabilities = (1:n)/(n+1)

# calculate normal quantiles using mean and standard deviation from "ozone"
normal.quantiles = qnorm(probabilities, mean(ozone, na.rm = T), sd(ozone, na.rm = T))

# normal quantile-quantile plot for "ozone"
png('analyse/normal-qqplot.png')
plot(sort(normal.quantiles), sort(ozone) , xlab = 'Theoretical Quantiles from Normal Distribution', ylab = 'Sample Quqnatiles of Ozone', main = 'Normal Quantile-Quantile Plot of Ozone')
abline(0,1)
dev.off()

# calculate gamma quantiles using mean and standard deviation from "ozone" to calculate shape and scale parameters
gamma.quantiles = qgamma(probabilities, shape = mean.ozone^2/var.ozone, scale = var.ozone/mean.ozone)


# gamma quantile-quantile plot for "ozone"
png('analyse/gamma-qqplot.png')
plot(sort(gamma.quantiles), sort(ozone), xlab = 'Theoretical Quantiles from Gamma Distribution', ylab = 'Sample Quantiles of Ozone', main = 'Gamma Quantile-Quantile Plot of Ozone')
abline(0,1)
dev.off()

# histogram with kernel density estimate
png('analyse/histogram and kernel density plot.png')
hist(ozone, breaks = 15, freq = F, xlab = 'Ozone (ppb)', ylim = c(0, 0.025), ylab = 'Probability', main = 'Histogram of Ozone Pollution Data with Kernel Density Plot')
lines(density(ozone, na.rm = T, from = 0, to = max.ozone))
dev.off()

# histogram with normal density curve
png('analyse/histogram and gamma density plot.png')
hist(ozone, breaks = 15, freq = F, xlim = c(0, 170), ylim = c(0, 0.025), xlab = 'Ozone (ppb)', ylab = 'Relative Frequency', main = 'Histogram of Ozone Pollution Data with Gamma Density Curve')
curve(dgamma(x, shape = mean.ozone^2/var.ozone, scale = var.ozone/mean.ozone), add = T)
dev.off()

# histogram with normal density curve
png('analyse/histogram and normal density plot.png')
ozone.histogram = hist(ozone, breaks = 50, freq = F)
ozone.ylim.normal = range(0, ozone.histogram$density, dnorm(ozone, mean = mean.ozone, sd = sd.ozone), na.rm = T)
hist(ozone, breaks = 15, freq = F, ylim = c(0, 0.025), xlab = 'Ozone (ppb)', ylab = 'Probability', main = 'Histogram of Ozone Pollution Data with Normal Density Curve')
curve(dnorm(x, mean = mean.ozone, sd = sd.ozone), add = T)
dev.off()


## https://onlinecourses.science.psu.edu/stat857/node/4
# Scatterplot: Wage vs. Age by race

if(!require(ISLR)){
  install.packages("ISLR")
}
library(ISLR)

with(Wage, plot(age, wage, col = c("lightgreen","navy", "mediumvioletred", 
                                   "red")[race], pch = 19, cex=0.6))
legend(70, 310, legend=levels(Wage$race), col=c("lightgreen","navy", 
                                                "mediumvioletred", "red"), bty="n", cex=0.7, pch=19)
title(main = "Relationship between Age and Wage by Race")
