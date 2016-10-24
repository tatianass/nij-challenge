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
