if(!require(Cairo)){
  install.packages("Cairo")
}
if(!require(xlsx)){
  install.packages("xlsx")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(scales)){
  install.packages("scales")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(plotly)){
  install.packages("plotly")
}
library(xlsx)
library(Cairo)
library(ggplot2)
library(scales)
library(dplyr)
library(grid)
library(plotly)

d <- read.csv(file = "data/mp_data.csv", header = T, sep = ";", stringsAsFactors = F)

data <- d

# convert character to date
data$occ_date <- as.POSIXct(data$occ_date, format="%d/%m/%Y")

# removing na's
data <- na.omit(data)

# see general aspects of the data
str(data)

# view top few records of data
head(data)

# Five Numbersummary & interqurtile range
# Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) for the input data.
fivenum(data$census_trac)

# data's summary
summary(data)

# uniques
unique(data$CATEGORY)
unique(data$CALL.GROUPS)
unique(data$final_case_type)
unique(data$CASE.DESC)
unique(data$census_tract)

## plots

category <- "category.png"
call <- "call.png"
final <- "final.png"
case <- "case.png"
census <- "census.png"
dt <- "district.png"

# Cairo package
# http://gforge.se/2013/02/exporting-nice-plots-in-r/
graphic <- "analyse/bar "

name <- paste(graphic, category)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

# sorting by attribute
data <- arrange(data, CATEGORY, final_case_type)

ggplot(data, aes(CATEGORY)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = CATEGORY), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x = "Category" ,y = "Percent", fill="Category") +
  scale_y_continuous(labels=percent)

dev.off()

name <- paste(graphic, call)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

# sorting by attribute
data <- arrange(data, CALL.GROUPS, final_case_type)

ggplot(data, aes(CATEGORY)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = CATEGORY), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x = "Call Groups" ,y = "Percent", fill="Call Groups") +
  scale_y_continuous(labels=percent)

dev.off()

name <- paste(graphic, final)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*10, 
      height=4*10, 
      pointsize=12*6, 
      dpi=144)

# sorting by attribute
data <- arrange(data, final_case_type, CATEGORY)

ggplot(data, aes(final_case_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = final_case_type), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x = "Final Case" ,y = "Percent", fill="Final Case") +
  scale_y_continuous(labels=percent)

dev.off()

name <- paste(graphic, census)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*14, 
      height=4*14, 
      pointsize=12*10, 
      dpi=144)

# sorting by attribute
data <- arrange(data, census_tract, final_case_type)

ggplot(data, aes(as.character(census_tract))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.character(census_tract)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x = "Census Tract" ,y = "Percent", fill="Census Tract") +
  scale_y_continuous(labels=percent)

dev.off()

name <- paste(graphic, dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*14, 
      height=4*14, 
      pointsize=12*10, 
      dpi=144)

# sorting by attribute
data <- arrange(data, as.character(DISTRICT), final_case_type)

ggplot(data, aes(as.character(DISTRICT))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.character(DISTRICT)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x = "District" ,y = "Percent", fill="District") +
  scale_y_continuous(labels=percent)

dev.off()

## District Density Analysis
# get district's name and coordinates
graphic <- "analyse/geom_density2d "

districts <- distinct(data, DISTRICT, .keep_all = TRUE)

# calculate percentage
get_perc <- data %>%
  group_by(DISTRICT) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(perc = freq*100)

# join district with percentage
districts <- inner_join(districts, get_perc, by = "DISTRICT")
districts <-   select(districts, DISTRICT, x_coordinate, y_coordinate, perc)

# normalize coordinates
districts$x_coordinate <- districts$x_coordinate/100000
districts$y_coordinate <- districts$y_coordinate/100000

districts2 <- with(districts, districts[rep(1:nrow(districts), perc),])

# density's colors
colfunc <- colorRampPalette(c("darkblue", "lightblue", "green", "yellow", "red"))

name <- paste(graphic, dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

ggplot(districts2, aes(x_coordinate, y_coordinate)) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  xlim(c(min(districts$x_coordinate), max(districts$x_coordinate))) + ylim(c(min(districts$y_coordinate), max(districts$y_coordinate))) +
  geom_density2d(colour="black", bins=10) +
  geom_point() + 
  geom_text(aes(label=DISTRICT), size=3, hjust=-.25, vjust=.75) +
  labs(x = "x", y = "y", fill = "Density")
guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  theme(legend.title=element_blank())

dev.off()


##https://www.r-bloggers.com/exploratory-data-analysis-quantile-quantile-plots-for-new-yorks-district-pollution-data/
##### Quantile-Quantile Plots of district Pollution Data
##### By Eric Cai - The Chemical Statistician
# extract "district" data vector
graphic <- "analyse/qqplot "
district = d$DISTRICT

# sample size of "district"
length(district)

# remove missing values from "district"
district = district[!is.na(district)]

# having removed missing values, find the number of non-missing values in "district"
n = length(district)

# calculate mean, variance and standard deviation of "district"
mean.district = mean(district)
var.district = var(district)
sd.district = sd(district)
max.district = max(district, na.rm = T)

# set n points in the interval (0,1)
# use the formula k/(n+1), for k = 1,..,n
# this is a vector of the n probabilities
probabilities = (1:n)/(n+1)

# calculate normal quantiles using mean and standard deviation from "district"
normal.quantiles = qnorm(probabilities, mean(district, na.rm = T), sd(district, na.rm = T))

# normal quantile-quantile plot for "district"
name <- paste(graphic, "normal ", dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

plot(sort(normal.quantiles), sort(district) , xlab = 'Theoretical Quantiles from Normal Distribution', ylab = 'Sample Quqnatiles of district', main = 'Normal Quantile-Quantile Plot of district')
abline(0,1)

dev.off()

# calculate gamma quantiles using mean and standard deviation from "district" to calculate shape and scale parameters
gamma.quantiles = qgamma(probabilities, shape = mean.district^2/var.district, scale = var.district/mean.district)


# gamma quantile-quantile plot for "district"
name <- paste(graphic, "gamma ",dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

plot(sort(gamma.quantiles), sort(district), xlab = 'Theoretical Quantiles from Gamma Distribution', ylab = 'Sample Quantiles of district', main = 'Gamma Quantile-Quantile Plot of district')
abline(0,1)

dev.off()

graphic <- "analyse/histogram "
# histogram with kernel density estimate
name <- paste(graphic, "kernel density ",dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

hist(district, breaks = 15, freq = F, xlab = 'district (ppb)', ylim = c(0, 0.025), ylab = 'Probability', main = 'Histogram of district Pollution Data with Kernel Density Plot')
lines(density(district, na.rm = T, from = 0, to = max.district))

dev.off()

# histogram with gamma density curve
name <- paste(graphic, "gamma density ",dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

hist(district, breaks = 15, freq = F, xlim = c(0, 170), ylim = c(0, 0.025), xlab = 'district (ppb)', ylab = 'Relative Frequency', main = 'Histogram of district Pollution Data with Gamma Density Curve')
curve(dgamma(x, shape = mean.district^2/var.district, scale = var.district/mean.district), add = T)

dev.off()

# histogram with normal density curve
name <- paste(graphic, "normal density ",dt)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)

district.histogram = hist(district, breaks = 50, freq = F)
district.ylim.normal = range(0, district.histogram$density, dnorm(district, mean = mean.district, sd = sd.district), na.rm = T)
hist(district, breaks = 15, freq = F, ylim = c(0, 0.025), xlab = 'district (ppb)', ylab = 'Probability', main = 'Histogram of district Pollution Data with Normal Density Curve')
curve(dnorm(x, mean = mean.district, sd = sd.district), add = T)

dev.off()

### Bivariable Analysis
## histogram 
# district
g <- ggplot(d, aes(DISTRICT)) 
  
g +  geom_histogram(aes(y=..density.., fill = CATEGORY), binwidth = 10) +
  labs(x = "District", y = "Density", fill = "CATEGORY")

g +  geom_histogram(aes(y=..density.., fill = final_case_type), binwidth = 10) +
  labs(x = "District", y = "Density", fill = "Final Case Type")

# date
#date to numeric
data$occ_date <- as.numeric(data$occ_date)

# removing na's
data <- na.omit(data)

# using geom_bar will automatically make a new "count" column
# available in an internal, transformed data frame. the help
# for geom_bar says as much
p <- ggplot(data, aes(occ_date)) +
  scale_y_continuous(labels=percent) +
  theme(legend.position="none")

p1 <- p + geom_bar(aes(y = ..density.., fill = CATEGORY), stat="density") +
  labs(x = "Date" ,y = "Percent", fill="Category")

p2 <- p + geom_bar(aes(y = ..density.., fill = as.character(DISTRICT)), stat="density") +
  labs(x = "Date" ,y = "Percent", fill="District")

p3 <- p + geom_bar(aes(y = ..density.., fill = final_case_type), stat="density") +
  labs(x = "Date" ,y = "Percent", fill="District")