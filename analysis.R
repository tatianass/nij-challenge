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
library(xlsx)
library(Cairo)
library(ggplot2)
library(scales)

data <- read.csv(file = "data/data.csv", header = T, sep = ";", stringsAsFactors = F)

# removing na's
data <- na.omit(data)

# convert character to date
data$occ_date <- as.POSIXct(data$occ_date, format="%m/%d/%Y")

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
ggplot(data, aes(CATEGORY, ..count..)) + geom_bar(aes(fill = CATEGORY), position = "dodge")
dev.off()

name <- paste(graphic, call)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)
ggplot(data, aes(CALL.GROUPS, ..count..)) + geom_bar(aes(fill = CALL.GROUPS), position = "dodge")
dev.off()

name <- paste(graphic, final)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)
ggplot(data, aes(final_case_type, ..count..)) + geom_bar(aes(fill = final_case_type), position = "dodge")
dev.off()

ggplot(data, aes(x= test2,  group=test1)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = ..count.., y= ..prop..), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="test2") +
  facet_grid(~test1) +
  scale_y_continuous(labels=percent)


name <- paste(graphic, case)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)
ggplot(data, aes(CASE.DESC, ..count..)) + geom_bar(aes(fill = CASE.DESC), position = "dodge")
dev.off()

name <- paste(graphic, census)
Cairo(file=name, 
      type="png",
      units="in", 
      width=5*2, 
      height=4*2, 
      pointsize=12*2, 
      dpi=144)
ggplot(data, aes(census_tract, ..count..)) + geom_bar(aes(fill = census_tract), position = "dodge")
dev.off()

## looking if census_tract has something to do with crime's location (analysing with arcgis)
# getting data from 2016 and census_tract = 980000
ct_980000_dt_2016 <- subset(data, census_tract == 980000 & strftime(occ_date, format = "%Y") == 2016)
ct_10600_dt_2016 <- subset(data, census_tract == 10600 & strftime(occ_date, format = "%Y") == 2016)

write.xlsx(ct_980000_dt_2016, "data/ct_980000_dt_2016.xlsx") 
write.xlsx(ct_10600_dt_2016, "data/ct_10600_dt_2016.xlsx") 

ggplot(ct_980000_dt_2016, aes(CATEGORY, ..count..)) + geom_bar(aes(fill = CATEGORY), position = "dodge")

