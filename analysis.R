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
library(xlsx)
library(Cairo)
library(ggplot2)
library(scales)
library(dplyr)

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

## Bivariable Analysis
# random sample
#a <- sample_n(data, 1000)
a <- data
a$occ_date <- as.numeric(strftime(a$occ_date, format = "%Y"))
p <- ggplot(a, aes(census_tract, CALL.GROUPS))
p + geom_point()
p + geom_point(aes(colour = CATEGORY))
p + geom_point(alpha = 1/10)

a <- filter(a, census_tract < 10000)
nrow(a)/nrow(data)
p <- ggplot(a, aes(CALL.GROUPS, census_tract))
p + geom_boxplot()

write.xlsx(a, file = "data/ct_m_10000.xlsx")
a <- distinct(data, census_tract, .keep_all = TRUE)
write.xlsx(a, file = "data/cts.xlsx")
