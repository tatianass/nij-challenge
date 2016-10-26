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

## Bivariable Analysis
# random sample
#a <- sample_n(data, 1000)
