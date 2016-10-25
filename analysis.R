if(!require(devtools)){
  install.packages("devtools")
}

if(!require(xda)){
  install_github("ujjwalkarn/xda")
}
library(devtools)
library(xda)

anova_fun <- function(analysis, type){
  
  #anova
  #h0: they are equal, h1: they are not (p<0.05)
  anova <- aov(analysis)
  
  file_name <- " anova sumary.txt"
  file_name <- paste(type, file_name)
  s<-summary(anova)
  capture.output(s, file = file_name)
  
  name <- " plot witch 1.png"
  name <- paste(type, name)
  png(filename=name)
  #Caso queira visualizar o gráfico
  plot(analysis, which = 1)
  dev.off()
  
  name <- " plot witch 2.png"
  name <- paste(type, name)
  png(filename=name)
  #can se that there's a positive skewness in the data
  plot(analysis, which = 2)
  dev.off()
  
  sresids <- rstandard(analysis)
  
  name <- " histogram.png"
  name <- paste(type, name)
  png(filename=name)
  hist(sresids)
  dev.off()
  
  #to know the variable that are significantly different
  # significant difference p < 0.05
  #there's just two types, so no need for turkey's test
  file_name <- " TukeyHSD sumary.txt"
  file_name <- paste(type, file_name)
  s<-TukeyHSD(anova)
  capture.output(s, file = file_name)
  
  
}

data <- read.csv(file = "data/data.csv", header = T, sep = ";", stringsAsFactors = F)

data$occ_date <- as.POSIXct(data$occ_date, format="%m/%d/%Y")

name <- "analyse/type stripchart.png"
png(filename=name)

#visualizing data
stripchart(as.numeric(data$occ_date)~CATEGORY,
           data=data,
           main="Different strip chart for each type",
           xlab="Type",
           ylab="Time",
           col="brown3",
           vertical=TRUE,
           pch=19,
           method = "jitter", jitter = 0.004
)
dev.off()


#fitting data to anova
aov_category <- lm(as.numeric(data$occ_date)~CATEGORY,data=data)

anova_fun(aov_category, "analyse/type")

library(foreign)
##dbf
d <- read.dbf("C:/Users/tatiana/Documents/tutorial - arcgis/Join_Output_2.dbf", as.is = FALSE)
