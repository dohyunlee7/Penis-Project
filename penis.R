

library(ggplot2)
library(tidyverse)
library(magrittr)

# read in dataset
penis_data <- read.csv("/Users/Dohyun/Desktop/projects/Penis-Project/world_penis_dataset/penis.csv")


#check normality of erect length means

#using a histogram
hist(penis_data$length_erect)

#NPP plot
qqnorm(penis_data$length_erect)
qqline(penis_data$length_erect)



#check normality of erect girth means

#using a histogram
hist(penis_data$circumf_erect)

#NPP plot
qqnorm(penis_data$circumf_erect)
qqline(penis_data$circumf_erect)



t.test(penis_data$length_erect)
t.test(penis_data$circumf_erect)


#Confidence interval for mean erect length is 13.56-14.16 cm.
#Confidence interval for mean erect girth is 11.72-11.96 cm.


#check for overlaps between both methods
self_reported_data <- filter(penis_data, Method == "Self reported")
measured_data <- filter(penis_data, Method == "Measured")

t.test(self_reported_data$length_erect)
#CI for self-reported length: 14.33-15.08
t.test(measured_data$length_erect)
#CI for measured length: 12.97-13.75

t.test(self_reported_data$circumf_erect)
#CI for self-reported girth: 11.55-11.86
t.test(measured_data$circumf_erect)
##CI for measure length: 11.90-12.25


size_length <- length(penis_data$length_erect)
size_girth <- length(penis_data$circumf_erect)
region <- penis_data[,"Region"]
length <- penis_data[,"length_erect"]
girth <- penis_data[,"circumf_erect"]

#boxplot of the regions vs length
bp <- ggplot(penis_data, aes(x = region, y = length)) +
  geom_boxplot()
bp + coord_flip()

#boxplot of the regions vs girth
bp2 <- ggplot(penis_data, aes(x = region, y = girth)) +
  geom_boxplot()
bp2 + coord_flip()

#What is the relationship between length and girth?
  
#create a linear regression model bt length & girth
fit1 <- lm(girth ~ length)
summary(fit1)
