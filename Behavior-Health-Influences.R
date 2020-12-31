##Load Packages
library(dplyr)
library(tidyverse)
library(ggplot2)

##Load Dataset
getwd()
setwd("C:/Users/Toby/Downloads")
load("brfss.RData")

##Dataset
#The Centers for Disease Control and Prevention (CDC) has an ongoing yearly project called The Behavioral Risk Factor Surveillance System (BRFSS) that collects the data of health factors of adults in the United States. The BRFSS was collected by telephone survey to adults above 18 years old in 2013. The dataset includes 491,775 observations within 330 different variables.

##Research Question 1
###-Is there a correlation between income and mental health?

##Research Question 2
###-Is there a correlation between education and BMI?

##Research Question 3
###-Is there a correlation between exercise and general health levels?

##Question 1
glimpse(brfss2013$income2)
glimpse(brfss2013$menthlth)

brfss2013$menthlth <- as.factor(brfss2013$menthlth)
ivsm <- filter(brfss2013,!is.na(income2), !is.na(menthlth))
ivsm <- select(ivsm,income2, menthlth)

ivsm %>% group_by(income2) %>% summarize(count = n()) 

ivsm %>% group_by(menthlth) %>% summarize(count = n())

colnames(ivsm)[2] <- "mentalhealth"
colnames(ivsm)[1] <- "income"

glimpse(ivsm)
ggplot(aes(x = mentalhealth, y = income), data = ivsm) +
  geom_count() + xlab("Number of Days with Bad Mental Health") +
  ylab("Income") + ggtitle("Income vs Bad Mental Health Days")

#The plot shows a increasing trend that people with higher income have less number of bad mental health days. Although the plot shows this statement's correlation, the data shows that more people with higher incomes were surveyed more than people with lower income. This might have skewed the results of this plot.

##Question 2
glimpse(brfss2013$educa)
glimpse(brfss2013$X_bmi5) 

brfss2013$X_bmi5 <- (brfss2013$X_bmi5)/100
edbmi <- select(brfss2013, educa, X_bmi5) %>% filter(!is.na(X_bmi5), !is.na(educa))
educat <- edbmi %>% group_by(educa) %>% summarize(count = n())

as_tibble(educat) 

edvsbmi<- edbmi %>% group_by(educa) %>% summarize(avg_bmi = mean(X_bmi5))

colnames(edvsbmi)[2] <- "averagebmi"
colnames(edvsbmi)[1] <- "education"

glimpse(edvsbmi)

ggplot(aes(x = averagebmi, y = education), data = edvsbmi) + geom_point() + xlab("Average BMI") + 
  ylab("Education Level") + ggtitle("BMI vs Education Level")
#The plot shows that there is a negative trend that people with lower education levels have a higher BMI. Something interesting when looking at this plot is that the average BMI are quite high. The plot's graph goes from 27 to 29. These levels of BMI are considered "overweight". But according to several health studies of American BMIs, the average American BMI is around 28. Therefore, the plotting of the graph is not incorrect. But the data does show that more people with high school and college education were surveyed the most. This might have affected the data's correlation betwen BMI and education level.

##Question 3
glimpse(brfss2013$exerany2)
glimpse(brfss2013$genhlth)

health <- select(brfss2013, exerany2, genhlth) %>% filter(
  !is.na(exerany2), !is.na(genhlth))
health %>% group_by(genhlth) %>% summarize(count = n())

health %>% group_by(exerany2) %>% summarize(count = n())

colnames(health)[1] <- "exercise"
colnames(health)[2] <- "generalhealth"

glimpse(health)

ggplot(aes(x = generalhealth, y = exercise), data = health) +
  geom_count() + xlab("General Health Level") +
  ylab("Exercise in the Past 30 Days") + ggtitle("General Health vs Exercise")
#This plot shows that people who exercise more have a higher health level. Although it seems there might be a correlation, people might just be more biased towards their own feelings of general health of being good.

##Overall Summary
#The dataset focuses on 491,775 observation within 330 variables pertaining to behavioral health risks of 2013. I focused on examining three research questions and the correlation between two variables. The first question focuses on income and mental health. I used a dot plot to show the correlation between income and bad mental health days. The plot showed an postiive correlation that people with higher income had less number of bad mental health days. The second question focuses on BMI and education level. I used a point plot to show the correlation between the two variables. THe graph shows a negative correlation that the higher the education, the lower the BMI level. The third question focuses on general health and exercise. I used a dot plot to the show the correlation between the two variables. This plot showed a increasing trend that people who exercised more in the past 30 days had better general health.

#~People who higher incomes have less number of bad mental health days

#~People with higher education have lower BMI

#~People who exercise more have better health levels

#Some future improvements:
#  ~Have more lower income people surveyed

#~Have more lower education people surveyed

#~Have more people who do not regularly exercise surveyed

#References
#https://www.cdc.gov/brfss/
