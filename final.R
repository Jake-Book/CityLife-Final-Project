## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
library(readr)
data_raw <- read_csv("raw_data.csv")
dataset <- na.omit(data_raw) 
View(dataset)


# set working directory
setwd("C:/Users/jakeb/OneDrive/Desktop/sta_215")

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(dataset$land_locked)
table(dataset$football)
table(dataset$pov_over10)


max(dataset$land_locked)
min(dataset$land_locked)
max(dataset$football)
min(dataset$football)
max(dataset$pov_over10)
min(dataset$pov_over10)

mean(dataset$gdp_per_capital)
sd(dataset$gdp_per_capital)
max(dataset$gdp_per_capital)
min(dataset$gdp_per_capital)

mean(dataset$crime_avg)
sd(dataset$crime_avg)
max(dataset$crime_avg)
min(dataset$crime_avg)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################

boxplot(gdp_per_capital ~ pov_over10, data = dataset)

anova <- aov(pov_avg ~ pov_over10, data = dataset)

summary(anova)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
plot(dataset$gdp_per_capital, dataset$crime_avg)

meany <- mean(dataset$crime_avg)
meanx <- mean(dataset$gdp_per_capital)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")
lm<- lm(crime_avg ~ gdp_per_capital, data = dataset)
summary(lm)
abline(lm, col = "red")



##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$gdp_per_capital, residuals(lm))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$football, dataset$land_locked)

chisq.test(table(dataset$lon_trm, dataset$land_locked))