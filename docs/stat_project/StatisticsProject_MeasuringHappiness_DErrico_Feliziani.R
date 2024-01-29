###### STATISTICS PROJECT ######
##### Measuring Happiness ######

# Elisa D'Errico
# Arianna Feliziani


# Install Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("tidyverse")
install.packages("olsrr")
install.packages("caret")
install.packages("MASS")

library("olsrr")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("car")
library("caret")
library("MASS")
select <- dplyr::select 


# Import data frames

# Happiness
happy <- read.csv("happiness-cantril-ladder.csv")
dfh <- data.frame(happy)

# Rights 
rights <- read.csv("distribution-human-rights-vdem.csv")
dfr <- data.frame(rights)
dfr$region <- NULL

# Income
income <- read.csv("economic-inequality-gini-index.csv")
dfi <- data.frame(income)

# Education
edu <- read.csv("total-government-expenditure-on-education-gdp.csv")
dfe <- data.frame(edu)

# Health Expenditure
health <- read.csv("share-of-public-expenditure-on-healthcare-by-country.csv")
dfhe <- data.frame(health)

# Mental diseases
mental <- read.csv("people-with-mental-health-disorders.csv")
dfm <- data.frame(mental)

# GNI
growth <- read.csv("gross-national-income-per-capita.csv")
dfg <- data.frame(growth)

# Homicides
crime <- read.csv("share-of-deaths-homicides.csv")
dfc <- data.frame(crime)

# Internet
internet <- read.csv("share-of-individuals-using-the-internet.csv")
dfn <- data.frame(internet)




# Make the table and select the years
df.list <- list(dfh, dfr, dfi, dfhe, dfm, dfg, dfc, dfn, dfe)
tb <- Reduce(function(x, y) merge(x, y, all=FALSE), df.list)
colnames(tb)[4] = "Satisfaction"
colnames(tb)[5] = "CivilRights"
colnames(tb)[6] = "IncomeInequality"
colnames(tb)[7] = "HealthExpenditure"
colnames(tb)[8] = "MentalHealth"
colnames(tb)[9] = "GNI"
colnames(tb)[10] = "Homicides"
colnames(tb)[11] = "Internet"
colnames(tb)[12] = "Education"
#temp <- tb %>% select(-Entity)
tb <- tb[tb$Year > "2013",]
tb.final <- tb %>% select(-Entity, -Year, -Code)

# Run Linear Regression with Multicolinearity Issue

multi.reg <- lm(Satisfaction ~ ., data = tb.final)
summary(multi.reg)

#### comment: P value of F statistics is significant, thus the model is useful ###

# Perform Correlation to find Multicolinearity -> two methods
# 1: Pairwise Scatterplot
pairs(tb.final,pch=19)

# 2: Perform VIF's Test to study correlation between the predictors
vif(multi.reg)

### comment: since values are < 10 there is no significant correlation ###

# Perform Diagnostic Plot
par(mfrow=c(2,2))
plot(multi.reg,which=1:4)

# Histogram of Residuals
hist.res <- residuals(multi.reg)
n.line <- seq( min(hist.res), max(hist.res), length = 60)
nc <- dnorm( n.line , mean = mean(hist.res), sd = sd(hist.res))
hist( hist.res, main="Histogram of residuals", prob = TRUE, ylim = c(0, max(nc)), xlab = "Residuals")
lines(n.line, nc, col = 2, lwd = 2)
boxplot(hist.res)
plot(density(hist.res), main="Density plot of residuals", ylab="Density",xlab="Residuals")

### comments: no pattern in the residual plot -> homoscendasticity
###           normal distribution in QQ plot
###           points greater than 0.015 in Cook's Distance


#kolmogorov-smirnov

ols_test_normality(multi.reg) #cannot reject the null hypothesis since p value greater than 0.05

######## SUBSET SELECTION SUMMARY ########


# Best Subset Summary
multi.reg.model.subset <- ols_step_best_subset(multi.reg)
multi.reg.model.subset

# Step Both
model <- lm(Satisfaction ~ ., data = tb.final)
ols_step_both_p(model)

# Step Forward
forward <- stepAIC(multi.reg, direction = "forward", trace = FALSE)
forward

# Step Backward
backward <- stepAIC(multi.reg, direction = "backward", trace = FALSE)
backward


# Study of the regression without influencial points

# Influential Points

cook.tb.final <- cooks.distance(multi.reg)
plot(cook.tb.final,pch=19,cex=1)

# Delete observations larger than criteria 0.015

tb.final.id <- which(cooks.distance(multi.reg)>0.015)
n.multi.reg <- lm(Satisfaction ~ ., data=tb.final[-tb.final.id,])
summary(n.multi.reg)

# Diagnostic Plot without Influencial Points
par(mfrow=c(2,2))
plot(n.multi.reg, which=c(1:4)) 

n.hist.res <- residuals(n.multi.reg)
n.n.line <- seq( min(n.hist.res), max(n.hist.res), length = 60)
n.nc <- dnorm( n.n.line , mean = mean(n.hist.res), sd = sd(n.hist.res))
hist( n.hist.res, main="Histogram of residuals", prob = TRUE, ylim = c(0, max(n.nc)), xlab = "Residuals")
lines(n.n.line, n.nc, col = 2, lwd = 2)

boxplot(n.hist.res)
plot(density(n.hist.res),main="Density plot of residuals",ylab="Density",xlab="Residuals")

### comment: Regression lines with/without influential points are almost the same.

#kolmogorov-smirnov

ols_test_normality(n.multi.reg) #cannot reject the null hypothesis since p value greater than 0.05

# Best Subset Summary
n.multi.reg.model.subset <- ols_step_best_subset(n.multi.reg)
n.multi.reg.model.subset

# Step Both
n.model <- lm(Satisfaction ~ ., data = tb.final[-tb.final.id,])
ols_step_both_p(n.model)

# Step Forward
n.forward <- stepAIC(n.multi.reg, direction = "forward", trace = FALSE)
n.forward

#Step Backward
n.backward <- stepAIC(n.multi.reg, direction = "backward", trace = FALSE)
n.backward

