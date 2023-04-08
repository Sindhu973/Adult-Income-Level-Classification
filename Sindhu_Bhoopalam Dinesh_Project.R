# Adult Income Level Classification
# Data Source: https://www.kaggle.com/datasets/uciml/adult-census-income

# All references( Ref ) provided are for R in Action, 3rd Edition, 
# ISBN:9781617296055, unless a different ISBN is explicitly mentioned
rm(list=ls())
dev.off(dev.list()["RStudioGD"])
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(digits=7, scipen=0)
opar <- par(no.readonly=TRUE)

# Load data set by reading the csv file
adultIncome <- read.csv('Data/adult.csv')

# Explore data set
# Ref: 2.5 Useful functions for working with data objects
dim(adultIncome)
names(adultIncome)
head(adultIncome)
tail(adultIncome)
# Some missing values can be seen on some columns like workclass, occupation  
str(adultIncome)
summary(adultIncome)
# Explore descriptive stats
# Ref: 7.1.2 Even more methods
library(psych)
describe(adultIncome)
# Data set has around 32k records with 15 variables,one of which is target 
# variable
# Variables are a mix of continuous and categorical
# Continuous variables like age, number of years of education and hours per week
# are somewhat evenly spread out in their range

# Data Cleaning 

# Replace missing values with NA
adultIncome[adultIncome == '?'] <- NA
str(adultIncome)

# Convert categorical columns to Factors - going step by step to check the 
# unique values and then decide how to convert the column
# Ref: 3.7 Type conversions
unique(adultIncome$workclass)
adultIncome$workclass <-  as.factor(adultIncome$workclass)
levels(adultIncome$workclass)
unique(adultIncome$education)
adultIncome$education <-  as.factor(adultIncome$education)
levels(adultIncome$education)
# Not using an ordered factor for Education because some may have similar levels
# - "Bachelors", "Some college"
unique(adultIncome$marital.status)
adultIncome$marital.status <-  as.factor(adultIncome$marital.status)
levels(adultIncome$marital.status)
unique(adultIncome$occupation)
# occupation can be a character as there are many occupations available
unique(adultIncome$relationship)
adultIncome$relationship <-  as.factor(adultIncome$relationship)
levels(adultIncome$relationship)
adultIncome$race <-  as.factor(adultIncome$race)
levels(adultIncome$race)
adultIncome$sex <-  as.factor(adultIncome$sex)
levels(adultIncome$sex)
unique(adultIncome$native.country)
#  native.country can be a character as there are many countries

# Analyse Missing Values

# Ref: 18.2 Identifying missing values
# Check count of missing values for each column
sapply(adultIncome,function(x) sum(is.na(x)))
# Variables workclass, occupation, native.country have missing values

# Check complete cases
table (complete.cases (adultIncome))

# Explore the missing data graphically
# Ref: 18.3.1 Visualizing missing values
library(mice)
md.pattern(adultIncome)

library("VIM")
aggr(adultIncome, prop=FALSE, numbers=TRUE)

# From the above two types of plots, it can be seen that:
# 'occupation' and 'workclass' missing together for 1809 entries
# 'workclass' is always missing with 'occupation', which makes sense
# as there is no workclass if no occupation, this is MAR, can be deleted
# native.country is missing only 583 
# only 27 times native.country is missing with the other two variables
# native.country could be MCAR as it is not dependent on any observed/hidden 
# variable, the missing entries can be removed 

# Listwise deletion as it is also not advisable to impute categorical values
# Ref: 3.5.2 Excluding missing values from analyses
adultIncome <- adultIncome[complete.cases(adultIncome),]
sapply(adultIncome,function(x) sum(is.na(x)))
# No more missing vales

# Analyse Variables and Correlations

# Check correlation between the variables for further analysis
# Ref: 11.3 Corrgrams
library(corrgram)
corrgram(adultIncome, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.conf, diag.panel=panel.minmax, 
         text.panel=panel.txt,
         main="Correlogram of Adult Income intercorrelations")
# As we can see this only gives us correlation for the continuous variables
# So we try mosaic plots

# Entire data set is large, mosaic plot errors out, try with few variables first
# Ref: 11.4 Mosaic plots
library(vcd)
mosaic(~income+workclass+education, data=adultIncome, 
       shade=TRUE, legend=TRUE)
# Complicated to analyse, lets work variable category-wise

# Analyse continuous variables w.r.t income
library(ggplot2)
library(gridExtra)
p1 <- ggplot(aes(x=income, y=age), data = adultIncome) + geom_boxplot() + 
  ggtitle('Age')
p2 <- ggplot(aes(x=income, y=education.num), data = adultIncome) + 
  geom_boxplot() + ggtitle('Years of Education')
p3 <- ggplot(aes(x=income, y=hours.per.week), data = adultIncome) + 
  geom_boxplot() + ggtitle('Hours Per Week')
p4 <- ggplot(aes(x=income, y=capital.gain), data=adultIncome) + 
  geom_boxplot() + ggtitle('Capital Gain')
p5 <- ggplot(aes(x=income, y=capital.loss), data=adultIncome) + 
  geom_boxplot() + ggtitle('Capital Loss')
p6 <- ggplot(aes(x=income, y=fnlwgt), data=adultIncome) + geom_boxplot() +
  ggtitle('Final Weight')
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

# 'fnlwgt' has no variation for w.r.t income,thus can be excluded from analysis
# 'age', 'education.num', 'hours.per.week' have 
# substantial variation w.r.t income
# 'capital.gain' and 'capital.loss' are unclear

# Analyse categorical variables w.r.t income
library(dplyr)
library(MASS) 
library(reshape2) 
library(reshape) 
# Define functions for analysis
# Ref: 5.6.3 The reshape package - ISBN:9781935182399
meltData <- function(var){
  tbl <- ftable(var=adultIncome[,var], adultIncome$income)
  dt <- melt(tbl,id=c(var))
  return (dt)
  
}
# Ref: 4.1.1 ggplot
plotData <- function(data, name){
  plt <- ggplot(aes(x=value.var, y=value.Freq, fill=value.Var2), data=data) + 
                geom_bar(stat = 'identity', position = position_dodge()) +
                ggtitle(sprintf('%s', name))+
                xlab(sprintf('%s', name)) +
                ylab('frequency') +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(plt)
}

by_workclass <- meltData('workclass')
by_education <- meltData('education')
by_marital <- meltData('marital.status')
by_occupation <- meltData('occupation')
by_relationship <- meltData('relationship')
by_race <- meltData('race')
by_sex <- meltData('sex')
by_country <- meltData('native.country')

p7 <- plotData(by_workclass, 'workclass')
p8 <- plotData(by_education, 'education')
p9 <- plotData(by_marital, 'marital status')
p10 <- plotData(by_occupation, 'occupation')
p11 <- plotData(by_relationship, 'relationship')
p12 <- plotData(by_race, 'race')
p13 <- plotData(by_sex, 'sex')
p14 <- plotData(by_country, 'country')
grid.arrange(p7, p8, p9, p10, ncol=2)
grid.arrange(p11, p12, p13, p14, ncol=2)

# Most values are spread out w.r.t income level, so they are important
# 'native.country' seems to be heavily dominated by 'United States',
# thus could be deleted as it might not add weight to the analysis

# Run tests of independence on some categorical variables 
# to confirm the analysis
# Ref: 7.2.2 Tests of independence
# Chisq Test
# Occupation is spread all over, perform t-test
edu <- xtabs(~income+education, data=adultIncome)
chisq.test(edu)
# p-value < 2.2e-16, thus income isn't independent of education
country <- xtabs(~income+race, data=adultIncome)
chisq.test(country)
# p-value < 2.2e-16, thus income isn't independent of race

# Run Cochran–Mantel–Haenszel chi-square test
occRel <- xtabs(~income+occupation+relationship, data=adultIncome)
mantelhaen.test(occRel)
# p-value < 2.2e-16, income isn't independent of occupation, relationship

# The results seem to align with the previous analysis, all the variables 
# are kept

# After analysing continuous and categorical variables, 'fnlwgt' and 
# 'native.country' removed from the data set for reasons stated above
adultIncome <- subset(adultIncome, select=-c(fnlwgt, native.country))

# For further analysis, target variable 'income' made into a factor with values 
# 0 for <=50k and 1 for > 50k
adultIncome$income = as.factor(
                    ifelse(adultIncome$income==adultIncome$income[1],0,1))
str(adultIncome)

# Model Fitting

# Comparative analyses is performed using 3 types of classification models-
# Logistic Regression, Decision Trees, SVM

# Split the data set into train( 80% ) and test( 20% ) sets
# Ref: 17.1 Preparing the data
set.seed(1234)
split <- sample(nrow(adultIncome), 0.8*nrow(adultIncome))
train <- adultIncome[split,]
test <- adultIncome[-split,]

# Define function for analyzing metrics
# Check accuracy and other metrics as just accuracy is not sufficient -
# Ref: 17.6 Choosing a best predictive solution
# AUC curve checks all
# install.packages('ROCR')
library(ROCR)
print_metrics <- function(prob, pred){
  misClassError <- round( mean(prob != test$income), 4)
  print(paste('Accuracy',1-misClassError))
  
  pr <- prediction(pred, test$income)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  auc <- performance(pr, measure = "auc")
  auc <- round( auc@y.values[[1]], 4 )
  print(paste('AUC',auc))
}

# Model 1: Logistic Regression

# Ref: 3.2 Logistic regression
# Fit the model with all variables
fit.logit1 <-glm(income ~.,family=binomial(link='logit'),data=train)
summary(fit.logit1)
# 'age', “hours.per.week”, 'sex', 'capital.gain' and 'capital.loss'
# are the most statistically significant variables
# 'workclass', 'marital.status', education', 'occupation' and 'relationship' 
# are all over the place, so cannot be removed
# 'race' is statistically insignificant, can be removed
# 'education.num' is NA, according to below, it is because it is correlated 
# with another variable, we can remove this from analysis
# https://stats.stackexchange.com/questions/212903/na-in-glm-model
fit.logit1 <-glm(income ~.-education.num,family=binomial(link='logit'),
                 data=train)
summary(fit.logit1)
# AIC is still the same as previous - 15748

# Fit the model without 'race'
fit.logit2 <-glm(income ~.-race+education.num,family=binomial(link='logit'),
             data=train)
summary(fit.logit2)
# But, AIC actually increased - 15757!

# Perform ANOVA analysis on model fit.logit1
# Ref: 9.3 One-way ANOVA
fit <- anova(fit.logit1, test="Chisq")
fit
# all variables are very significant with p-value < 2.2e-16
# 'race' is relatively less significant - 0.0003504, but still significant
# Thus, 'race' cannot be removed and will still be kept in analysis

# Make predictions with the best model
logit.prob <- predict(fit.logit1, newdata=test, type='response')
logit.pred <- ifelse(logit.prob > 0.5,1,0)
table(test$income, logit.pred, dnn=c("Actual","Predicted"))

# Check metrics
print_metrics(logit.pred, logit.prob)
# Accuracy 0.8473
# AUC 0.9055

# Model 2: Decision Trees
# Ref: 17.3.1 Classical decision trees
# Fit the model
library(rpart)
dtree <- rpart(income ~., data=train, method="class",
               parms=list(split="information")) 
dtree$cptable
# As seen from the cptable, tree with nsplit=3 has a minimum 
# cross-validated error(xerror) is 0.64 with standard error(xstd) of 0.009
# The smallest tree with a crossvalidated error within 0.64 ± 0.009 
# (that is, between 0.631 and 0.649) can be selected
# In this case, it is same tree i.e., tree with nsplit = 4
dtree.pruned <- prune(dtree, cp=.01) 

# Plot the tree
library(rattle)
fancyRpartPlot(dtree.pruned, sub="Classification Tree")

# Make predictions to check performance
dtree.pred <- predict(dtree.pruned, test, type="class") 
dtree.perf <- table(test$income, dtree.pred,
                    dnn=c("Actual", "Predicted"))
dtree.perf

# Check metrics
dtree.prob <- predict(dtree.pruned, test, type="prob")[,2]
print_metrics(dtree.pred, dtree.prob)
# "Accuracy 0.8369"
# "AUC 0.8473"

# Model 3: SVM
# Ref: 17.5 Support vector machines
# Fit the model
library(e1071)
set.seed(1234)
fit.svm <- svm(income~., data=train, probability = TRUE)
fit.svm
# Make predictions
svm.pred <- predict(fit.svm, test, probability = TRUE)
svm.perf <- table(test$income, svm.pred, dnn=c("Actual", "Predicted"))
svm.perf

# Check metrics
svm.prob <- attr(svm.pred,"probabilities")[,2]
print_metrics(svm.pred, svm.prob)
# "Accuracy 0.8473"
# "AUC 0.9025"

# Logistic Regression performs best with Accuracy of ~85% and AUC of 0.9055
###############################################################################