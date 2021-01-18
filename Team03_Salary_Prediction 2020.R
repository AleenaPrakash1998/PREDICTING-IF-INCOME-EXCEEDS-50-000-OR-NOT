# BEGINNING PROCESS
# clearing out my work-space
rm(list = ls())

# reading the current working directory
getwd()

# set working directory
setwd("./Data_Science/Project")

# loading in the dataset
adult <- read.table(file = "Project_3_adult.csv", 
                    header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Dimension of our dataset
dim(adult)

# Returns the first few parts of our  data frame.
head(adult)

# Displaying compactly the internal structure of our adult dataset.
str(adult)

# Producing a summary of all records in our data set
summary(adult)

# MISSING VALUES
# Total percentage of missing values
(sum(adult == "?") / nrow(adult))*100
# Number of missing values in each columns
sapply(adult, function(x) sum(x == "?"))
# Dataset with no missing values
refined = adult[apply(adult != "?", 1, all),]

# FUNCTION TO INSTALL PACKAGES
# Installation of packages
# Loading the neccessary libraries
installPackage <- function(nameOfThePackage) {
  install.packages(nameOfThePackage)
  library(nameOfThePackage)
  return ("installed")
}

# DATA EXPLORATION & DATA VISUALIZATION

# Age
# Histogram of Age
hist(refined$age, main = paste("Histogram of Age"), col = "olivedrab")
if (!require(ggplot2)) do.call("installPackage", args = list("ggplot2"))
if (!require(dplyr)) do.call("installPackage", args = list("dplyr"))
# Income Level with Age Level
# plot(refined$age, refined$income)
agg <- count(refined, age, income)
agg_ord <- mutate(agg, age, income)
ggplot(agg_ord) + geom_col(aes(x = age, y = n, fill = income)) + 
  ggtitle('Income Level with Age Level')


# workclass
# Exploring workclass of the individual
ggplot(refined, aes(workclass)) + geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring workclass of the individual')
# Counting with respect to columns
refined %>%
  group_by(workclass) %>%
  summarise(counts = n())
# Income Level with Workclass Level
agg <- count(refined,workclass,income)
agg_ord <- mutate(agg, workclass, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x=workclass,y=n,fill=income)) + 
  ggtitle('Income Level with Workclass Level')


# fnlwgt
# Histogram of fnlwgt
hist(refined$fnlwgt, main = paste("Histogram of final weight"), col = "olivedrab" )
# Counting with respect to columns
refined %>%
  group_by(fnlwgt) %>%
  summarise(counts = n())


# education
# Exploring the highest level of Education of the individual
ggplot(refined, aes(education)) + 
  geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring the highest level of Education of the individual')
# Counting with respect to columns
refined %>%
  group_by(education) %>%
  summarise(counts = n())
# Income Level with Education Level
agg <- count(refined, education,income)
agg_ord <- mutate(agg, education, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x = education, y = n,fill = income)) + 
  ggtitle('Income Level with Education Level')


# marital.status
# Exploring the marital status of the individual
ggplot(refined, aes(marital.status)) + 
  geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring the marital status of the individual')
# Counting with respect to columns
refined %>%
  group_by(marital.status) %>%
  summarise(counts = n())
# Income Level with Marital status of the individual
agg <- count(refined, marital.status,income)
agg_ord <- mutate(agg, marital.status, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x = marital.status, y = n,fill = income)) + 
  ggtitle('Income Level with Marital status of the individual')


# occupation
# Counting with respect to columns
refined %>%
  group_by(occupation) %>%
  summarise(counts = n())
# Exploring the Occupation of the individual
ggplot(refined, aes(occupation)) + 
  geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring the Occupation of the individual')
# Income Level with Occupation of the individual
agg <- count(refined, occupation,income)
agg_ord <- mutate(agg, occupation, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x = occupation, y = n,fill = income)) + 
  ggtitle('Income Level with Occupation of the individual')


# race
# Counting with respect to columns
refined %>%
  group_by(race) %>%
  summarise(counts = n())
# Exploring the Race
ggplot(refined, aes(race)) + 
  geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring the Race')
# Income Level with Race of the individual
agg <- count(refined, race,income)
agg_ord <- mutate(agg, race, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x = race, y = n,fill = income)) + 
  ggtitle('Income Level with Race of the individual')


# sex
# Counting with respect to columns
refined %>%
  group_by(sex) %>%
  summarise(counts = n())
# Exploring the gender of the individual
ggplot(refined, aes(sex)) + 
  geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring the gender of the individual')
# Income Level with Biological Gender of the individual
agg <- count(refined, sex,income)
agg_ord <- mutate(agg, sex, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x = sex, y = n,fill = income)) + 
  ggtitle('Income Level with Biological Gender of the individual')


# hours.per.week
# Counting with respect to columns
refined %>%
  group_by(hours.per.week) %>%
  summarise(counts = n())
# Exploring the hours the individual works on weekly basis
ggplot(refined, aes(hours.per.week)) + 
  geom_bar(fill = "olivedrab") + 
  ggtitle('Exploring the hours the individual works on weekly basis') + 
  ylim(0, 100)
# Income Level with hours per Week
agg <- count(refined, hours.per.week,income)
agg_ord <- mutate(agg, hours.per.week, income = reorder(income, -n, sum))
ggplot(agg_ord) + geom_col(aes(x = hours.per.week, y = n,fill = income)) + 
  ggtitle('Income Level with hours per Week') + 
  ylim(0, 100)


# DATA MODELLING

# if needed, then install the packages
# if already installed, then ignore
if (!require(caTools)) do.call("installPackage", args = list("caTools"))
if (!require(caret)) do.call("installPackage", args = list("caret"))

set.seed(12345)
# Spliting data for Buildind the Model
SampleSplit <- sample.split(refined$income, SplitRatio = 0.7)
trainingSet <- subset(refined, SampleSplit == TRUE)
testingSet <- subset(refined,SampleSplit == FALSE)

# Decision Tree
if (!require(rpart)) do.call("installPackage", args = list("rpart"))
if (!require(rpart.plot)) do.call("installPackage", args = list("rpart.plot"))
# DATA MODEL
salaryTree <- rpart(income ~. , data = refined)
# PREDICTION
PredictIncomeTree <- predict(salaryTree, testingSet, type = 'class')
# CHECKING ACCURACY
confusionMatrix(PredictIncomeTree, testingSet $ income)


# Naive Bayes
# if needed, then install the packages
# if already installed, then ignore
if (!require(e1071)) do.call("installPackage", args = list("e1071"))
# DATA MODEL
salaryNaiveBayes <- naiveBayes(income ~. , data = refined)
# PREDICTION
PredictIncomeNaiveBayes <- predict(salaryNaiveBayes, testingSet, type = 'class')
# CHECKING ACCURACY
confusionMatrix(PredictIncomeNaiveBayes, testingSet $ income)

# Random Forest
if (!require(randomForest)) do.call("installPackage", args = list("randomForest"))
# DATA MODEL
salaryForest <- randomForest(income ~. , data = refined)
# PREDICTION
PredictIncomeForest <- predict(salaryForest, testingSet, type = 'class')
# CHECKING ACCURACY
confusionMatrix(PredictIncomeForest, testingSet $ income)