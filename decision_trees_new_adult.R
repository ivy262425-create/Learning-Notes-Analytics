# 1. Collectinng data
adult <- read.csv("E:\\zxy.ntu\\Analytics Solftware\\Individual\\adult.data")
install.packages(c("ggplot2", "dplyr", "readr", "gridExtra"))
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(patchwork)
# 2. Exploring and preparing the data
str(adult)
# Age Distribution (Histogram)
age_plot <- ggplot(adult, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.6) +
  labs(title = "Age Distribution", x = "age", y = "Count") +
  theme_minimal()
age_plot
table(adult$education)
table(adult$race)
#education
edu_plot <- ggplot(adult, aes(x = education)) +
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Education Level Distribution", x = "education", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#hours.per.week
hpw <- ggplot(adult, aes(x = hours.per.week)) +
  geom_histogram(binwidth = 5, fill = "#A8BBA3", color = "#F7F4EA", alpha = 0.7) +
  labs(title = "Hours-per-week Distribution", x = "hours per Week", y = "Count") +
  theme_minimal()
age_plot | hpw
names(adult)
# Income vs Age (Boxplot)
income_age_plot <- ggplot(adult, aes(x = income, y = age, fill = income)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(title = "Income vs Age", x = "Income", y = "Age") +
  theme_minimal() +
  scale_fill_manual(values = c("#FFDBB6", "#67C090"))
income_age_plot
table(adult$income)
adult$income <- as.factor(adult$income)
table(adult$income)
# Income vs Hours-per-week (Boxplot)
income_hours_plot <- ggplot(adult, aes(x = income, y = hours.per.week, fill = income)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(title = "Income vs Hours-per-week", x = "Income", y = "Hours per Week") +
  theme_minimal() +
  scale_fill_manual(values = c("#A8F1FF", "#FFFA8D"))
income_age_plot | income_hours_plot
# Data preparation - creating random training and test datasets
# Using a random sample, and set a seed value
nrow(adult)
set.seed(1233)
train_sample2 <- sample(32561,32561*0.9)
str(train_sample2)
adult_train <- adult[train_sample2,]
adult_test <- adult[-train_sample2,]
prop.table(table(adult_train$income))
prop.table(table(adult_test$income))
# 3. Training a model on the data
install.packages("C50")
library(C50)
#credit_train$default <- as.factor(credit_train$default), require a factor outcome
adult_model <- C5.0(adult_train[-c(3,15)], adult_train$income)
adult_model
# to see the tree's decisions
summary(adult_model)
# 4. Evaluating model performance
adult_pred  <- predict(adult_model, adult_test)
install.packages("gmodels")
library(gmodels)
CrossTable(adult_test$income, adult_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# 5. Improving model performance
# Boosting the accuracy of decision trees
adult_boost20 <- C5.0(adult_train[-c(3,15)], adult_train$income,
                       trials = 20)
adult_boost20
summary(adult_boost20)
adult_boost_pred20 <- predict(adult_boost20, adult_test)
CrossTable(adult_test$income, adult_boost_pred20,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))
# Try random forest
install.packages("randomForest")
library(randomForest)
set.seed(123)
adult_model_rf <- randomForest(income ~ . - fnlwgt, data = adult_train, ntree = 600, importance=TRUE)
adult_model_rf
adult_pred_rf <- predict(adult_model_rf, adult_test)
CrossTable(adult_test$income, adult_pred_rf,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predict default'))
imp <- importance(adult_model_rf)
imp_df <- data.frame(Variable = rownames(imp),
                     MeanDecreaseGini = imp[, "MeanDecreaseGini"])
imp_df <- imp_df[order(-imp_df$MeanDecreaseGini),]
top10 <- head(imp_df,10)
library(ggplot2)
ggplot(data=imp_df, mapping = aes(x = Variable, y = MeanDecreaseGini), color= Variable) + geom_point()

ggplot(top10, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # 水平条形图，变量名更易读
  labs(title = "Top 10 Variable Importance",
       x = "Variable",
       y = "Importance") +
  theme_minimal()
