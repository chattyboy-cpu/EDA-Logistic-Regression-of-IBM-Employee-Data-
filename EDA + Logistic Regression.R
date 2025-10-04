---
title: "Linear Regression"
author: ',Chatty'
date: "2025-06-30"
output: html_document
---
Loading the dataset
```{r}
df = read.csv("C:\\Users\\lamin\\Downloads\\WA_Fn-UseC_-HR-Employee-Attrition.csv")
```
```{r}
head(df,15)
```
loading the necessary libries
```{r}
library(dplyr)
library(ggplot2)
library(ggthemes)
```


```{r}
summary(df)
```
```{r}
attach(df)
```


```{r}
#filter rows where attrition is NO and select the first 550 rows
attrition_no = df %>% 
  filter(Attrition == "No") %>%
  slice_head(n= 550)
```
```{r}
attrition_no
```
```{r}
library(dplyr)
library(ggplot2)

# Create age groups
df <- df %>%
  mutate(AgeGroup = case_when(
    Age < 30 ~ "<30",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 ~ "50+"
  ))

# Summarize attrition by age group
attrition_by_age <- df %>%
  group_by(AgeGroup) %>%
  summarise(
    Total = n(),
    LeftJob = sum(Attrition == "Yes"),
    StayJob = sum(Attrition == "No"),
    AttritionRate = LeftJob / Total
  )

attrition_by_age

# Optional: visualize attrition rate by age group
ggplot(attrition_by_age, aes(x = AgeGroup, y = AttritionRate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Attrition Rate by Age Group", y = "Attrition Rate", x = "Age Group")

```
```{r}
#filter rows where attrition is NO and select the first 550 rows
attrition_Yes = df %>% 
  filter(Attrition == "Yes") %>%
  slice_head(n= 550)
```


```{r}
attrition_Yes
```
```{r}
# Create age groups
attrition_Yes <- attrition_Yes %>%
  mutate(AgeGroup = case_when(
    Age < 30 ~ "<30",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 ~ "50+"
  ))

# Summarize by age group
stay_by_age <- attrition_Yes%>%
  group_by(AgeGroup) %>%
  summarise(
    Count = n()
  )

stay_by_age

# Optional: visualize
ggplot(stay_by_age, aes(x = AgeGroup, y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Employees Likely to Stay by Age Group", y = "Number of Employees", x = "Age Group")
```
```{r}
dim(df)
```


```{r}
### Overtime vs Attiriton
l <- ggplot(df, aes(OverTime,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(df$Attrition) - 1 ,df$OverTime,mean)
```
The employees how work over time have more attrition

```{r}
```


```{r}
### MaritalStatus vs Attiriton
x<- ggplot(df, aes(MaritalStatus,fill = Attrition))
x <- x + geom_histogram(stat="count")
print(x)
tapply(as.numeric(df$Attrition) - 1 ,df$MaritalStatus,mean)
```


```{r}
# Attrition Distribution
# Count & percentage
table(df$Attrition)
prop.table(table(df$Attrition)) * 100

# Bar plot
library(ggplot2)
ggplot(df, aes(x = Attrition, fill = Attrition)) +
  geom_bar() +
  labs(title = "Attrition Count", y = "Number of Employees")

```


```{r}
# Age Distribution
ggplot(df, aes(x= Age, fill= Attrition))+
  geom_histogram(binwidth = 5 , position = "dodge")+
  labs(title = "Age Distribution by Attrition")
```


```{r}
# Job Satisfaction vs Attrition
ggplot(df, aes(x = factor(JobSatisfaction), fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Job Satisfaction vs Attrition", x = "Job Satisfaction (1-4)")

```
```{r}
# Department vs Atrrition
ggplot(df, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Attrition by Department")

```
```{r}
### x=Overtime, y= Age, z = MaritalStatus , t = Attrition
ggplot(df, aes(OverTime, Age)) +  
  facet_grid(.~MaritalStatus) +
  geom_jitter(aes(color = Attrition),alpha = 0.4) +  
  ggtitle("x=Overtime, y= Age, z = MaritalStatus , t = Attrition") +  
  theme_light()
```
LOGISTIC REGRESSION

```{r}
# Convert Attrition to binary (factor with 0/1)
df$Attrition <- ifelse(df$Attrition == "Yes", 1, 0)
df$Attrition <- as.factor(df$Attrition)

# Quick check
table(df$Attrition)

```
2. Split Train and Test


```{r}
set.seed(123)  # reproducibility

# 70% index for training
train_index <- sample(1:nrow(df), 0.7 * nrow(df))

# Split
train <- df[train_index, ]
test  <- df[-train_index, ]

```
Fit Logistic Regression on Training Data

```{r}
# Simple example with selected predictors
model <- glm(Attrition ~ Age + MonthlyIncome + DistanceFromHome + JobSatisfaction + OverTime,
             data = train,
             family = binomial)

summary(model)

```
Employee attrition at IBM is significantly influenced by age, monthly income, commute distance, job satisfaction, and overtime work.

Younger employees and lower paid staff are more likely to leave.

Longer commuting distances slightly increase the chance of leaving.

Higher job satisfaction strongly reduces attrition risk.

Working overtime is the strongest predictor  employees who work overtime are much more likely to quit.

```{r}
# Predict probabilities
```


```{r}
test$pred_prob <- predict(model, newdata = test, type = "response")

# Convert to classes (cutoff = 0.5)
test$pred_class <- ifelse(test$pred_prob > 0.5, 1, 0)
test$pred_class <- as.factor(test$pred_class)

```


```{r}
library(caret)
```


Model Evaluation
```{r}
# Confusion Matrix
confusionMatrix(test$pred_class, test$Attrition, positive = "1")

# Accuracy
mean(test$pred_class == test$Attrition)

```
Conclusion on IBM HR Attrition Data

Attrition Rate: Around 16% (237 out of 1470 employees) left the company, while 84% stayed.

Age Factor: Younger employees (<30) show the highest attrition rate (≈28%), while middle-aged employees (40–49) are more stable (≈9.7%).

Model Performance: Logistic regression achieved ~85% accuracy, but it struggles with sensitivity (13%), meaning it detects very few actual leavers.

HR Insight: Attrition is concentrated among younger, lower-income, and overtime-working employees. HR should focus on retention programs for these groups.




