# Research_Capstone_project
🍔 Uncovering How Social Media Marketing Tactics Drive Gen Z’s Fast Food Consumer Behavior This project analyzes how social media marketing strategies, impact Gen Z’s fast food purchasing behavior in the Gampaha District using statistical inference and R-based analysis.
---
title: "inferencial analysis"
author: "PS/2020/215 R N R Fonseka"
date: "2025-01-23"
output: html_document
---
```{r}
# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")

data

# Merge TikTok and Other into "Other" category
data$Most_Used_SM <- ifelse(data$Most_Used_SM %in% c("TikTok", "Other"), "Other", data$Most_Used_SM)

# Check the changes
table(data$Strategy,data$Most_Used_SM)

# Filter out rows where Strategy is "Other"
data_filtered <- data %>%
  filter(Strategy != "Other")

# Check the filtered data
table(data_filtered$Strategy, data_filtered$Most_Used_SM)


contingency_table <- table(data_filtered$Strategy, data_filtered$Most_Used_SM)

# Run the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

# Check the result
chi_square_test

# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v

# Load Required Library
library(nnet)

data <- subset(data, Strategy != "Other")  # Exclude rows where Strategy is "Other"



# Convert Variables to Factors (if not already)
data$Most_Used_SM <- as.factor(data$Most_Used_SM)
data$Strategy <- as.factor(data$Strategy)

# Fit Multinomial Logistic Regression Model
logit_model <- multinom(Strategy ~ Most_Used_SM, data = data)

# View Model Summary
summary(logit_model)

levels(data$Most_Used_SM)

data$Most_Used_SM <- relevel(data$Most_Used_SM, ref = "WhatsApp")  # Change reference to Instagram
logit_model <- multinom(Most_Used_SM ~ Strategy, data = data)
summary(logit_model)

# Load libraries
library(ggplot2)
library(ggmosaic)
library(ggpubr)
library(corrplot)
library(reshape2)
library(dplyr)

coef_df <- data.frame(
  Predictor = rownames(summary(logit_model)$coefficients),
  Estimate = summary(logit_model)$coefficients[, 1],  # Coefficients
  StdError = summary(logit_model)$standard.errors[, 1]  # Standard Errors
)

# Plot coefficients with error bars
ggplot(coef_df, aes(x = Predictor, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.96 * StdError, ymax = Estimate + 1.96 * StdError), width = 0.2) +
  labs(title = "Multinomial Logistic Regression Coefficients",
       x = "Predictor Variables", y = "Coefficient Estimate") +
  theme_minimal() +
  coord_flip()

```
