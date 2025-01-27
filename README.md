# Research_Capstone_project
🍔 Uncovering How Social Media Marketing Tactics Drive Gen Z’s Fast Food Consumer Behavior This project analyzes how social media marketing strategies, impact Gen Z’s fast food purchasing behavior in the Gampaha District according to our sample using statistical inference and R-based analysis.

---
title: "inferential analysis"
author: "R N R Fonseka PS/2020/215 "
date: "2025-01-23"
output: html_document
---
```{r}
#Strategy VS Platform

library(dplyr)

# Read the dataset
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")

print(data)

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

library(nnet)

data <- subset(data, Strategy != "Other")  # Exclude rows where Strategy is "Other"

# Convert Variables to Factors 
data$Most_Used_SM <- as.factor(data$Most_Used_SM)
data$Strategy <- as.factor(data$Strategy)

# Fit Multinomial Logistic Regression Model
logit_model <- multinom(Strategy ~ Most_Used_SM, data = data)

# View Model Summary
summary(logit_model)

levels(data$Most_Used_SM)

data$Most_Used_SM <- relevel(data$Most_Used_SM, ref = "WhatsApp")  # Change reference 
logit_model <- multinom(Most_Used_SM ~ Strategy, data = data)
summary(logit_model)


library(ggplot2)

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

```{r}
#Daily SM time VS  Platform, Frequency of Usage, Marital Status, State Trust  

library(dplyr)

# Read the dataset
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")

data


# Check the changes
table(data$Daily_Spend_SM, data$Most_Used_SM)

contingency_table <- table(data_filtered$Daily_Spend_SM, data_filtered$Most_Used_SM)

contingency_table_modified <- contingency_table[-2, ]  # Removes the second row which contains majority of zero values

contingency_table_modified

# Run the Chi-Square test
chi_square_test <- chisq.test(contingency_table_modified)

# Print the result
chi_square_test

# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v



# Check the changes
table(data$Daily_Spend_SM, data$Frequency_FFA)

contingency_table <- table(data_filtered$Daily_Spend_SM, data_filtered$Frequency_FFA)

contingency_table_modified <- contingency_table[-2, ]  # Removes the second row which contains majority of zero values 
contingency_table_modified

# Run the Chi-Square test
chi_square_test <- chisq.test(contingency_table_modified)

# Print the result
chi_square_test


# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v


# Check the changes
table(data$Daily_Spend_SM, data$Marital_Status)

contingency_table <- table(data_filtered$Daily_Spend_SM, data_filtered$Marital_Status)

contingency_table_modified <- contingency_table[-2, ]  # Removes the second row which contains majority of zero values
contingency_table_modified

# Run the Chi-Square test
chi_square_test <- chisq.test(contingency_table_modified)

# Print the result
chi_square_test


# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v


table(data$Daily_Spend_SM, data$State_Trust)

contingency_table <- table(data_filtered$Daily_Spend_SM, data_filtered$State_Trust)

contingency_table_modified <- contingency_table[-2, ]  # Removes the second row which contains majority of zero values
contingency_table_modified

# Run the Chi-Square test
chi_square_test <- chisq.test(contingency_table_modified)

# Print the result
chi_square_test


# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v

# Convert Variables to Factors (if not already)
data$Most_Used_SM <- as.factor(data$Most_Used_SM)
data$Daily_Spend_SM <- as.factor(data$Daily_Spend_SM)
data$Frequency_FFA <- as.factor(data$Frequency_FFA)
data$Marital_Status <- as.factor(data$Marital_Status)
data$State_Trust <- as.factor(data$State_Trust)

# Fit Multinomial Logistic Regression Model
logit_model <- multinom(Daily_Spend_SM ~ Most_Used_SM + Frequency_FFA + Marital_Status + State_Trust,  data = data)

# View Model Summary
summary(logit_model)

# Load libraries
library(ggplot2)

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
```{r}
#Platform VS Marketing Strategies, Gender, Marital status, State of Trust

library(dplyr)

# Read the dataset
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")

data

# Merge TikTok and Other into "Other" category
data$Most_Used_SM <- ifelse(data$Most_Used_SM %in% c("TikTok", "Other"), "Other", data$Most_Used_SM)

# Check the filtered data
table(data_filtered$Most_Used_SM, data_filtered$Marital_Status)


contingency_table <- table(data_filtered$Most_Used_SM, data_filtered$Marital_Status)

# Run the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

# Check the result
chi_square_test

# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v


# Check the filtered data
table(data_filtered$Most_Used_SM, data_filtered$Gender)


contingency_table <- table(data_filtered$Most_Used_SM, data_filtered$Gender)

# Run the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

# Check the result
chi_square_test

# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v


# Check the filtered data
table(data_filtered$Most_Used_SM, data_filtered$Strategy)


contingency_table <- table(data_filtered$Most_Used_SM, data_filtered$Strategy)

# Run the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

# Check the result
chi_square_test

# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v


# Check the filtered data
table(data_filtered$Most_Used_SM, data_filtered$State_Trust)


contingency_table <- table(data_filtered$Most_Used_SM, data_filtered$State_Trust)

# Run the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

# Check the result
chi_square_test

# Compute Cramér's V
cramers_v <- sqrt(chi_square_test$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))

# Display Results
cramers_v


# Convert Variables to Factors (if not already)
data$Most_Used_SM <- as.factor(data$Most_Used_SM)
data$Daily_Spend_SM <- as.factor(data$Strategy)
data$Frequency_FFA <- as.factor(data$Gender)
data$Marital_Status <- as.factor(data$Marital_Status)
data$State_Trust <- as.factor(data$State_Trust)

# Fit Multinomial Logistic Regression Model
logit_model <- multinom(Most_Used_SM ~ Strategy + Marital_Status + Gender + State_Trust ,  data = data)

# View Model Summary
summary(logit_model)

#install.packages(c("ggplot2", "ggmosaic", "ggpubr", "corrplot", "reshape2", "dplyr"))

# Load libraries
library(ggplot2)

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

---
title: "Project Analysis K mode"
author: "PS/2020/215 R N R Fonseka"
date: "2025-01-23"
output: html_document
---

```{r}

library(klaR)
library(dplyr)
library(ggplot2)

#load data 
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")  # Replace with actual file name

# Convert Categorical Columns to Factors
categorical_columns <- c("Marital_Status", "Gender", "Daily_Spend_SM", "Most_Used_SM", "Frequency_FFA", 
                         "Strategy", "Type_TA", "Type_IR", "Type_CR", "Type_LTO", 
                         "Influence", "State_Trust", "Age_Requirement", "Residence_Requirement")

data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Assign Strategy Type Based on Responses
data <- data %>%
  mutate(Strategy_Type = case_when(
    Strategy == "Targeted Ads" ~ "Targeted Ads",
    Strategy == "Influencer Recommendation" ~ "Influencer Recommendation",
    Strategy == "Customer Reviews" ~ "Customer Reviews",
    Strategy == "Limited-Time Offers or Promotions" ~ "Limited-Time Offers or Promotions",
    TRUE ~ "Other"
  ))

# Assign Clusters Based on Strategy Type
data$Cluster <- case_when(
  data$Strategy_Type == "Targeted Ads" ~ 1,         # Cluster 1: Targeted Ads
  data$Strategy_Type == "Influencer Recommendation" ~ 2,  # Cluster 2: Influencer Recommendation
  data$Strategy_Type == "Customer Reviews" ~ 3,        # Cluster 3: Customer Reviews
  data$Strategy_Type == "Limited-Time Offers or Promotions" ~ 4,  # Cluster 4: Limited-Time Offers
  TRUE ~ 5                                              # Cluster 5: Other
)

# Select relevant categorical columns for clustering (excluding Strategy_Type and Cluster)
clustering_columns <- c("Most_Used_SM", "Gender", "Daily_Spend_SM", "Frequency_FFA", 
                        "Influence", "State_Trust", "Marital_Status","Age_Requirement", "Residence_Requirement")

# Apply K-Modes clustering for refinement (you can choose the appropriate 'k' after running the Elbow method or other criteria)
set.seed(123)
kmodes_model <- kmodes(data[clustering_columns], 4, iter.max = 10)

# Assign refined cluster labels to data
data$Refined_Cluster <- kmodes_model$cluster

# Summary of clusters
cluster_summary <- data %>%
  group_by(Cluster, Strategy_Type) %>%
  summarise(
    Count = n(),
    Most_Common_SM = names(sort(table(Most_Used_SM), decreasing = TRUE))[1],

  )

# View Cluster Summary
print(cluster_summary)

# Visual Representation (Bar Plot for Strategy Type Distribution by Cluster)
ggplot(data, aes(x = as.factor(Cluster), fill = Strategy_Type)) +
  geom_bar(position = "stack") +
  labs(title = "Cluster Distribution by Strategy Type", x = "Cluster", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Visual Representation (Bar Plot for Gender Distribution in Clusters)
ggplot(data, aes(x = as.factor(Cluster), fill = Gender)) +
  geom_bar(position = "stack") +
  labs(title = "Gender Distribution by Cluster", x = "Cluster", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Visual Representation (Boxplot for Daily Spend by Cluster)
ggplot(data, aes(x = as.factor(Cluster), y = as.numeric(Daily_Spend_SM), fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Boxplot for Daily Spend by Cluster", x = "Cluster", y = "Daily Spend") +
  theme_minimal()


```

```{r}
library(klaR)
library(dplyr)
library(ggplot2)

# Load Your Dataset (replace with actual file)
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")  # Replace with actual file name

# Convert Categorical Columns to Factors
categorical_columns <- c("Marital_Status", "Gender", "Daily_Spend_SM", "Most_Used_SM", "Frequency_FFA", 
                         "Strategy", "Type_TA", "Type_IR", "Type_CR", "Type_LTO", 
                         "Influence", "State_Trust", "Age_Requirement", "Residence_Requirement")

data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Assign Strategy Type Based on Responses
data <- data %>%
  mutate(Platform_Type = case_when(
    Most_Used_SM == "Facebook" ~ "Facebook",   
    Most_Used_SM == "Instagram" ~ "Instagram", 
    Most_Used_SM == "WhatsApp" ~ "WhatsApp",  
    Most_Used_SM == "YouTube" ~ "YouTube",    
    Most_Used_SM == "TikTok" ~ "TikTok",     
    TRUE ~ "Other"                          
  ))

# Assign Clusters Based on Strategy Type
data$Cluster <- case_when(
  data$Platform_Type == "Facebook" ~ 1,     # Cluster 1: Facebook
  data$Platform_Type == "Instagram" ~ 2,    # Cluster 2: Instagram
  data$Platform_Type == "WhatsApp" ~ 3,     # Cluster 3: WhatsApp
  data$Platform_Type == "YouTube" ~ 4,      # Cluster 4: YouTube
  data$Platform_Type == "TikTok" ~ 5,       # Cluster 5: TikTok
  TRUE ~ 6                                  # Cluster 6: Other            
)

# Select relevant categorical columns for clustering (excluding Strategy_Type and Cluster)
clustering_columns <- c("Strategy", "Gender", "Daily_Spend_SM", "Frequency_FFA", 
                        "Influence", "State_Trust", "Marital_Status","Age_Requirement", "Residence_Requirement")

# Apply K-Modes clustering for refinement (you can choose the appropriate 'k' after running the Elbow method or other criteria)
set.seed(123)
kmodes_model <- kmodes(data[clustering_columns], 5, iter.max = 10)

# Assign refined cluster labels to data
data$Refined_Cluster <- kmodes_model$cluster

# Summary of clusters
cluster_summary <- data %>%
  group_by(Cluster, Platform_Type) %>%
  summarise(
    Count = n(),
    Most_Common_Strategy = names(sort(table(Strategy), decreasing = TRUE))[1],
    Dominant_Gender = names(sort(table(Gender), decreasing = TRUE))[1],
    Dominant_State_Trust = names(sort(table(State_Trust), decreasing = TRUE))[1],
    Dominant_Marital_Status = names(sort(table(Marital_Status), decreasing = TRUE))[1]
  )

# View Cluster Summary
print(cluster_summary)

# Visual Representation (Bar Plot for Strategy Type Distribution by Cluster)
ggplot(data, aes(x = as.factor(Cluster), fill = Platform_Type)) +
  geom_bar(position = "stack") +
  labs(title = "Cluster Distribution by Strategy Type", x = "Cluster", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Visual Representation (Bar Plot for Gender Distribution in Clusters)
ggplot(data, aes(x = as.factor(Cluster), fill = Gender)) +
  geom_bar(position = "stack") +
  labs(title = "Gender Distribution by Cluster", x = "Cluster", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")




```
```{r}
library(klaR)
library(dplyr)
library(ggplot2)

# Load Your Dataset (replace with actual file)
data <- read.csv("C:/Users/HP/Desktop/kaleniya/3rd year/SEM 2/CAPSTONE PROJECT/Data_Research.csv")  # Replace with actual file name

# Convert Categorical Columns to Factors
categorical_columns <- c("Marital_Status", "Gender", "Daily_Spend_SM", "Most_Used_SM", "Frequency_FFA", 
                         "Strategy", "Type_TA", "Type_IR", "Type_CR", "Type_LTO", 
                         "Influence", "State_Trust", "Age_Requirement", "Residence_Requirement")

data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Assign Strategy Type Based on Responses
data <- data %>%
  mutate(Daily_Spend_SM_Type = case_when(
    Daily_Spend_SM == "1 - 3 hours" ~ "1 - 3 hours",
    Daily_Spend_SM == "4 - 6 hours" ~ "4 - 6 hours",
    Daily_Spend_SM == "Less than 1 hour" ~ "Less than 1 hour",
    Daily_Spend_SM == "More than 6 hours" ~ "More than 6 hours",
    TRUE ~ "Other"
  ))

# Assign Clusters Based on Strategy Type
data$Cluster <- case_when(
  data$Daily_Spend_SM_Type == "1 - 3 hours" ~ 1,        # Cluster 1: 1 - 3 hours
  data$Daily_Spend_SM_Type == "4 - 6 hours" ~ 2,        # Cluster 2: 4 - 6 hours
  data$Daily_Spend_SM_Type == "Less than 1 hour" ~ 3,   # Cluster 3: Less than 1 hour     
  data$Daily_Spend_SM_Type == "More than 6 hours" ~ 4,  # Cluster 4: More than 6 hours
  TRUE ~ 5                                              # Cluster 5: Other
)

# Select relevant categorical columns for clustering (excluding Strategy_Type and Cluster)
clustering_columns <- c("Strategy", "Gender", "Most_Used_SM", "Frequency_FFA", 
                        "Influence", "State_Trust", "Marital_Status","Age_Requirement", "Residence_Requirement")

# Apply K-Modes clustering for refinement (you can choose the appropriate 'k' after running the Elbow method or other criteria)
set.seed(123)
kmodes_model <- kmodes(data[clustering_columns], 4, iter.max = 10)

# Assign refined cluster labels to data
data$Refined_Cluster <- kmodes_model$cluster

# Summary of clusters
cluster_summary <- data %>%
  group_by(Cluster, Daily_Spend_SM_Type) %>%
  summarise(
    Count = n(),
    Most_Common_Used_SM = names(sort(table(Most_Used_SM), decreasing = TRUE))[1],
    Dominant_State_Trust = names(sort(table(State_Trust), decreasing = TRUE))[1],
    Dominant_Marital_Status = names(sort(table(Marital_Status), decreasing = TRUE))[1]
  )

# View Cluster Summary
print(cluster_summary)



```

