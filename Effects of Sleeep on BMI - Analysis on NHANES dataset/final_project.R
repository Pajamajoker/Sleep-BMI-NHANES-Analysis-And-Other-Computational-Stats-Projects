# Install necessary libraries if not already installed
if(!require(nhanesA)) install.packages("nhanesA")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(haven)) install.packages("haven")


# Load libraries
library(nhanesA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)

# Function to clean and merge datasets
# Function to clean and merge datasets
clean_data <- function(demo, slq, bmx, bpx) {
  # Merge the datasets by SEQN (Survey Participant ID)
  merged_data <- demo %>%
    left_join(slq, by = "SEQN") %>%
    left_join(bmx, by = "SEQN") %>%
    left_join(bpx, by = "SEQN")
  
  # Ensure the columns are present before filtering
  essential_columns <- c("BMXBMI", "BPXSY1", "BPXDI1", "SLD012", "SLQ030", "SLQ120")
  missing_columns <- setdiff(essential_columns, names(merged_data))
  
  if(length(missing_columns) > 0) {
    stop(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Clean the data: remove rows with missing essential values
  cleaned_data <- merged_data %>%
    filter(!is.na(BMXBMI), !is.na(BPXSY1), !is.na(BPXDI1), !is.na(SLD012), !is.na(SLQ030), !is.na(SLQ120)) %>%
    dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, SLD012, SLQ030, SLQ120, BMXBMI, BPXSY1, BPXDI1)
  
  return(cleaned_data)
}



# Function for descriptive statistics
descriptive_stats <- function(data) {
  stats <- data %>%
    summarise(
      mean_BMI = mean(BMXBMI, na.rm = TRUE),
      sd_BMI = sd(BMXBMI, na.rm = TRUE),
      mean_SBP = mean(BPXSY1, na.rm = TRUE),
      sd_SBP = sd(BPXSY1, na.rm = TRUE),
      mean_DBP = mean(BPXDI1, na.rm = TRUE),
      sd_DBP = sd(BPXDI1, na.rm = TRUE)
    )
  return(stats)
}

# Function for correlation analysis
correlation_analysis <- function(data) {
  # Select the specified columns
  selected_data <- data[, c("SLD012", "SLQ030", "SLQ120", "BMXBMI", "BPXSY1", "BPXDI1")]
  
  # Ensure the columns are numeric by converting them explicitly
  selected_data[] <- lapply(selected_data, as.numeric)
  
  # Compute the correlation matrix using complete observations
  cor_matrix <- cor(selected_data, use = "complete.obs")
  
  return(cor_matrix)
}

# Function for multiple linear regression
regression_analysis <- function(data) {
  model <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + RIDAGEYR + RIAGENDR + RIDRETH1, data = data)
  return(summary(model))
}

# Function for subgroup analysis
subgroup_analysis <- function(data) {
  model_subgroup <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + RIDAGEYR + RIAGENDR + RIDRETH1 + RIDRETH1:SLD012, data = data)
  return(summary(model_subgroup))
}


# Step 1: Load the datasets

#demo <- nhanes('DEMO_I')     # Demographic data
#slq <- nhanes('SLQ_I')       # Sleep data
#bmx <- nhanes('BMX_I')       # Body measurement (BMI)
#bpx <- nhanes('BPX_I')       # Blood pressure data

demo <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/DEMO_I.XPT")
slq <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/SLQ_I.XPT")
bmx <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/BMX_I.XPT")
bpx <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/BPX_I.XPT")


# Step 2: Clean and merge the data
cleaned_data <- clean_data(demo, slq, bmx, bpx)

# Step 3: Descriptive statistics for BMI and blood pressure
stats <- descriptive_stats(cleaned_data)
print("Descriptive Statistics for BMI and Blood Pressure:")
print(stats)

# Plot distributions of BMI, SBP, and DBP
ggplot(cleaned_data, aes(x = BMXBMI)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of BMI", x = "BMI", y = "Count") +
  theme_minimal()

ggplot(cleaned_data, aes(x = BPXSY1)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(title = "Distribution of Systolic Blood Pressure", x = "SBP", y = "Count") +
  theme_minimal()

ggplot(cleaned_data, aes(x = BPXDI1)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Diastolic Blood Pressure", x = "DBP", y = "Count") +
  theme_minimal()




# Step 4: Correlation analysis to check relationships between variables
cor_matrix <- correlation_analysis(cleaned_data)
print("Correlation Matrix:")
print(cor_matrix)

# Plot heatmap of correlation matrix
library(ggcorrplot)
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE,
           title = "Correlation Matrix of Sleep and Health Variables")




# Step 5: Multiple linear regression to see the effect of sleep patterns on BMI
regression_result <- regression_analysis(cleaned_data)
print("Multiple Linear Regression Analysis for BMI:")
print(regression_result)

regression_model <- lm(BMXBMI ~ SLD012 + BPXSY1 + BPXDI1 + RIAGENDR + RIDRETH1, data = cleaned_data)
# Use the regression model to predict BMI
predicted_BMI <- predict(regression_model, cleaned_data)
ggplot(cleaned_data, aes(x = predicted_BMI, y = BMXBMI)) +
  geom_point(aes(color = as.factor(RIAGENDR))) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual BMI", x = "Predicted BMI", y = "Actual BMI") +
  theme_minimal()


# Plot Predicted vs. Actual BMI
#predicted_BMI <- predict(regression_result, cleaned_data)
#ggplot(cleaned_data, aes(x = predicted_BMI, y = BMXBMI)) +
#  geom_point(aes(color = as.factor(RIAGENDR))) +
#  geom_abline(intercept = 0, slope = 1, color = "red") +
#  labs(title = "Predicted vs Actual BMI", x = "Predicted BMI", y = "Actual BMI") +
#  theme_minimal()



# Step 6: Subgroup analysis to see how demographic factors influence the relationship
subgroup_result <- subgroup_analysis(cleaned_data)
print("Subgroup Analysis with Interaction Term between Ethnicity and Sleep Duration:")
print(subgroup_result)

# Plot interaction effect of ethnicity and sleep duration on BMI
ggplot(cleaned_data, aes(x = SLD012, y = BMXBMI, color = as.factor(RIDRETH1))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Interaction Effect of Ethnicity and Sleep Duration on BMI", 
       x = "Sleep Duration (Hours)", y = "BMI", color = "Ethnicity") +
  theme_minimal()


# Scatter plot of sleep duration vs BMI
ggplot(cleaned_data, aes(x = SLD012, y = BMXBMI)) +
  geom_point(aes(color = as.factor(RIAGENDR))) + 
  labs(title = "Relationship between Sleep Duration and BMI",
       x = "Sleep Duration (Hours)",
       y = "BMI",
       color = "Gender") +
  theme_minimal()

# Optional: Create a scatter plot to visualize the relationship between sleep duration and BMI
ggplot(cleaned_data, aes(x = SLD012, y = BMXBMI)) +
  geom_point(aes(color = as.factor(RIAGENDR))) + 
  labs(title = "Relationship between Sleep Duration and BMI",
       x = "Sleep Duration (Hours)",
       y = "BMI",
       color = "Gender") +
  theme_minimal()
