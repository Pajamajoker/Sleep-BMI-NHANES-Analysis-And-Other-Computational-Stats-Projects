# =============================================
# Load Necessary Libraries
# =============================================
# Load all the required libraries. Install them if not present.
if (!require("NHANES")) install.packages("NHANES")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("broom")) install.packages("broom")
library(NHANES)
library(dplyr)
library(ggplot2)
library(broom)

# =============================================
# Function Definitions: Logic Section
# =============================================

# 1 function to prepare the dataset.
# Select specific columns, recode categorical variables as factors, and calculate BMI.
prepare_data <- function(selected_columns = c("Age", "Gender", "Height", "Weight", 
                                              "BPSysAve", "BPDiaAve", "Diabetes", "PhysActive", "BMI")) {
  print("Preparing the data...")
  NHANES <- NHANES[!duplicated(NHANES$ID), ]
  
  df <- NHANES %>%
    select(all_of(selected_columns)) %>%
    rename_with(~ c("Age", "Sex", "Height", "Weight", "SBP", "DBP", "Diabetes", "PhysicalActivity", "BMI")) %>%
    mutate(across(c(Sex, Diabetes, PhysicalActivity), as.factor)) %>%
    na.omit() # Remove rows with NA values.
  
  print("data preparation completed!")
  return(df)
}

# 2. function to compute descriptive statistics.
# This calculates summary statistics like mean and SD for numeric variables.
compute_summary <- function(data) {
  print("Computing descriptive statistics now...")
  summary <- data %>%
    summarise(
      across(where(is.numeric), 
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
             .names = "{col}_{fn}")
    )
  cat("Summary statistics computed!!", "\n")
  print(summary)
  return(summary)
}

# 3 function to fit multiple regression model.
# Fits a linear model with interaction terms for predictors of SBP.
fit_model <- function(data) {
  print("Fitting multiple regression model with interaction terms...")
  model <- lm(SBP ~ BMI * Age, data = data) # Model includes interaction term.
  print("Model fitting completed!!")
  return(model)
}

# 4) Function to generate diagnostic plots for the model.
# This creates residual vs fitted and Q-Q plots for diagnostics.
diagnostic_plots <- function(model) {
  print("Creating diagnostic plots noew...")
  
  # Residuals vs Fitted
  res_vs_fitted <- ggplot(data = augment(model), aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "residuals vs fitted", x = "fitted Values", y = "residuals") +
    theme_minimal()
  
  # Q-Q Plot
  qq_plot <- ggplot(data = augment(model), aes(sample = .std.resid)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Q-Q Plot of Residuals") +
    theme_minimal()
  
  print("Diagnostic plots created!")
  return(list(res_vs_fitted = res_vs_fitted, qq_plot = qq_plot))
}

# 5) Function to interpret model results.
# This gives a summary of the model coefficients and p-values.
interpret_model <- function(model) {
  print("Interpreting the model results...")
  tidy_model <- tidy(model)
  print("Model coefficients and p-values:")
  print(tidy_model)
  return(tidy_model)
}

# =============================================
# Execution Section: Task List in Sequence
# =============================================

# Step 1: Prepare the dataset.
df <- prepare_data()
str(df)  # Display the structure of the cleaned dataset to verify.

# Step 2: Compute descriptive statistics for the full dataset (Table 1).
table1 <- compute_summary(df)
print("Table 1: Descriptive Statistics")
print(table1)

# Step 3: Fit a multiple regression model to predict SBP using BMI and Age.
model <- fit_model(df)

# Step 4: Generate diagnostic plots for the regression model.
diagnostics <- diagnostic_plots(model)
print("Displaying Diagnostic Plots:")
print(diagnostics$res_vs_fitted)
print(diagnostics$qq_plot)

# Step 5: Interpret the results of the regression model.
model_results <- interpret_model(model)

# =============================================
# Conclusion Section
# =============================================
# In this project, we analyzed the predictors of systolic blood pressure using a 
# multiple regression model that included interaction terms for BMI and age.
# Descriptive statistics were computed to summarize the dataset.
# Diagnostic plots indicated whether assumptions of linearity and normality were valid.
# The regression results showed the adjusted and unadjusted effects of BMI and age on SBP,
# providing insights into their complex association.
