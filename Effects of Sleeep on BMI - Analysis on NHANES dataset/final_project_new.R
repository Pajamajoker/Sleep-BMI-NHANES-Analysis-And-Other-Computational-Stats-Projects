if (!require(nhanesA)) install.packages("nhanesA")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(lmtest)) install.packages("lmtest")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(car)) install.packages("car")
if (!require(ggcorrplot)) install.packages("ggcorrplot")
if (!require(haven)) install.packages("haven") # For reading XPT files

# Load libraries
library(nhanesA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(haven)
library(lmtest)
library(car)  # For vif()
library(gridExtra)

# Function to clean and merge datasets
clean_data <- function(demo, slq, bmx) {
  # Merge the datasets by SEQN (Survey Participant ID)
  merged_data <- demo %>%
    left_join(slq, by = "SEQN") %>%
    left_join(bmx, by = "SEQN")
  
  # Ensure the columns are present before filtering
  essential_columns <- c("BMXBMI", "SLD012", "SLQ030", "SLQ120", "SLQ310", "SLQ300")
  missing_columns <- setdiff(essential_columns, names(merged_data))
  
  if(length(missing_columns) > 0) {
    stop(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Clean the data: remove rows with missing essential values
  cleaned_data <- merged_data %>%
    filter(!is.na(BMXBMI), !is.na(SLD012), !is.na(SLQ030), !is.na(SLQ120), !is.na(SLQ310), !is.na(SLQ300)) %>%
    dplyr::select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, SLD012, SLQ030, SLQ120, SLQ310, SLQ300, BMXBMI) %>%
    # Convert categorical variables to factors
    mutate(
      RIAGENDR = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
      RIDRETH1 = factor(RIDRETH1, levels = c(1, 2, 3, 4, 5), 
                        labels = c("Non-Hispanic White", "Non-Hispanic Black", "Mexican American", 
                                   "Other Hispanic", "Other Race")),
      SLD012 = as.numeric(SLD012),  # Keep SLD012 as numeric for sleep hours
      
      # SLQ030 - How often do you snore?
      SLQ030 = factor(SLQ030, levels = c(0, 1, 2, 3, 7, 9), labels = c("Never", "Rarely - 1-2 nights a week", 
                                                                       "Occasionally - 3-4 nights a week", 
                                                                       "Frequently - 5 or more nights a week", 
                                                                       "Refused", "Don't know")),
      
      # SLQ120 - How often feel overly sleepy during day?
      SLQ120 = factor(SLQ120, levels = c(0, 1, 2, 3, 4, 7, 9), labels = c("Never", "Rarely - 1 time a month", 
                                                                          "Sometimes - 2-4 times a month", 
                                                                          "Often - 5-15 times a month", 
                                                                          "Almost always - 16-30 times a month", 
                                                                          "Refused", "Don't know")),
      
      SLQ310 = as.character(SLQ310),  # Treat SLQ310 as a character variable ('HH:MM')
      SLQ300 = as.character(SLQ300),  # Treat SLQ300 as a character variable ('HH:MM')
      
      # Validate time format for SLQ310 and SLQ300
      SLQ310 = ifelse(grepl("^\\d{2}:\\d{2}$", SLQ310), SLQ310, NA),
      SLQ300 = ifelse(grepl("^\\d{2}:\\d{2}$", SLQ300), SLQ300, NA),
      
      # Assign sleep categories based on SLQ300 (sleep time) and SLQ310 (wake time)
      SLQ300_category = factor(
        case_when(
          as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) >= 19 * 60 & as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) < 21 * 60 ~ "SLEPT EARLY",
          as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) >= 21 * 60 & as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) < 23 * 60 ~ "SLEPT ON TIME",
          as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) >= 23 * 60 | as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) < 1 * 60 ~ "SLEPT LATE",  # Between 23:00 and 01:00
          as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) >= 1 * 60 & as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) < 3 * 60 ~ "SLEPT VERY LATE",
          as.numeric(substr(SLQ300, 1, 2)) * 60 + as.numeric(substr(SLQ300, 4, 5)) >= 3 * 60 ~ "SLEPT AT OTHER TIME",
          TRUE ~ "NA"
        ),
        levels = c("SLEPT EARLY", "SLEPT ON TIME", "SLEPT LATE", "SLEPT VERY LATE", "SLEPT AT OTHER TIME")
      ),
      
      SLQ310_category = factor(
        case_when(
          as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) >= 4 * 60 & as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) < 6 * 60 ~ "WOKE UP EARLY",
          as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) >= 6 * 60 & as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) < 8 * 60 ~ "WOKE UP ON TIME",
          as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) >= 8 * 60 & as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) < 10 * 60 ~ "WOKE UP LATE",
          as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) >= 10 * 60 & as.numeric(substr(SLQ310, 1, 2)) * 60 + as.numeric(substr(SLQ310, 4, 5)) < 12 * 60 ~ "WOKE UP VERY LATE",
          TRUE ~ "WOKE UP AT OTHER TIMES"
        ),
        levels = c("WOKE UP EARLY", "WOKE UP ON TIME", "WOKE UP LATE", "WOKE UP VERY LATE", "WOKE UP AT OTHER TIMES")
      )
    ) %>%
    
    # Optional: Additional cleaning - Remove rows with missing 'BMXBMI' values
    filter(!is.na(BMXBMI))
  
  return(cleaned_data)
}

# Function to calculate descriptive statistics for cleaned data
descriptive_stats <- function(cleaned_data) {
  # Descriptive statistics for numerical variables
  numeric_vars <- cleaned_data %>%
    select(where(is.numeric))
  
  numeric_stats <- numeric_vars %>%
    summarise(across(everything(), list(
      mean = ~mean(. , na.rm = TRUE),
      sd = ~sd(. , na.rm = TRUE),
      min = ~min(. , na.rm = TRUE),
      max = ~max(. , na.rm = TRUE)
    )))
  
  # Descriptive statistics for categorical variables
  categorical_vars <- cleaned_data %>%
    select(where(is.factor) | where(is.character))
  
  categorical_stats <- categorical_vars %>%
    summarise(across(everything(), ~{
      counts <- table(.)
      paste(names(counts), counts, sep = ": ", collapse = ", ")
    }))
  
  # Combine both numeric and categorical stats
  stats <- list(
    numeric = numeric_stats,
    categorical = categorical_stats
  )
  
  return(stats)
}

# Correlation Analysis Function
correlation_analysis <- function(cleaned_data) {
  # Select only numerical columns for correlation analysis (excluding SEQN)
  numeric_vars <- cleaned_data %>%
    select(where(is.numeric)) %>%
    select(-SEQN)  # Exclude SEQN from correlation analysis
  
  # Compute the correlation matrix
  correlation_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
  
  # Return the correlation matrix
  return(correlation_matrix)
}

# Function for multiple linear regression
regression_analysis <- function(cleaned_data) {
  
  # Create the regression model
  model <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + RIDAGEYR + RIAGENDR, data = cleaned_data)
  
  # Summary of the model
  model_summary <- summary(model)
  
  # Display the model summary
  print(model_summary)
  
  # Step 2: Visualize Residuals
  # 1) Residuals vs Fitted plot
  residuals_plot <- ggplot(data = data.frame(fitted = model$fitted.values, residuals = model$residuals), 
                           aes(x = fitted, y = residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smooth line
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  print(residuals_plot)  # Print residuals plot
  
  # 2) Q-Q Plot of residuals
  qq_plot <- ggplot(data = data.frame(residuals = model$residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Q-Q Plot of Residuals") +
    theme_minimal()
  
  print(qq_plot)  # Print Q-Q plot
  
  # 3) Fitted Values vs sqrt(Standardized Residuals)
  standardized_residuals <- rstandard(model)
  sqrt_standardized_residuals <- sqrt(abs(standardized_residuals))
  
  fitted_vs_sqrt_residuals_plot <- ggplot(data = data.frame(fitted = model$fitted.values, sqrt_residuals = sqrt_standardized_residuals), 
                                          aes(x = fitted, y = sqrt_residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smooth line
    labs(title = "Fitted Values vs Sqrt(Standardized Residuals)", x = "Fitted Values", y = "Sqrt(Standardized Residuals)") +
    theme_minimal()
  
  print(fitted_vs_sqrt_residuals_plot)  # Print the plot
  
  # 4) Leverage vs Standardized Residuals plot
  leverage_values <- hatvalues(model)
  leverage_vs_residuals_plot <- ggplot(data = data.frame(leverage = leverage_values, standardized_residuals = standardized_residuals), 
                                       aes(x = leverage, y = standardized_residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smooth line
    labs(title = "Leverage vs Standardized Residuals", x = "Leverage", y = "Standardized Residuals") +
    theme_minimal()
  
  print(leverage_vs_residuals_plot)  # Print leverage vs residuals plot
  
  # Step 3: Check for Multicollinearity (Variance Inflation Factor)
  vif_values <- vif(model)
  
  # Display VIF values
  print("Variance Inflation Factor (VIF) values:")
  print(vif_values)
  
  # Return model summary and diagnostics
  return(list(
    model_summary = model_summary,
    residuals_plot = residuals_plot,
    qq_plot = qq_plot,
    fitted_vs_sqrt_residuals_plot = fitted_vs_sqrt_residuals_plot,
    leverage_vs_residuals_plot = leverage_vs_residuals_plot,
    vif_values = vif_values
  ))
}

# Function for advanced intereaction effect analysis
advanced_regression_analysis <- function(cleaned_data) {
  
  # Create the regression model with interaction effects
  model_interaction <- lm(BMXBMI ~ SLD012 * SLQ030 * SLQ120 * RIDAGEYR * RIAGENDR, data = cleaned_data)
  
  # Summary of the model
  model_summary <- summary(model_interaction)
  
  # Display the model summary
  print(model_summary)
  
  # Step 2: Visualize Residuals
  # 1) Residuals vs Fitted plot
  residuals_plot <- ggplot(data = data.frame(fitted = model_interaction$fitted.values, residuals = model_interaction$residuals), 
                           aes(x = fitted, y = residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smooth line
    labs(title = "Residuals vs Fitted (Interaction Model)", x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  print(residuals_plot)  # Print residuals plot
  
  # 2) Q-Q Plot of residuals
  qq_plot <- ggplot(data = data.frame(residuals = model_interaction$residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Q-Q Plot of Residuals (Interaction Model)") +
    theme_minimal()
  
  print(qq_plot)  # Print Q-Q plot
  
  # 3) Fitted Values vs sqrt(Standardized Residuals)
  standardized_residuals <- rstandard(model_interaction)
  sqrt_standardized_residuals <- sqrt(abs(standardized_residuals))
  
  fitted_vs_sqrt_residuals_plot <- ggplot(data = data.frame(fitted = model_interaction$fitted.values, sqrt_residuals = sqrt_standardized_residuals), 
                                          aes(x = fitted, y = sqrt_residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smooth line
    labs(title = "Fitted Values vs Sqrt(Standardized Residuals) (Interaction Model)", 
         x = "Fitted Values", y = "Sqrt(Standardized Residuals)") +
    theme_minimal()
  
  print(fitted_vs_sqrt_residuals_plot)  # Print the plot
  
  # 4) Leverage vs Standardized Residuals plot
  leverage_values <- hatvalues(model_interaction)
  leverage_vs_residuals_plot <- ggplot(data = data.frame(leverage = leverage_values, standardized_residuals = standardized_residuals), 
                                       aes(x = leverage, y = standardized_residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess smooth line
    labs(title = "Leverage vs Standardized Residuals (Interaction Model)", 
         x = "Leverage", y = "Standardized Residuals") +
    theme_minimal()
  
  print(leverage_vs_residuals_plot)  # Print leverage vs residuals plot
  
  # Return model summary and diagnostics
  return(list(
    model_summary = model_summary,
    residuals_plot = residuals_plot,
    qq_plot = qq_plot,
    fitted_vs_sqrt_residuals_plot = fitted_vs_sqrt_residuals_plot,
    leverage_vs_residuals_plot = leverage_vs_residuals_plot
  ))
}

# Function for subgroup analysis
subgroup_analysis <- function(cleaned_data) {
  # Create Age Group categories
  cleaned_data$age_group <- cut(cleaned_data$RIDAGEYR,
                                breaks = c(18, 29, 44, 59, Inf),
                                labels = c("18-29", "30-44", "45-59", "60+"),
                                right = FALSE)
  
  # Create Sleep Duration categories
  cleaned_data$sl_sleep_group <- cut(cleaned_data$SLD012, 
                                     breaks = c(0, 5, 7, 9, Inf), 
                                     labels = c("<5 hours", "5-7 hours", "7-9 hours", ">9 hours"),
                                     right = FALSE)
  
  # Run Regression by Age Group
  age_group_regression <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + SLQ300_category + SLQ310_category + age_group, data = cleaned_data)
  age_group_summary <- summary(age_group_regression)
  
  # Run Regression by Gender
  gender_regression <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + SLQ300_category + SLQ310_category + RIAGENDR, data = cleaned_data)
  gender_summary <- summary(gender_regression)
  
  # Run Regression by Ethnicity
  ethnicity_regression <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + SLQ300_category + SLQ310_category + RIDRETH1, data = cleaned_data)
  ethnicity_summary <- summary(ethnicity_regression)
  
  # Run Regression by Sleep Duration Group
  sleep_duration_regression <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + SLQ300_category + SLQ310_category + sl_sleep_group, data = cleaned_data)
  sleep_duration_summary <- summary(sleep_duration_regression)
  
  # Run Regression by Snoring Frequency
  snoring_regression <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + SLQ300_category + SLQ310_category + SLQ030, data = cleaned_data)
  snoring_summary <- summary(snoring_regression)
  
  # Run Regression by Daytime Sleepiness
  sleepiness_regression <- lm(BMXBMI ~ SLD012 + SLQ030 + SLQ120 + SLQ300_category + SLQ310_category + SLQ120, data = cleaned_data)
  sleepiness_summary <- summary(sleepiness_regression)
  
  # Collect all summaries
  regression_summaries <- list(
    age_group = age_group_summary,
    gender = gender_summary,
    ethnicity = ethnicity_summary,
    sleep_duration = sleep_duration_summary,
    snoring = snoring_summary,
    sleepiness = sleepiness_summary
  )
  
  # Generate Plots for each subgroup
  plot_age_group <- ggplot(cleaned_data, aes(x = age_group, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Age Group")
  
  plot_gender <- ggplot(cleaned_data, aes(x = RIAGENDR, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Gender")
  
  plot_ethnicity <- ggplot(cleaned_data, aes(x = RIDRETH1, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Ethnicity")
  
  plot_sleep_duration <- ggplot(cleaned_data, aes(x = sl_sleep_group, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Sleep Duration")
  
  plot_snoring <- ggplot(cleaned_data, aes(x = SLQ030, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Snoring Frequency")
  
  plot_sleepiness <- ggplot(cleaned_data, aes(x = SLQ120, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Daytime Sleepiness")
  
  # Store the plots
  plots <- list(
    age_group = plot_age_group,
    gender = plot_gender,
    ethnicity = plot_ethnicity,
    sleep_duration = plot_sleep_duration,
    snoring = plot_snoring,
    sleepiness = plot_sleepiness
  )
  
  return(list(regression_summaries = regression_summaries, plots = plots))
}

regression_line_plots <- function(cleaned_data) {
  # Create Age Group categories
  cleaned_data$age_group <- cut(cleaned_data$RIDAGEYR,
                                breaks = c(18, 29, 44, 59, Inf),
                                labels = c("18-29", "30-44", "45-59", "60+"),
                                right = FALSE)
  
  # Create Regression Line Plot for Age Groups
  age_group_regression <- lm(BMXBMI ~ SLD012 * age_group, data = cleaned_data)
  age_group_summary <- summary(age_group_regression)
  
  plot_bmi_sleep_age <- ggplot(cleaned_data, aes(x = SLD012, y = BMXBMI, color = age_group)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Regression of BMI vs Sleep Duration by Age Group",
         x = "Sleep Duration (Hours)", y = "BMI") +
    theme_minimal()
  
  # Create Regression Line Plot for Race/Ethnicity
  ethnicity_regression <- lm(BMXBMI ~ SLD012 * factor(RIDRETH1), data = cleaned_data)
  ethnicity_summary <- summary(ethnicity_regression)
  
  plot_bmi_sleep_race <- ggplot(cleaned_data, aes(x = SLD012, y = BMXBMI, color = factor(RIDRETH1))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Regression of BMI vs Sleep Duration by Race/Ethnicity",
         x = "Sleep Duration (Hours)", y = "BMI", color = "Race/Ethnicity") +
    theme_minimal()
  
  # Return the plots and summaries
  return(list(
    plots = list(
      plot_bmi_sleep_age = plot_bmi_sleep_age,
      plot_bmi_sleep_race = plot_bmi_sleep_race
    ),
    summaries = list(
      age_group_summary = age_group_summary,
      ethnicity_summary = ethnicity_summary
    )
  ))
}

subgroup_boxplots <- function(cleaned_data) {
  # Create Age Group categories
  cleaned_data$age_group <- cut(cleaned_data$RIDAGEYR,
                                breaks = c(18, 29, 44, 59, Inf),
                                labels = c("18-29", "30-44", "45-59", "60+"),
                                right = FALSE)
  
  # Create Sleep Duration categories
  cleaned_data$sl_sleep_group <- cut(cleaned_data$SLD012, 
                                     breaks = c(0, 5, 7, 9, Inf), 
                                     labels = c("<5 hours", "5-7 hours", "7-9 hours", ">9 hours"),
                                     right = FALSE)
  
  # Generate Boxplots for each subgroup
  plot_age_group <- ggplot(cleaned_data, aes(x = age_group, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Age Group")
  
  plot_gender <- ggplot(cleaned_data, aes(x = factor(RIAGENDR), y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Gender", x = "Gender")
  
  plot_ethnicity <- ggplot(cleaned_data, aes(x = factor(RIDRETH1), y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Ethnicity", x = "Race/Ethnicity")
  
  plot_sleep_duration <- ggplot(cleaned_data, aes(x = sl_sleep_group, y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Sleep Duration", x = "Sleep Duration Group")
  
  plot_snoring <- ggplot(cleaned_data, aes(x = factor(SLQ030), y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Snoring Frequency", x = "Snoring Frequency")
  
  plot_sleepiness <- ggplot(cleaned_data, aes(x = factor(SLQ120), y = BMXBMI)) +
    geom_boxplot() +
    labs(title = "BMI by Daytime Sleepiness", x = "Daytime Sleepiness")
  
  # Regression summaries for each subgroup
  age_group_summary <- summary(lm(BMXBMI ~ age_group, data = cleaned_data))
  gender_summary <- summary(lm(BMXBMI ~ factor(RIAGENDR), data = cleaned_data))
  ethnicity_summary <- summary(lm(BMXBMI ~ factor(RIDRETH1), data = cleaned_data))
  sleep_duration_summary <- summary(lm(BMXBMI ~ sl_sleep_group, data = cleaned_data))
  snoring_summary <- summary(lm(BMXBMI ~ factor(SLQ030), data = cleaned_data))
  sleepiness_summary <- summary(lm(BMXBMI ~ factor(SLQ120), data = cleaned_data))
  
  # Store the plots
  plots <- list(
    age_group = plot_age_group,
    gender = plot_gender,
    ethnicity = plot_ethnicity,
    sleep_duration = plot_sleep_duration,
    snoring = plot_snoring,
    sleepiness = plot_sleepiness
  )
  
  # Store the summaries
  summaries <- list(
    age_group_summary = age_group_summary,
    gender_summary = gender_summary,
    ethnicity_summary = ethnicity_summary,
    sleep_duration_summary = sleep_duration_summary,
    snoring_summary = snoring_summary,
    sleepiness_summary = sleepiness_summary
  )
  
  return(list(
    plots = plots,
    summaries = summaries
  ))
}



# Step 1: Load the datasets
demo <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/DEMO_I.XPT")
slq <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/SLQ_I.XPT")
bmx <- read_xpt("/Users/pratham/project_aston/fall_2024/CSnP/BMX_I.XPT")

# Step 2: Clean and merge the data
cleaned_data <- clean_data(demo, slq, bmx)

# Step 3: Descriptive statistics for BMI
stats <- descriptive_stats(cleaned_data)
print("Descriptive Statistics for BMI:")
print(stats)
print(summary(cleaned_data))

# Step 4: Correlation analysis to check relationships between sleep patterns and BMI
cor_matrix <- correlation_analysis(cleaned_data)
print("Correlation Matrix:")
print(cor_matrix)

# Plot heatmap of correlation matrix with enhancements
ggcorrplot(cor_matrix, 
           method = "circle",        # Circle method for visualization
           type = "lower",           # Only plot the lower triangle of the matrix
           lab = TRUE,               # Show correlation values inside the circles
           lab_size = 4,             # Adjust label size for readability
           colors = c("blue", "white", "red"),  # Custom color palette for correlation values
           ggtheme = theme_minimal(),  # Minimal theme for a clean look
           outline.col = "white") +   # White outline around circles for clarity
  ggtitle("Correlation Matrix of Sleep and BMI Variables") +  # Title for the plot
  theme(plot.title = element_text(size = 14, hjust = 0.5))  # Title size and centering


# Step 5: Multiple linear regression to see the effect of sleep patterns on BMI
regression_result <- regression_analysis(cleaned_data)

# Print the model summary, residuals plot, and VIF values
print(regression_result$model_summary)
print(regression_result$vif_values)

grid.arrange(regression_result$residuals_plot, 
             regression_result$qq_plot, 
             regression_result$fitted_vs_sqrt_residuals_plot, 
             regression_result$leverage_vs_residuals_plot, ncol = 2)

# Step 6: Advanced Interaction effects tested
adv_regression_result <- advanced_regression_analysis(cleaned_data)

# Print the model summary, residuals plot, and VIF values
print(adv_regression_result$model_summary)

grid.arrange(adv_regression_result$residuals_plot, 
             adv_regression_result$qq_plot, 
             adv_regression_result$fitted_vs_sqrt_residuals_plot, 
             adv_regression_result$leverage_vs_residuals_plot, ncol = 2)

# Step 7: Subgroup analysis to see how demographic factors influence the relationship
#result <- subgroup_analysis(cleaned_data)
# Call the regression line plots function
# Call the regression line plots function
regression_results <- regression_line_plots(cleaned_data)

# Print the plots
print(regression_results$plots$plot_bmi_sleep_age)
print(regression_results$plots$plot_bmi_sleep_race)

# Print the summaries
print(regression_results$summaries$age_group_summary)
print(regression_results$summaries$ethnicity_summary)

# Call the boxplots function
boxplot_results <- subgroup_boxplots(cleaned_data)

# Print the plots
print(boxplot_results$plots$sleep_duration)
print(boxplot_results$plots$snoring)
print(boxplot_results$plots$sleepiness)

# Print the summaries
print(boxplot_results$summaries$sleep_duration_summary)
print(boxplot_results$summaries$snoring_summary)
print(boxplot_results$summaries$sleepiness_summary)


# Access the summary and plots
#summary_result <- result$summary
#plots_result <- result$plots
