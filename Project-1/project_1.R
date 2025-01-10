# =============================================
# Load Necessary Libraries
# =============================================
# I start by checking and loading all the required libraries. 
# If the 'NHANES' package isn't installed, I install it on the spot.
if (!require("NHANES")) install.packages("NHANES")
if (!require("gridExtra")) install.packages("gridExtra")
library(NHANES)
library(dplyr)   # For data wrangling.
library(ggplot2) # For creating histograms.
library(rmarkdown) # To generate the HTML report at the end.


# =============================================
# Function Definitions: Logic Section
# Basically in this section I define all the
# functions which I later call in the execution
# section
# =============================================

# 1 Function to prepare the dataset.
# I select specific columns, recode categorical variables to factors, and calculate BMI.
# I also filter out participants whose BMI doesn’t fall into "Normal" or "High" categories.
prepare_data <- function(selected_columns = c("Age", "Gender", "Height", "Weight", 
                                              "BPSysAve", "BPDiaAve", "Diabetes", "PhysActive", "BMI")) {
  print("Preparing the data...")
  NHANES<-NHANES[!duplicated(NHANES$ID),]
  
  df <- NHANES %>%
    select(all_of(selected_columns)) %>% # Selecting relevant columns.
    rename_with(~ c("Age", "Sex", "Height", "Weight", "SBP", "DBP", "Diabetes", "PhysicalActivity", "BMI")) %>%
    mutate(across(c(Sex, Diabetes, PhysicalActivity), as.factor)) %>% # Converting to factors.
    #mutate(BMI = Weight / (Height / 100)^2) %>% # Calculating BMI.
    mutate(BMI_Category = case_when(
      BMI >= 18.5 & BMI <= 25 ~ "Normal", # Normal BMI category.
      BMI > 30 ~ "High", # High BMI category.
      TRUE ~ "Other" # Exclude others.
    )) %>%
    filter(BMI_Category %in% c("Normal", "High")) %>% # Keep only Normal and High BMI.]
    na.omit() # Remove rows with NA values (non-finite)
  
  print("Data preparation complete!")
  return(df)
}

# 2) Function to compute summary statistics
# I use this function to calculate means and standard deviations for all numeric variables
compute_summary <- function(data) {
  print("Computing summary statistics...")
  summary <- data %>%
    summarise(
      across(where(is.numeric), 
             list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
             .names = "{col}_{fn}")
    )
  cat("Summary statistics computed as follows!", "\n")
  print(summary)
  return(summary)
}

# 3) Function to sample 250 participants from a given group
# This is necessary for hypothesis generation with the investigator's specified groups
sample_group <- function(data, filter_col, filter_value, n = 250) {
  print(paste("Sampling", n, "participants from", filter_value, "group..."))
  group_sample <- data %>%
    filter(get(filter_col) == filter_value) %>%
    sample_n(n)
  print("Sampling complete!")
  return(group_sample)
}

# 4 Function to perform a z test and return the z score and p value.
# This function compares a sample mean to the population mean.
z_test <- function(sample_mean, population_mean, sd, n) {
  print("Performing z-test...")
  z <- (sample_mean - population_mean) / (sd / sqrt(n)) # Z-score formula.
  p_value <- 2 * (1 - pnorm(abs(z))) # Two-tailed p-value.
  return(list(z = z, p_value = p_value))
}

# 5 Function to create histograms.
# I use this function to visualize the distribution of SBP and DBP across different groups.
create_histogram <- function(data, var, title) {
  print(paste("Creating histogram for", var, "..."))
  ggplot(data, aes_string(x = var)) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = title, x = var, y = "Frequency") +
    theme_minimal()
}

# 6) Function to interpret the z-test results.
# This function helps determine whether the null hypothesis is rejected or not.
interpret_result <- function(z_test_result) {
  if (z_test_result$p_value < 0.05) {
    return("Reject the null hypothesis: Significant difference detected.")
  } else {
    return("Fail to reject the null hypothesis: No significant difference detected.")
  }
}

# =============================================
# Execution Section: Task List in Sequence
# =============================================

# Step 1: Prepare the dataset.
df <- prepare_data()
str(df)  # Display the structure of the cleaned dataset to verify.

# Step 2: Compute summary statistics for the full dataset (Table 1).
table1 <- compute_summary(df)
print("Table 1: Descriptive Statistics")
print(table1)

# Step 3: Generate random samples for the specified groups.
active_group <- sample_group(df, "PhysicalActivity", "Yes")
inactive_group <- sample_group(df, "PhysicalActivity", "No")
normal_bmi_group <- sample_group(df, "BMI_Category", "Normal")
high_bmi_group <- sample_group(df, "BMI_Category", "High")

# Step 4: Perform z-tests to compare group means with population means.
z_active_sbp <- z_test(
  mean(active_group$SBP, na.rm = TRUE), 
  mean(df$SBP, na.rm = TRUE), 
  sd(df$SBP, na.rm = TRUE), 
  n = 250
)
print("Z-Test: SBP (Active Group vs. Population)")
print(z_active_sbp)

z_inactive_sbp <- z_test(
  mean(inactive_group$SBP, na.rm = TRUE), 
  mean(df$SBP, na.rm = TRUE), 
  sd(df$SBP, na.rm = TRUE), 
  n = 250
)
print("Z-Test: SBP (Inactive Group vs. Population)")
print(z_inactive_sbp)

# Step 5a: Create histograms for SBP and DBP distributions.
histograms <- list(
  create_histogram(active_group, "SBP", "Figure 1a: SBP (Active Group)"),
  create_histogram(inactive_group, "SBP", "Figure 1b: SBP (Inactive Group)"),
  create_histogram(active_group, "DBP", "Figure 1c: SBP (Active Group)"),
  create_histogram(inactive_group, "DBP", "Figure 1d: SBP (Inactive Group)"),
  create_histogram(normal_bmi_group, "DBP", "Figure 1e: DBP (Normal BMI)"),
  create_histogram(high_bmi_group, "DBP", "Figure 1f: DBP (High BMI)")
)

# Step 5.b: Arrange histograms in a 3x2 grid format.
grid.arrange(grobs = histograms, ncol = 2)

# Step 6: Interpret the results.
print("Interpretation for Active Group:")
print(interpret_result(z_active_sbp))

print("Interpretation for Inactive Group:")
print(interpret_result(z_inactive_sbp))
