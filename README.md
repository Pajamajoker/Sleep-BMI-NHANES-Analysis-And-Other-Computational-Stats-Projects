# **Overview** 🌙📊

This project analyzes the relationship between **sleep patterns** and **Body Mass Index (BMI)** using data from the **National Health and Nutrition Examination Survey (NHANES)**. The study focuses on adults, examining:

- 💤 Sleep duration
- 😴 Snoring frequency
- 🌞 Daytime sleepiness
- ⚖️ BMI
- 👥 Demographic variables such as age, gender, and ethnicity.

# **Analysis Report Link** 📄🔗
<a href="./The-Impact-of-Sleep-Patterns-on-Body-Mass-Index-Prathamesh-Joshi.pdf">Download PDF Report</a>

# **Study Purpose** 🎯

The goal of this study is to:

- Investigate how sleep-related factors correlate with **BMI** in adult populations.
- Understand the influence of demographic factors such as **age**, **gender**, and **ethnicity** on these relationships.

# **Key Research Parameters** 🧑‍🔬📋

### Sleep Variables 🛏️:
- **Sleep Duration**: Total hours of sleep per night.
- **Snoring Frequency**: How often the individual experiences snoring.
- **Daytime Sleepiness**: Subjective experience of sleepiness during the day.

### Health Outcome 🏋️‍♂️:
- **Body Mass Index (BMI)**: A measure of body fat based on height and weight.

### Demographic Factors 🌍:
- **Age**: Age of the participants (16-80 years).
- **Gender**: Male or female participants.
- **Ethnicity**: Racial and ethnic background of participants.

# **Methods** 🛠️

### Data Preparation 🧹
- **Datasets Used**: Merged NHANES datasets: `DEMO_I.XPT`, `SLQ_I.XPT`, and `BMX_I.XPT`.
- **Data Cleaning**: 
  - 🗑️ Removed rows with missing essential values.
  - 🔄 Converted categorical variables into factors.
  - ⏰ Standardized time-related data (e.g., sleep duration).

### Statistical Analysis 📈
- **Descriptive Statistics**: Summary statistics for numerical and categorical variables.
- **Correlation Analysis**: Investigated relationships between sleep patterns and BMI.
- **Multiple Linear Regression**: Analyzed predictors of BMI.
- **Diagnostic Plots**: 
  - 📉 Residuals vs. Fitted plots
  - 📊 Q-Q Plot for normality checks
- **Multicollinearity Check**: Using Variance Inflation Factor (VIF) to ensure no high correlation among predictors.

### Data Visualization 📊
- Created **interactive plots** and **graphs** to visually demonstrate key relationships and distributions.

# **Key Findings** 🔑

- **Demographic Diversity** 🌍: 
  - 👶 Age range: 16-80 years.
  - ⚖️ BMI range: 14.5 - 67.3.
  - 🚻 Near-equal gender distribution and diverse ethnic representation.
  
- **Sleep Pattern Variability** 💤: 
  - Significant differences in sleep duration, snoring frequency, and daytime sleepiness across the population.
  
- **BMI Predictors** 📉: 
  - Multiple linear regression found significant predictors of BMI, including **sleep duration**, **snoring frequency**, **daytime sleepiness**, **age**, and **gender**.

- **Model Robustness** ✅: 
  - Diagnostic plots and multicollinearity checks confirmed the reliability of the regression model.

# **Conclusion** 📝

This study sheds light on the complex relationship between **sleep patterns** and **BMI** in adults. Key takeaways include:

- 💤 Sleep duration and sleep-related factors are significant predictors of BMI.
- 👨‍👩‍👧‍👦 Demographic factors, such as **age** and **gender**, also play a role in BMI.
  
These insights are important for developing targeted public health strategies and interventions to address obesity and sleep-related issues.

# **Author** ✍️
Prathamesh Joshi
