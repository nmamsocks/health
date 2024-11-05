library(ggplot2)
library(dplyr)

# Function for the Health and Fitness Dataset
generate_data <- function(n) {

  # Setting the seed
  set.seed(42)
  
  # Creating the dataset
  data <- data.frame(
    Age = sample(18:80, n, replace = TRUE),
    Height = sample(150:190, n, replace = TRUE),
    Weight = sample(40:120, n, replace = TRUE),
    Exercise = sample(0:10, n, replace = TRUE),
    Diet = sample(c("balanced", "high-protein", "vegetarian"), n, replace = TRUE),
    Sleep = sample(5:10, n, replace = TRUE),
    MedicalCondition = sample(0:1, n, replace = TRUE)
  )
  
  return(data)
}

# Function to preprocess the data
process_data <- function(data) {

  # Calculate BMI
  data$BMI <- data$Weight / (data$Height / 100)^2
  
  # Convert to factors
  data$Diet <- factor(data$Diet, levels = c("balanced", "high-protein", "vegetarian"))
  data$MedicalCondition <- factor(data$MedicalCondition, levels = c(0, 1))
  
  return(data)
}

# Function for the linear regression model
fit_model <- function(data) {
  model <- lm(BMI ~ Age + Height + Weight + Exercise + Sleep + Diet + MedicalCondition, data = data)
  return(model)
}

# Function to display the model summary
print_model_summary <- function(model) {
  summary(model)
}

# Function to print the statistics (mean, median, sd)
print_stats <- function(data) {
  cat("\nStatistics:\n")
  
  # Summary statistics for numeric columns
  numeric_data <- data[, sapply(data, is.numeric)]
  stats <- data.frame(
    Feature = names(numeric_data),
    Mean = sapply(numeric_data, mean, na.rm = TRUE),
    Median = sapply(numeric_data, median, na.rm = TRUE),
    SD = sapply(numeric_data, sd, na.rm = TRUE)
  )
  
  print(stats)
}

# Main function
run_analysis <- function(n) {

  # timer
  start_time <- Sys.time()
  
  # Generate 
  data <- generate_data(n)
  data <- process_data(data) 
  model <- fit_model(data)
  print_model_summary(model)
  
  # Print 
  print_stats(data)
  
  # Display 
  print(head(data))  
  
  # print the time taken
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  cat("Time taken:", as.numeric(time_taken, units = "secs"), "seconds.\n")
}

# Run 1000 times
run_analysis(1000)
