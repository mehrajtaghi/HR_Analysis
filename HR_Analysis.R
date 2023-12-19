library(dplyr)

#task 1

manager_survey_data <- read.csv("/Users/mehrac/Downloads/manager survey data.csv")
general_data <- read.csv("/Users/mehrac/Downloads/general data.csv")
employee_survey_data <- read.csv("/Users/mehrac/Downloads/employee survey data.csv")


combined_data <- inner_join(manager_survey_data, general_data, by = "EmployeeID") %>%
  inner_join(employee_survey_data, by = "EmployeeID")

na_count <- sum(is.na(combined_data))

cat("Number of NA values in the combined dataset:", na_count, "\n")

# task 2 
combined_data$EmployeeID <- as.factor(combined_data$EmployeeID)

# task 3
for (col in colnames(combined_data)) {
  if (sum(is.na(combined_data[[col]])) > 0) {
    mean_val <- mean(combined_data[[col]], na.rm = TRUE)
    combined_data[[col]][is.na(combined_data[[col]])] <- mean_val
  }
}

# task 4
hr_data <- read.csv("/Users/mehrac/Downloads/HR.csv")

# task 5 
unique_counts <- sapply(hr_data, function(x) length(unique(x)))
vars_to_convert <- names(unique_counts[unique_counts <= 20])
hr_data[vars_to_convert] <- lapply(hr_data[vars_to_convert], as.factor)
unique_counts_table <- data.frame(Variable = names(unique_counts), Unique_Values = unique_counts, Data_Type = sapply(hr_data, class))


#task 6
for (col in colnames(hr_data)) {
  if (sum(is.na(hr_data[[col]])) > 0) {
    mean_val <- mean(hr_data[[col]], na.rm = TRUE)
    hr_data[[col]][is.na(hr_data[[col]])] <- mean_val
  }
}

# task 7
hr_data$time_spend_company <- as.numeric(as.character(hr_data$time_spend_company))
hr_data$Experience_Status <- ifelse(hr_data$time_spend_company > 10, "Experienced", "Not experienced")

#task 8 
turnover_rate <- hr_data %>%
  group_by(Department) %>%
  summarize(Turnover_Rate = mean(left == 1, na.rm = TRUE))
highest_turnover_dept <- turnover_rate %>%
  filter(Turnover_Rate == max(Turnover_Rate))

# task 9 
library(ggplot2)

ggplot(hr_data, aes(x = satisfaction_level, y = average_montly_hours, color = left)) +
  geom_point() +
  labs(title = "Why Employees Leave Their Jobs",
       x = "Satisfaction Level",
       y = "Average Monthly Hours",
       color = "Turnover") +
  theme_minimal()

# task 10
hr_data$time_spend_company <- as.numeric(as.character(hr_data$time_spend_company))
hr_data$last_evaluation <- as.numeric(as.character(hr_data$last_evaluation))
hr_data$number_project <- as.numeric(as.character(hr_data$number_project))
valuable_employees_left <- hr_data %>%
  filter(left == 1, 
         time_spend_company > 3,
         last_evaluation > 0.72,
         number_project > 4)

valuable_employees_left
