# Load the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(furrr) 
library(future)

# Load your dataset (make sure the file path is correct)
df <- read.csv('C:/Users/acer/Documents/R_Case_Studies/FinPrediction/Top_12_German_Companies.csv')


## Inspect dataset

# Inspect the column names
colnames(df)


# Temporary rename for fitting the model
df_temp <- df %>%
  rename(ds = Period, y = Revenue)


# Check for missing values in the y column
missing_values <- sum(is.na(df_temp$y))
print(paste("Number of missing values in Revenue:", missing_values))


# Handle missing values (e.g., remove rows with NA values)
df_temp <- df_temp %>%
  filter(!is.na(y))

# Check for infinite values and remove them
infinite_values <- sum(is.infinite(df_temp$y))
print(paste("Number of infinite values in Revenue:", infinite_values))
df_temp <- df_temp %>%
  filter(!is.infinite(y))

# Check for zero values and remove them if necessary
zero_values <- sum(df_temp$y == 0)
print(paste("Number of zero values in Revenue:", zero_values))
df_temp <- df_temp %>%
  filter(y != 0)

# Final check after cleaning 
print("Data after cleaning:") 
print(summary(df_temp$y))

head(df)


# Convert Period to Date type with the correct format and handle NAs
df <- df %>%
  mutate(Period = as.Date(Period, format="%m/%d/%Y")) %>% 
  filter(!is.na(Period))

# Check the format of the dates
print(head(df$Period))

# Extract Year from Period
df <- df %>%
  mutate(Year = year(Period))

## Summarized Yearly Revenue


# Summarize Revenue Yearly per Company
yearly_revenue <- df %>%
  group_by(Company, Year) %>%
  summarize(Total_Revenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')

# Adjust y-axis limits with padding
y_max <- max(yearly_revenue$Total_Revenue, na.rm = TRUE) * 1.1  # Adding 10% padding to the top

# Scatter Plot of Yearly Revenue per Company
ggplot(yearly_revenue, aes(x=Year, y=Total_Revenue, color=Company)) +
  geom_point(size=3) +
  labs(title="Yearly Revenue per Company", x="Year", y="Total Revenue") +
  scale_y_continuous(labels = comma, expand = c(0, 0), limits = c(0, y_max)) +  # Display real values for y and add padding to the top
  theme_minimal() +
  # Remove company names from x-axis
  theme(axis.title.y = element_blank(), axis.title.x =element_blank())

# Print the yearly_revenue data frame with comma formatting 

yearly_revenue <- yearly_revenue %>% mutate(Total_Revenue = comma(Total_Revenue)) 
print(yearly_revenue, n = Inf)

# Filter data 

df <- df %>%
  filter(Year >= 2017 & Year <= 2024)

# Summarize average Revenue per Company from 2017 to 2024 and arrange by highest average revenue
average_revenue <- df %>%
  group_by(Company) %>%
  summarize(Average_Revenue = mean(Revenue, na.rm = TRUE)) %>%
  arrange(desc(Average_Revenue))

# Print the average revenue with comma formatting
average_revenue <- average_revenue %>%
  mutate(Average_Revenue = scales::comma(Average_Revenue))

print(average_revenue)

# Profitability Analysis

# Summarize average Net Income per Company from 2017 to 2024
average_net_income <- df %>%
  filter(Year >= 2017 & Year <= 2024) %>%
  group_by(Company) %>%
  summarize(Average_Net_Income = mean(Net_Income, na.rm = TRUE)) %>%
  arrange(desc(Average_Net_Income))

# Calculate Net Income Margin (Net Income / Revenue) and average it
net_income_margin <- df %>%
  filter(Year >= 2017 & Year <= 2024) %>%
  group_by(Company) %>%
  summarize(Average_Net_Income_Margin = mean(Net_Income / Revenue, na.rm = TRUE)) %>%
  arrange(desc(Average_Net_Income_Margin))

# Print the results with comma formatting
average_net_income <- average_net_income %>%
  mutate(Average_Net_Income = scales::comma(Average_Net_Income))

net_income_margin <- net_income_margin %>%
  mutate(Average_Net_Income_Margin = scales::percent(Average_Net_Income_Margin))

print(net_income_margin)


## Average Net Income Margin Per Company

# Ensure 'Average_Net_Income_Margin' is numeric
net_income_margin <- net_income_margin %>%
  mutate(Average_Net_Income_Margin = as.numeric(gsub("%", "", Average_Net_Income_Margin)) / 100)

# Plot the average Net Income Margin
ggplot(net_income_margin, aes(x = reorder(Company, -Average_Net_Income_Margin), y = Average_Net_Income_Margin, fill = Company)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +  # Ensure percentage y-axis labels
  labs(title = "Average Net Income Margin (2017-2024) by Company",
       x = "Company",
       y = "Net Income Margin",
       fill = "Company") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank())  # Hide y-axis label

## Observations: 

* Allianz SE stands out as the top performer in terms of net income margin, indicating strong profitability.

* The other companies have relatively similar margins, showing consistent and stable performance

## Predicted 2025 Revenue


# Filter data from 2017 to 2024
yearly_revenue <- df %>%
  filter(Year >= 2017 & Year <= 2024) %>%
  group_by(Company, Year) %>%
  summarize(Total_Revenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')

# Ensure there are no NA/NaN/Inf values in Total_Revenue
yearly_revenue <- yearly_revenue %>%
  filter(is.finite(Total_Revenue))

# Fit a linear regression model for each company and predict revenue for 2025
predicted_revenue <- yearly_revenue %>%
  group_by(Company) %>%
  do({
    model <- lm(Total_Revenue ~ Year, data = .)
    predicted_revenue_2025 <- predict(model, newdata = data.frame(Year = 2025))
    data.frame(Predicted_Revenue_2025 = predicted_revenue_2025)
  }) %>%
  ungroup()

# Print the predicted revenue for 2025 with comma formatting
predicted_revenue <- predicted_revenue %>%
  mutate(Predicted_Revenue_2025 = scales::comma(Predicted_Revenue_2025))

print(predicted_revenue)


## Bar Graph of Predicted Revenue for 2025

# Ensure 'Predicted_Revenue_2025' is numeric
predicted_revenue <- predicted_revenue %>%
  mutate(Predicted_Revenue_2025 = as.numeric(gsub(",", "", Predicted_Revenue_2025)))

# Plot the predicted 2025 revenue
ggplot(predicted_revenue, aes(x = reorder(Company, -Predicted_Revenue_2025), y = Predicted_Revenue_2025, fill = Company)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +  # Ensure exact y-axis labels
  labs(title = "Predicted Revenue for 2025 by Company",
       fill = "Company") +  # Remove y-axis label
  theme_minimal() +  # Remove company names from x-axis
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
       axis.title.y = element_blank())

