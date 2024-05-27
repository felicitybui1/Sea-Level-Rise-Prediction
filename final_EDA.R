#load data
library('dplyr')

# load data
library(readxl)
satellite<- read_excel("2023.10.30 - Sea_level_Satellite.xlsx")
original <- read_excel("global_basin_timeseries.xlsx")

#####
#eda satelitte

#split year and month in land temp data
satellite$year_fraction <- as.character(satellite$year_fraction)  # Convert to character to manipulate the values
# Extract 'year' and 'month' as separate columns
satellite$year <- substring(satellite$year_fraction, 1, 4)  # Extract the first 4 characters as 'year'
#check na
sapply(satellite, function(x) any(is.na(x)))


colnames(satellite)
colnames(original)
satellite_2<- aggregate(satellite$GMSL_GIA_not_applied, by = list(Year = satellite$Year), FUN = mean)
colnames(satellite_2)[2] <- "New"
colnames(original)[1] <- "Year"

# Merge the datasets by year
merged_data <- merge(original, satellite_2, by = "Year", all = TRUE)

# Filter the merged dataset to include only the values from 1993 to 2018
colnames(merged_data)
merged_data_filtered <- merged_data[merged_data$Year >= 1993 & merged_data$Year <= 2018, c("Year","Altimetry [mean]", "New")]
str(merged_data_filtered)

# Calculate the percentage change for each year
n <- nrow(merged_data_filtered)
percentage_change_original <- rep(NA, n)
percentage_change_original[1] <- 0  # No change for the first value
for (i in 2:n) {
  percentage_change_original[i] <- ((merged_data_filtered[i, "Altimetry [mean]"] - merged_data_filtered[i - 1, "Altimetry [mean]"]) / merged_data_filtered[i - 1, "Altimetry [mean]"]) * 100
}

# Calculate the percentage change for 'New'
percentage_change_new <- rep(NA, n)
percentage_change_new[1] <- 0  # No change for the first value
for (i in 2:n) {
  percentage_change_new[i] <- ((merged_data_filtered[i, "New"] - merged_data_filtered[i - 1, "New"]) / merged_data_filtered[i - 1, "New"]) * 100
}

# Add the newly calculated columns to the dataset
merged_data_filtered$original_pct_change <- percentage_change_original
merged_data_filtered$new_pct_change <- percentage_change_new

# Graph disrtibution of percentage change
library(ggplot2)
library(tidyverse)

# Create distribution plots
ggplot(merged_data_filtered, aes(x = percentage_change_original)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Distribution of Original")

ggplot(merged_data_filtered, aes(x = percentage_change_new)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Distribution of New")

# Perform t-test
t_test_result <- t.test(merged_data_filtered$original_pct_change, merged_data_filtered$new_pct_change)

# Print the t-test result
print(t_test_result)
# p greater than 0.05, null H0 accepted

### Embedding values
# Calculate percentage change from sattelite data from 2018 to 2023

# Extract the necessary data for the specified years
GMSL_2018 <- 42.26081081
GMSL_2019 <- 49.16567568
GMSL_2020 <- 49.71111111
GMSL_2021 <- 53.93378378
GMSL_2022 <- 56.30189189

percentage_change_2019_2018 <- ((GMSL_2019 - GMSL_2018) / GMSL_2018) * 100
percentage_change_2020_2019 <- ((GMSL_2020 - GMSL_2019) / GMSL_2019) * 100
percentage_change_2021_2020 <- ((GMSL_2021 - GMSL_2020) / GMSL_2020) * 100
percentage_change_2022_2021 <- ((GMSL_2022 - GMSL_2021) / GMSL_2021) * 100

# Print the results
percentage_changes <- c(percentage_change_2019_2018, percentage_change_2020_2019, percentage_change_2021_2020, percentage_change_2022_2021)
percentage_changes


# Create a new dataset with the required structure
new_years <- c(1993:2022)
existing_years <- merged_data_filtered$Year
existing_altimetry <- merged_data_filtered$`Altimetry [mean]`

# Create a data frame with the existing data
new_dataset <- data.frame(Year = existing_years, Altimetry_mean = existing_altimetry)

# Add rows for 2019, 2020, 2021, and 2022
new_rows <- data.frame(Year = c(2019, 2020, 2021, 2022), Altimetry_mean = rep(NA, 4))
new_dataset <- rbind(new_dataset, new_rows)

# Print the new dataset
print(new_dataset)

### Creating data for 2018 -2022
# Extract the 'Altimetry [mean]' value for 2018
altimetry_2018 <- new_dataset[new_dataset$Year == 2018, "Altimetry_mean"]

# Calculate the 'Altimetry [mean]' values for 2019 to 2022
altimetry_2019 <- altimetry_2018 * (1 + percentage_change_2019_2018 / 100)
altimetry_2020 <- altimetry_2019 * (1 + percentage_change_2020_2019 / 100)
altimetry_2021 <- altimetry_2020 * (1 + percentage_change_2021_2020 / 100)
altimetry_2022 <- altimetry_2021 * (1 + percentage_change_2022_2021 / 100)

# Update the 'Altimetry [mean]' values in the dataset for the years 2019 to 2022
new_dataset[new_dataset$Year == 2019, "Altimetry_mean"] <- altimetry_2019
new_dataset[new_dataset$Year == 2020, "Altimetry_mean"] <- altimetry_2020
new_dataset[new_dataset$Year == 2021, "Altimetry_mean"] <- altimetry_2021
new_dataset[new_dataset$Year == 2022, "Altimetry_mean"] <- altimetry_2022



################################################################################
# Global temperature prep
Temperature = read.csv("data.csv", skip = 4, header = TRUE)    # remove metadata

#split year and month in land temp data
Temperature$Year <- as.character(Temperature$Year)  # Convert to character to manipulate the values

# Extract 'year' and 'month' as separate columns
Temperature$year <- substring(Temperature$Year, 1, 4)  # Extract the first 4 characters as 'year'
Temperature$month <- substring(Temperature$Year, 5, 6)  # Extract the next 2 characters as 'month'
Temperature <- subset(Temperature, select = -Year)
Temperature <- Temperature[, c(2:ncol(Temperature), 1)]

# Calculate the average anomaly by year
Temperature_Agg <- aggregate(Anomaly ~ year, data = Temperature, FUN = mean)
colnames(Temperature_Agg)[2] <- "Avg_Global_Temp_Anomaly"
colnames(Temperature_Agg)[1] <- "Year"

################################################################################
# Prep GMSL from before 1993
colnames(original)[3] <- "Gauge_GMSL"
sea_lvl_gauge_1993_b = subset(original , Year < 1993, select = c(Year, Gauge_GMSL)) ## gauge data before 1993
new_dataset$Year <- as.numeric(new_dataset$Year) #convert year in new_dataset to use bind_rows
sea_lvl = bind_rows(
  sea_lvl_gauge_1993_b%>% select(Year, sealevel=Gauge_GMSL),
  new_dataset%>% select(Year, sealevel= Altimetry_mean)
)
merged <- merge(sea_lvl, Temperature_Agg, by = "Year", all = TRUE)
colnames(merged)[2] <- "Avg_Global_SL_Anomaly"
final_data <- merged[1:(nrow(merged) - 1), ]

#Export as csv
file_path <- "2022_FINAL_DATA.csv"
write.csv(final_data, file = file_path, row.names = FALSE)

################################################################################
#correlation
#correlation between "Avg_Global_Temp_Anomaly" and "Avg_Global_SL_Anomaly"
cor_global_sealevel <- cor(final_data$Avg_Global_Temp_Anomaly, final_data$Avg_Global_SL_Anomaly, use = "complete.obs")
cor_global_sealevel #0.9364105

################################################################################
# Graph
library(ggplot2)
# Assuming final_data is your dataset

# Convert the Year column to numeric if it is in character format
final_data$Year <- as.numeric(final_data$Year)

# Define the scaling factor for Avg_Global_SL_Anomaly
scaling_factor <- max(final_data$Avg_Global_Temp_Anomaly) / max(final_data$Avg_Global_SL_Anomaly)

# Create the plot
ggplot(final_data, aes(x = Year)) +
  geom_line(aes(y = Avg_Global_Temp_Anomaly, color = "Temperature Anomaly")) +
  geom_line(aes(y = Avg_Global_SL_Anomaly * scaling_factor, color = "Sea Level Anomaly")) +
  scale_y_continuous(
    name = "Avg Global Temp Anomaly",
    sec.axis = sec_axis(~. / scaling_factor, name = "Scaled Avg Global SL Anomaly")
  ) +
  labs(title = "Dual-Axis Plot with Different Scales", x = "Year") +
  theme_minimal() +
  theme(legend.position = "top")



















