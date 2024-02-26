#Import required packages

library(ggplot2)
library(tidyverse)
library(cowplot)
library(scales)
library(dplyr)
library(zoo)



## Calculage 95% CI for vaccination coverage mean

# Assuming "urban" and "rural" are the levels in the "area23" column
urban_data <- subset(combined_data, area23 == "urban")
rural_data <- subset(combined_data, area23 == "rural")

# Calculate mean and 95% CI for fully_vaccinated in urban counties
urban_mean <- mean(urban_data$fully_vaccinated)
urban_ci <- t.test(urban_data$fully_vaccinated)$conf.int

# Calculate mean and 95% CI for fully_vaccinated in rural counties
rural_mean <- mean(rural_data$fully_vaccinated)
rural_ci <- t.test(rural_data$fully_vaccinated)$conf.int

# Display the results
cat("Urban Mean:", urban_mean, "\n")
cat("Urban 95% CI:", urban_ci[1], "-", urban_ci[2], "\n\n")

cat("Rural Mean:", rural_mean, "\n")
cat("Rural 95% CI:", rural_ci[1], "-", rural_ci[2], "\n")


# Calculate t-test result for Vaccination
t_test_result <- t.test(fully_vaccinated ~ area23, data = combined_data)

# Extract relevant values from the t-test output
mean_rural <- 45.94344
df <- 61.667  # degrees of freedom

# Calculate standard error for rural counties
se_rural <- sqrt(var.test(fully_vaccinated ~ area23, data = combined_data)$statistic / df)

# Calculate 95% confidence interval for the mean in the rural group
ci_rural <- mean_rural + c(-1, 1) * qt(0.975, df) * se_rural
ci_rural


# Extract relevant values from the t-test output
mean_urban <- 69.48
df <- 61.667  # degrees of freedom

# Calculate standard error for rural counties
se_urban <- sqrt(var.test(fully_vaccinated ~ area23, data = combined_data)$statistic / df)

# Calculate 95% confidence interval for the mean in the rural group
ci_urban <- mean_urban + c(-1, 1) * qt(0.975, df) * se_urban
ci_urban


# Plot for Vaccination
ggplot(combined_data, aes(x = area23, y = fully_vaccinated)) +
  geom_boxplot(fill = c("rural" = "skyblue", "urban" = "orange"), width = 0.4) +
  geom_jitter(width = 0.1, shape = 16, color = "black", size = 4, alpha = 0.7) +
  scale_x_discrete(labels = c("urban" = "Urban", "rural" = "Rural")) +
  labs(x = NULL, y = "Vaccination coverage (%)") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  geom_text(
    data = data.frame(group = "urban-rural", p_value = t_test_result$p.value),
    aes(
      x = 1.5,
      y = max(combined_data$fully_vaccinated),
      label = paste("p value < 0.001"),
      vjust = -0.2),size = 4)




# Calculate t-test result for One dose Vaccination
t_test_result <- t.test(vaccine_one_dose ~ area23, data = combined_data)

# Plot for Vaccination one dose
ggplot(combined_data, aes(x = area23, y = vaccine_one_dose)) +
  geom_boxplot(fill = c("rural" = "skyblue", "urban" = "orange"), width = 0.4) +
  geom_jitter(width = 0.1, shape = 16, color = "black") +
  scale_x_discrete(labels = c("urban" = "Urban", "rural" = "Rural")) +
  labs(x = NULL, y = " vaccinated with One dose (%)") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black")) +
  geom_text(
    data = data.frame(group = "urban-rural", p_value = t_test_result$p.value),
    aes(
      x = 1.5,
      y = max(combined_data$fully_vaccinated),
      label = paste("p value < 0.001"),
      vjust = -0.2),size = 4)


###################################################
## Urban Rural time series Vaccination box plot####
###################################################

# import usa vaccination data downloaded from
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh

usa_vaccination <- read_csv("~/Documents/Florida_data/Florida_Socioecono/vaccinationdata_CDC/COVID-19_Vaccinations_in_the_United_States_County.csv", show_col_types = FALSE)

#Subset function to select FL county vaccination data
usa_vaccination <- subset(usa_vaccination, usa_vaccination$Recip_State == "FL")

## Just select and keep the required column
florida_vaccinated <- usa_vaccination %>% dplyr::select(Date, Recip_County, Administered_Dose1_Pop_Pct, Series_Complete_Pop_Pct, Series_Complete_Pop_Pct_SVI, Metro_status)

# Remove unknown county
florida_vaccinated <- florida_vaccinated %>% filter(Recip_County != "Unknown County")

# Remove the " County" part from the county names
florida_vaccinated$Recip_County <- sub(" County", "", florida_vaccinated$Recip_County)


# Define the lists of rural and urban counties
rural_counties <- c(
  "Baker", "Bradford", "Calhoun", "Columbia", "DeSoto", "Dixie", "Flagler",
  "Franklin", "Gadsden", "Gilchrist", "Glades", "Gulf", "Hamilton", "Hardee",
  "Hendry", "Highlands", "Holmes", "Jackson", "Jefferson", "Lafayette", "Levy",
  "Liberty", "Madison", "Nassau", "Okeechobee", "Putnam", "Suwannee", "Taylor",
  "Union", "Wakulla", "Walton", "Washington"
)

urban_counties <- c(
  "Alachua", "Bay", "Brevard", "Broward", "Charlotte", "Citrus", "Clay",
  "Collier", "Duval", "Escambia", "Hernando", "Hillsborough", "Indian River",
  "Lake", "Lee", "Leon", "Manatee", "Marion", "Martin", "Miami-Dade",
  "Monroe", "Okaloosa", "Orange", "Osceola", "Palm Beach", "Pasco", "Pinellas",
  "Polk", "Santa Rosa", "Sarasota", "Seminole", "St. Johns", "St. Lucie",
  "Sumter", "Volusia"
)

# Add a new column "County_Type" based on whether the county is rural or urban
florida_vaccinated <- florida_vaccinated %>%
  mutate(
    County_Type = case_when(
      Recip_County %in% rural_counties ~ "Rural",
      Recip_County %in% urban_counties ~ "Urban",
      TRUE ~ "Unknown" # Handle any other counties not in the lists
    ))


##Safe the date in date format  
florida_vaccinated$Date <- as.Date(florida_vaccinated$Date, "%m/%d/%Y")

#Keep data upto December 2022
florida_vaccinated<- florida_vaccinated%>% subset(Date <= as.Date('2022-12-31'))


# Group the data by "Date" and County_Type and calculate the average percent vaccinated
average_vaccination <- florida_vaccinated %>%
  group_by(Date, County_Type) %>%
  summarize(Avg_Percent_Vaccinated = mean(Series_Complete_Pop_Pct))



# Create the line plot for average urban and rural vaccination
ggplot(average_vaccination, aes(x = Date, y = Avg_Percent_Vaccinated, color = County_Type)) +
  geom_line() +
  labs(
    title = "Average Percent Vaccinated Over Time",
    x = "Date",
    y = "Average Percent Vaccinated",
    color = "County_Type"
  ) +
  theme_classic()



## Vaccination time series line in Urban Vs Rural counties
ggplot(florida_vaccinated, aes(x = Date, y = Series_Complete_Pop_Pct, group = interaction(Recip_County, County_Type), color = County_Type)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Percent Vaccinated",
    color = "County Type") +
  scale_color_manual(
    values = c("Urban" = "#fc8d59", "Rural" = "#3288bd"), # Set color for Urban and Rural
    labels = c("Urban", "Rural") # Set legend labels
  ) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %Y",  expand = c(0, 0)) + # Customize x-axis date format
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 15)) + # Customize y-axis percentage format and ticks
  theme_classic() +
  theme(
    axis.text.x = element_text(color = "black"), # Set x-axis text color to black
    axis.text.y = element_text(color = "black"), # Set y-axis text color to black
    axis.title.x = element_text(color = "black"), # Set x-axis title color to black
    axis.title.y = element_text(color = "black")  # Set y-axis title color to black
  )




### Vaccination timeseries Box Plot Urban Vs Rural###


# Convert the Date column to a proper date format 
florida_vaccinated$Date <- as.Date(florida_vaccinated$Date, "%m/%d/%Y")

# Calculate the rolling 7-day average for each County_Type
florida_vaccinated_bp <- florida_vaccinated %>%
  group_by(County_Type) %>%
  arrange(County_Type, Date) %>%
  mutate(Rolling_Avg = rollmean(Series_Complete_Pop_Pct, k = 7, fill = NA, align = "right"))


# Create a new column for 7-day periods
florida_vaccinated_dose1 <- florida_vaccinated %>%
  mutate(Period2 = as.Date(cut(Date, breaks = "7 days")))


# Create a new column for 7-day periods
florida_vaccinated_bp <- florida_vaccinated %>%
  mutate(Period2 = as.Date(cut(Date, breaks = "7 days")))


# Create the box plot for fully vaccinated
ggplot(florida_vaccinated_bp, aes(x = as.factor(Period2), y = Series_Complete_Pop_Pct, fill = County_Type)) +
  geom_boxplot(width=1, outlier.shape = NA) +
  scale_fill_manual(values = c("skyblue","orange")) +
  labs(
    x = "Date",
    y = "Percent of people fully vaccinated",
    fill = "Counties"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 15)) + # Customize y-axis percentage format and ticks
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black")) + # Rotate x-axis labels for better readability
  theme(legend.position="top")
#panel.grid.major.y = element_line(color = "grey", linetype = "dashed"))


# Create the box plot for dose 1
ggplot(florida_vaccinated_dose1, aes(x = as.character(Period2), y = Administered_Dose1_Pop_Pct, fill = County_Type)) +
  geom_boxplot(width=1, outlier.shape = NA) +
  labs(
    x = "Date",
    y = "Percent of One Dose Vaccinated",
    fill = "County Type"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text()) + # Rotate x-axis labels for better readability
  theme(legend.position="top")

#Subset function to select FL county vaccination data
Sumter_vaccination  <- subset(florida_vaccinated_bp, florida_vaccinated_bp$Recip_County == "Sumter")

