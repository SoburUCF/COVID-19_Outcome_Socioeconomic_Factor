
library(tidyverse)

# Read data
cor_data <- read_csv("correlation_cof.csv")

# Define the desired order for the predictors
desired_order <- c("Median age", "Aged", "Median income", "Rural population", "Unemployment Rate",
                    "Below poverty level", "Hospital beds", "Acute care beds",
                   "Health insurance", "Vaccination one dose", "Fully vaccinated", "Adult obesity",
                   "Diabetes", "Smoking", "Black", "Hispanic")

# Convert the "Predictor" column to a factor with the desired order
cor_data$Predictor <- factor(cor_data$Predictor, levels = rev(desired_order))

# Calculate significance
cor_data <- cor_data %>%
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ ""
  ))

# Reorder Outcome factor levels
cor_data$Outcome <- factor(cor_data$Outcome, levels = c("Case Rate", "Mortality Rate", "Case Fatality Rate"))


# Create heatmap
ggplot(cor_data, aes(x = Outcome, y = Predictor, fill = Cor_cof)) +
  geom_tile() +
  geom_text(aes(label = significance), color = "black", size = 4) +
  scale_fill_gradient2(name = "Correlation", 
                       low = "green4", mid = "white", high = "red3",
                       limits = range(cor_data$Cor_cof),
                       labels = scales::number_format(accuracy = 0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, color = "black"),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_blank(),  # Remove x-axis label
        legend.position = "right") +
  labs(x = NULL, y = "Predictor Variables") +
  scale_x_discrete(limits = c("Case Rate", "Mortality Rate", "Case Fatality Rate"),
                   position = "top") +
  coord_cartesian(expand = FALSE) 

