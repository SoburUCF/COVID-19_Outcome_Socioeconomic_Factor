######################################
##All death age adjusted #############
######################################

#Required Packages

library(ggplot2)
library(tidyverse)
library(ggforce)

#Import data

all_death_rate <- read.csv("~/Documents/Florida_data/Florida_Socioecono/all_deaths/Age-adjusted_all_causes_death_2018-22_organised.csv", header = TRUE)
all_death_rate$year <- as.character(all_death_rate$year)
all_death_rate$drate <- as.numeric(all_death_rate$drate)

# Combined violin plot
ggplot(all_death_rate, aes(x = year, y = drate, fill = area23)) +
  geom_violin(position = position_dodge(width = 0.7))+
  geom_boxplot(width = 0.2, outlier.colour = NA, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("skyblue","orange")) +
  geom_sina(alpha = 0.6) + 
  theme_classic() +
  labs(x = "Years",
       y = "Age adjusted all causes death rate",
       fill = "Counties") +
  theme(legend.position = "top",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))





##############################################
### Pre_Post Pandemic death rate increase ####
##############################################

## Inport Data
prepost_inrate2 <- read.csv("~/Documents/Florida_data/Florida_Socioecono/all_deaths/Age-adjusted_all_causes_death_2018-22.csv", header = TRUE)

## histogram to check the data distribution in urban and rural counties
prepost_inrate2 %>%
  filter(area23 == "rural") %>%
  ggplot(aes(x = InRate_20)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Histogram of inrate_20 for Rural Data")

# calculate t-test for urban vs rural increase rate
t.test(InRate_20 ~ area23, data = prepost_inrate2)
t.test(InRate_21 ~ area23, data = prepost_inrate2)
t.test(InRate_22 ~ area23, data = prepost_inrate2)

####For combined box plot#

prepost_inrate3 <- read.csv("~/Documents/Florida_data/Florida_Socioecono/all_deaths/Age-adjusted_all_causes_death_2018-22_V2.csv", header = TRUE)
prepost_inrate3$Year <- as.character(prepost_inrate3$Year)

ggplot(prepost_inrate3, aes(x = Year, y = InRate, fill = area23)) +
  geom_boxplot(width = 0.7, outlier.colour = NA, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("skyblue","orange")) +
  geom_sina(alpha = 0.7) + 
  theme_classic() +
  labs(x = "Years",
       y = "Changed from 2019 (%)",
       fill = "Counties") +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12),  # Adjust the font size for the x-axis title
        axis.title.y = element_text(size = 12))
