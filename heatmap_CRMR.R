## Import required packages
library(tidyverse)
library(dplyr)
library(cowplot) # simple add-on to ggplot
library(scales)
library(readxl)
library(cetcolor) # For heatmap color gradient


#set working directory 
setwd("~/Documents/Florida_data/Florida_Socioecono/caseheatmap/")

# download COVID-19 case data

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2020.csv"
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2021.csv"
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2022.csv"

destfile <- "~/Documents/Florida_data/Florida_Socioecono/caseheatmap/us22.txt"
download.file(url,destfile)

#import data set 
us20 <- read.csv("~/Documents/Florida_data/Florida_Socioecono/caseheatmap/us20.txt")
us21 <- read.csv("~/Documents/Florida_data/Florida_Socioecono/caseheatmap/us21.txt")
us22 <- read.csv("~/Documents/Florida_data/Florida_Socioecono/caseheatmap/us22.txt")

#combined all in one dataframe
usdataall <- rbind(us20, us21, us22)

#Subset FL data 
fldata_all <- subset(usdataall, usdataall$state == "Florida")
fldata_all <- fldata_all %>% select(c("date", "county", "cases_avg_per_100k", "deaths_avg_per_100k", "cases_avg", "deaths_avg" )) # keep only required column
fldata_all <- fldata_all %>% rename(caserate = cases_avg_per_100k, deathrate = deaths_avg_per_100k) # rename to caserate and deathrate

# Remove unknown county
fldata_all <- fldata_all %>% filter(county != "Unknown")

# Export the data
write_csv(fldata_all, "casedeath_fldata_all.csv")

#Safe the date in date format  
fldata_all$date <- as.Date(fldata_all$date)



# Define the custom order of counties
custom_order <- c(
  "Broward", "Pinellas", "Miami-Dade", "Palm Beach", "Orange", "Duval", "St. Lucie",
  "Sarasota", "Hillsborough", "St. Johns", "Indian River", "Brevard", "Manatee", "Lee",
  "Osceola", "Escambia", "Martin", "Collier", "Monroe", "Charlotte", "Pasco", "Volusia",
  "Flagler", "Bay", "Okaloosa", "Leon", "Polk", "Clay", "Lake", "Hernando", "Seminole",
  "Highlands", "Alachua", "Santa Rosa", "Marion", "Citrus", "Sumter", "Okeechobee",
  "Hendry", "DeSoto", "Hardee", "Nassau", "Putnam", "Baker", "Wakulla", "Columbia",
  "Walton", "Hamilton", "Gadsden", "Union", "Calhoun", "Franklin", "Taylor", "Glades",
  "Jackson", "Bradford", "Dixie", "Gulf", "Holmes", "Madison", "Suwannee", "Gilchrist",
  "Washington", "Levy", "Jefferson", "Lafayette", "Liberty")

## Order with 2020 census data
county_order20 <- c(
  "Broward", "Pinellas", "Palm Beach", "Miami-Dade", "Orange", "Sarasota", "Duval",
  "Seminole", "St. Lucie", "Hillsborough", "Brevard", "Lee", "Manatee", "Osceola",
  "Charlotte", "Indian River", "Pasco", "Martin", "Escambia", "Flagler", "Volusia",
  "Okaloosa", "Polk", "Collier", "Monroe", "Leon", "Bay", "Clay", "St. Johns", "Lake",
  "Highlands", "Santa Rosa", "Hernando", "Sumter", "Alachua", "Citrus", "Marion",
  "Hendry", "Okeechobee", "Nassau", "DeSoto", "Walton", "Hardee", "Baker", "Columbia",
  "Wakulla", "Taylor", "Bradford", "Putnam", "Gadsden", "Glades", "Suwannee", "Jackson",
  "Calhoun", "Dixie", "Franklin", "Gilchrist", "Gulf", "Hamilton", "Holmes", "Jefferson",
  "Lafayette", "Levy", "Liberty", "Madison", "Union", "Washington"
)


# Create a factor variable based on custom order
#fldata_all$county <- factor(fldata_all$county, levels = custom_order)

# Create a factor variable based on custom order census 2020
fldata_all$county <- factor(fldata_all$county, levels = county_order20)

# Final heatmap for case rate
ggplot(fldata_all, aes(x = date, y = county, fill = caserate)) +
  geom_tile() +
  scale_fill_gradientn(name = "log(Case Rates)", colors = (cet_pal(4, name = "r2"))) +
  labs(x = "Date",
       y = "County") +
  theme_classic() +
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-12-31"), by = "4 months"),
               labels = scales::date_format("%h-%y"), 
               expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"))


# Final heatmap for case rate log transformed
ggplot(fldata_all, aes(x = date, y = county, fill = caserate)) +
  geom_tile() +
  scale_fill_gradientn(name = "log(Case Rates)", colors = (cet_pal(4, name = "r2")),
                       trans = "log") +
  labs(x = "Date",
       y = "County") +
  theme_classic() +
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-12-31"), by = "4 months"),
               labels = scales::date_format("%h-%y"), 
               expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"))

# Final heatmap for Death rate

ggplot(fldata_all, aes(x = date, y = county, fill = deathrate)) +
  geom_tile() +
  scale_fill_gradientn(name = "log(Mortaliry Rate)", colors = (cet_pal(4, name = "r2"))
                       )+
  labs(x = "Time",
       y = "Counties") +
  theme_classic() +
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-12-31"), by = "4 months"),
               labels = scales::date_format("%h-%y"), 
               expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"))


# Final heatmap for Death rate with Log transformation

ggplot(fldata_all, aes(x = date, y = county, fill = deathrate)) +
  geom_tile() +
   scale_fill_gradientn(name = "log(Mortaliry Rate)", colors = (cet_pal(4, name = "r2")),
                       trans = "log")+
  scale_y_discrete(
    labels = fldata_all$county,
    aesthetics = c(color = fldata_all$label_color)
  ) +
  labs(x = "Time",
       y = "Counties") +
  theme_classic() +
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-12-31"), by = "4 months"),
               labels = scales::date_format("%h-%y"), 
               expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"), 
        legend.position = "right")


#### Change the color of the county

# Create a new column to specify the label color based on Urban or Rural
fldata_all$label_color <- ifelse(fldata_all$county %in% urban_counties, "Urban", "Rural")

library(ggplot2)
library(cetcolor)

# Your data frame
# fldata_all <- ...

ggplot(fldata_all, aes(x = date, y = county, fill = deathrate)) +
  geom_tile() +
  scale_fill_gradientn(
    name = "log(Mortality Rate)",
    colors = cet_pal(4, name = "r2"),
    trans = "log"
  ) +
  scale_y_discrete(labels = function(x) {
    ifelse(x == "Urban", scales::label_color()(x, "orange"),
           scales::label_color()(x, "skyblue"))
  })
library(cetcolor)

color_vector <- ifelse(fldata_all$label_color == "Urban", "orange", "skyblue")

ggplot(fldata_all, aes(x = date, y = county, fill = deathrate)) +
  geom_tile() +
  scale_fill_gradientn(
    name = "log(Mortality Rate)",
    colors = cet_pal(4, name = "r2"),
    trans = "log"
  ) +
  scale_y_discrete(labels = scales::label_color()(color_vector))

library(ggplot2)
library(cetcolor)

# Your data frame
# fldata_all <- ...

# Create a color vector based on the 'label_color' column
color_vector <- ifelse(fldata_all$label_color == "Urban", "orange", "skyblue")

# Define custom labels with colors
custom_labels <- scales::label_manual(
  values = setNames(color_vector, fldata_all$county)
)

ggplot(fldata_all, aes(x = date, y = county, fill = deathrate)) +
  geom_tile() +
  scale_fill_gradientn(
    name = "log(Mortality Rate)",
    colors = cet_pal(4, name = "r2"),
    trans = "log"
  ) +
  scale_y_discrete(labels = custom_labels)



#filtered_data <- fldata_all[fldata_all$deathrate > 0, ]

# Create the heatmap with filtered data
heatmap_plot <- ggplot(filtered_data, aes(x = date, y = county, fill = deathrate)) +
  geom_tile() +
  scale_fill_gradientn(name = "log(Mortality Rate)", colors = (cet_pal(4, name = "r2")), trans = "log") +
  labs(x = "Time", y = "Counties") +
  theme_minimal()


## Total Case Rate and Mortality Rate bar plot 

# Create a separate data frame for total caserate per county
total_casmor_rate <- fldata_all %>%
  group_by(county) %>%
  summarize(total_caserate = sum(caserate), totol_mortalrate = sum(deathrate))


# Ordered barplot for Death Rate rate
ggplot(total_casmor_rate, aes(x =county, y = total_caserate)) +
  geom_bar(stat = "identity", fill = "#1f78b4", alpha = 0.8, width=0.8) +
  labs(x = NULL, y = "Case Rates") +
  scale_y_continuous(labels = scales::comma, position = "left", expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"))+
   theme(
    axis.text.y = element_blank(),  # Remove x-axis labels
    axis.ticks.y = element_blank()  # Remove x-axis ticks
   ) +
  coord_flip()


# Ordered barplot for Death Rate rate
ggplot(total_casmor_rate, aes(x =county, y = totol_mortalrate)) +
  geom_bar(stat = "identity", fill = "#1f78b4", alpha = 0.8, width=0.8) +
  labs(x = NULL, y = "Death Rates") +
  scale_y_continuous(labels = scales::comma, position = "left", expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black"))+
  theme(
    axis.text.y = element_blank(),  # Remove x-axis labels
    axis.ticks.y = element_blank()  # Remove x-axis ticks
  ) +
  coord_flip()


###############################################
# Case geom line to put on the top of the figure
###############################################

#set working directory 
setwd("~/Documents/Florida_data/Florida_Socioecono/caseheatmap/")

# download Florida state COVID-19 case data not county wise

destfile <- "~/Documents/Florida_data/Florida_Socioecono/caseheatmap/us_states.txt"
download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-states.csv",destfile)


#import dataset 
usa_casedeath <- read.csv("~/Documents/Florida_data/Florida_Socioecono/caseheatmap/us_states.txt")


names(usa_casedeath)

#Subset FL data
fl_state_casedeath <- subset(usa_casedeath, usa_casedeath$state == "Florida")

##Safe the date in date format  
fl_state_casedeath$date <- as.Date(fl_state_casedeath$date)

#Subset of data during the study period
fl_state_casedeath <- fl_state_casedeath %>% subset(date <= as.Date('2022-12-31'))

# Florida Case Line plot

ggplot(fl_state_casedeath, aes(x = date)) +
  geom_line(aes(y = cases_avg), color = "red", size = 0.8) +
  labs(x = NULL,
       y = "COVID-19 cases") +
  theme_classic() +
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-12-31"), by = "4 months"),
               labels = scales::date_format("%h-%y"), 
               expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))+
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )


## Florida Death Line plot
ggplot(fl_state_casedeath, aes(x = date)) +
  geom_line(aes(y = deaths_avg), color = "red", size = 0.8) +
  labs(x = NULL,
       y = "COVID-19 Death in Fl") +
  theme_classic() +
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-12-31"), by = "4 months"),
               labels = scales::date_format("%h-%y"), 
               expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"))+
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )
