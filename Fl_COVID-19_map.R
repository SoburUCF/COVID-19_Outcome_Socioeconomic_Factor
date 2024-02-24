#Import required Packages

library(sf) # to make map and import shape data
library(ggplot2)
library(dplyr)
library(tidyverse) # to modify data

# Download the Florida county map shape data from uscensus website
# Import the data 

shapefile <- st_read("~/Downloads/cb_2022_us_county_500k/cb_2022_us_county_500k.shp")

# Filter the florida counties only
florida_counties_map <- shapefile[shapefile$STATEFP == "12", ] # STATEFP 12, is for Florida

# Rename to county
florida_counties_map <- florida_counties_map %>% rename(county = NAME)

# Merge urban-rural county classification
florida_counties_map <- merge(florida_counties_map, urbanrural_classification, by = "county")

#Florida urban-rural map
ggplot() +
  geom_sf(data = florida_counties_map, aes(fill = area23)) +
  scale_fill_manual(
    values = c("rural" = "skyblue", "urban" = "orange"),
    name = "Rural/Urban Classification"
  ) +
  theme_void()+
  theme(legend.position = c(0.42, 0.3))


####################################
########COVID-19 Outcomes map ######
####################################


# download county wise case and death data

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv"
destfile <- "C:/Florida_data/Florida_map/us_counties_22.txt"
download.file(url,destfile)


#import dataset on cases and death by day
us_counties_22 <- read.csv("C:/Florida_data/Florida_map/us_counties_22.txt")


# Combine counties data from 3 years into a single data frame
us_counties <- rbind(us_counties_20, us_counties_21, us_counties_22)

#Select the florida county only
florida_counties <- us_counties %>% filter(state == "Florida")


# export the master data 
write.csv(florida_counties, "C:/Florida_data/Florida_map/florida_counties_data.csv")


# extract county level cases and death upto 2022 
florida_casedeath <- florida_counties %>% filter(date == "2022-12-31")

#Filter out Unknown counties
florida_casedeath <- florida_casedeath %>% filter(county != "Unknown")

str(florida_casedeath)

# Remove the others column
florida_casedeath <- florida_casedeath %>% select(c("county","cases", "deaths"))

# create the case fatality ratio
florida_casedeath <- florida_casedeath %>% mutate(cfr =  (deaths/cases)*100)

#Export the case and death data 
write.csv(florida_casedeath, "C:/Florida_data/Florida_map/florida_casedeath_data.csv")


#Import florida_casedeath data with population added 
florida_casedeath <- read.csv("C:/Florida_data/Florida_map/florida_casedeath_data.csv")

florida_casedeath <- florida_casedeath %>% mutate(case_rate = (cases/population)*100000, death_rates = (deaths/population)*100000)

#Export the case and death data 
write.csv(florida_casedeath, "C:/Florida_data/Florida_map/florida_casedeath_data.csv")



#compile map data with florida case death data and named masterdata
masterdata2 <- merge(florida_counties_map,florida_casedeath, by = "county", all = TRUE)



#Mapping case fatality ratio in Florida, countywise
ggplot() +
  geom_sf(data = masterdata2, aes(fill = case_rate)) +
  scale_fill_gradientn(
    colors = c( "blue", "yellow", "red"),
    na.value = "grey80",
    limits = c(16000, 68000),
    oob = scales::squish,
    name = "Cases per 100k population") +
  theme_void()+
  theme(legend.position = c(0.42, 0.3))


#check the death rates limit
sort(masterdata$cfr)

#Map fatality ratio in Florida, countywise
ggplot() +
  geom_sf(data = masterdata2, aes(fill = death_rates)) +
  scale_fill_gradientn(
    colors = c("#3cf15f", "#55c89d", "#309d8e", "#247074", "#204651", "#15212b"),
    na.value = "grey80",
    limits = c(148, 804),
    oob = scales::squish,
    name = "Mortality per 100k population") +
  theme_void()+
  theme(legend.position = c(0.42, 0.3))

#Map case fatality rate in Florida, countywise

ggplot() +
  geom_sf(data = masterdata2, aes(fill = cfr)) +
  scale_fill_gradientn(
    colors = c("#08589e","#2b8cbe", "#4eb3d3", "#7bccc4","#a8ddb5", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f","#8a0303"),
    na.value = "grey80",
    limits = c(0.6, 3),
    oob = scales::squish,
    name = "Case fatality Rate") +
  theme_void()+
  theme(legend.position = c(0.42, 0.3))


## Export Shapefile

# Install and load the sf package
install.packages("sf")
library(sf)


# Convert your data frame to a simple features object
florida_COVID_19 <- st_as_sf(masterdata2)

# Export the shapefile
st_write(florida_COVID_19, "~/Documents/Florida_data/Florida_map/florida_COVID_19_map.shp")




