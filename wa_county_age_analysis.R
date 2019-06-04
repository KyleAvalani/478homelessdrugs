library(dplyr)
library(urbnmapr)
library(ggplot2)
library(tidyverse)
library(sf)
library(ggvis)
library(plotly)

# Read in raw data
raw_data <- read.csv("data/wa_by_county_and_age_rates.csv", stringsAsFactors = F)

#Clean data up
cleaned_data <- raw_data %>% select(-X, -X.1) #Remove weird extra variables 
cleaned_data$All.Ages[cleaned_data$All.Ages %in% c("*", "N")] <- NA
cleaned_data$X15.19[cleaned_data$X15.19 %in% c("*", "N")] <- NA
cleaned_data$X15.17[cleaned_data$X15.17 %in% c("*", "N")] <- NA
cleaned_data$X18.19[cleaned_data$X18.19 %in% c("*", "N")] <- NA
cleaned_data$X20.24[cleaned_data$X20.24 %in% c("*", "N")] <- NA
cleaned_data$X25.29[cleaned_data$X25.29 %in% c("*", "N")] <- NA
cleaned_data$X30.34[cleaned_data$X30.34 %in% c("*", "N")] <- NA
cleaned_data$X35.39[cleaned_data$X35.39 %in% c("*", "N")] <- NA
cleaned_data$X40.44[cleaned_data$X40.44 %in% c("*", "N")] <- NA
colnames(cleaned_data) <- c("county_name", "all_ages", "15to19","15to17","18to19",
                            "20to24", "25to29", "30to34", "35to39", "40to44")

# Get County Location Data
wa_county_data <- get_urbn_map("counties", sf = T) %>% 
  filter(state_name == "Washington")
  
# Test filtering by year
year_test <- filter(cleaned_data, substring(county_name, nchar(county_name)-3) == 2012)
year_test$county_name <- substring(year_test$county_name, 0, nchar(year_test$county_name) -4)
year_test$county_name <- paste0(year_test$county_name, " County")
year_test <- select(year_test, county_name, all_ages)
colnames(year_test) <- c("county_name", "rate")

wa_map_data <- left_join(wa_county_data, year_test, by = "county_name")

# ggplot() + 
#   geom_sf(wa_map_data, mapping = aes(fill = as.numeric(rate)), color="#FFFFFF") +
#   labs(fill = "Abortion Rate per 1,000 Women")
# 
# please_work <- ggplotly(good_good_map)
