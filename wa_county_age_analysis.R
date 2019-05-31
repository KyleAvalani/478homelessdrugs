library(dplyr)

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
colnames(cleaned_data) <- c("county", "all_ages", "15to19","15to17","18to19",
                            "20to24", "25to29", "30to34", "35to39", "40to44")


garbage_test <- filter(cleaned_data, county %in% "2016")
