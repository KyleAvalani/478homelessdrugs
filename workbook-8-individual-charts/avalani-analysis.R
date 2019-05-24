library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.csv("../data/WashingtonAbortionYearly.csv", stringsAsFactors = F)
data <- na.omit(data)
colnames(data) <- c("Year", "15 to 19", "15 to 17", "18 to 19")
data <- melt(data, id = "Year")
colnames(data) <- c("Year", "Age", "value")

## These are rates per 1,000 women in their age group
ggplot(data, aes(x=Year, y=value, group=Age)) + 
  geom_point(aes(color=Age)) + 
  geom_line(aes(color=Age)) + 
  labs(title = "Abortion Rates in WA State per 1,000 Women", y="Rate (per 1,000 Women)", fill = "Abortion Rate")
