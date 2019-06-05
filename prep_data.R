legis <- read.csv("data/legislature.csv")
library(dplyr)
library(tidyr)

short <- function(value) {
  value <- strsplit(value, " ")
  value <- value[[1]][1]
  return(value)
}

shorten <- function(list) {
  list <- sapply(list, short)
  return(list)
}
prev <- apply(legis[8:13],2, shorten)
print(prev)
legis <- cbind(legis[1:7], apply(legis[8:13],2, shorten))
legis <- mutate(legis, X.15 = replace(X.15, X.15 == 'â€”', NA))
legis <- mutate(legis, X15 = replace(X15, X15 == 'â€”', NA))
legis <- mutate(legis, X17 = replace(X17, X17 == 'â€”', NA))

legis$X.15 <- as.numeric(as.character(legis$X.15))
legis$X15 <- as.numeric(as.character(legis$X15))
legis$X16 <- as.numeric(as.character(legis$X16))
legis$X17 <- as.numeric(as.character(legis$X17))
legis$Total_Abortions <- as.numeric(as.character(legis$Total_Abortions))

p_involve <- legis %>%
  mutate(rate.u15 = X.15 / Total_Abortions * 100) %>%
  mutate(rate.15 = X15 / Total_Abortions * 100) %>%
  mutate(rate.16 = X16 / Total_Abortions * 100) %>%
  mutate(rate.17 = X17 / Total_Abortions * 100) %>%
  mutate(tot_minor = X.15 + X15 + X16 + X17) %>%
  mutate(rate.minor = tot_minor / Total_Abortions * 100)

p_involve <- group_by(p_involve, p_involvement) %>%
  summarize(rate.u15 = mean(rate.u15, na.rm = TRUE),
            rate.15 = mean(rate.15, na.rm = TRUE),
            rate.16 = mean(rate.16, na.rm = TRUE),
            rate.17 = mean(rate.17, na.rm = TRUE),
            rate.minor = mean(rate.minor, na.rm = TRUE),
            Total_Abortions = sum(Total_Abortions, na.rm=TRUE))

p_inv <- p_involve %>%
  gather(age_group, rate, rate.u15, rate.15, rate.16, rate.17)

p_inv$age_group[p_inv$age_group == "rate.u15"] <- 'Under 15'
p_inv$age_group[p_inv$age_group == "rate.15"] <- '15 y/o'
p_inv$age_group[p_inv$age_group == "rate.16"] <- '16 y/o'
p_inv$age_group[p_inv$age_group == "rate.17"] <- '17 y/o'

c_counsel <- subset(legis, BCL_Counseling == 1)
no_c_counsel <- subset(legis, BCL_Counseling == 0)
fp_counsel <- subset(legis, FP_Counseling == 1)
no_fp_counsel <- subset(legis, FP_Counseling == 0)

c_counsel['Counseling'] = 'BCL_Counseling'
no_c_counsel['Counseling'] = 'No_BCL_Counseling'
fp_counsel['Counseling'] = 'FP_Counseling'
no_fp_counsel['Counseling'] = 'No_FP_Counseling'

counsel <- rbind(c_counsel, no_c_counsel, fp_counsel, no_fp_counsel)
counsel$Abortion_Rate <- as.numeric(as.character(counsel$Abortion_Rate))
counsel$Counseling[counsel$Counseling == "BCL_Counseling"] <- "Breast Cancer Link Counseling"
counsel$Counseling[counsel$Counseling == "FP_Counseling"] <- "Fetal Pain Counseling"
counsel$Counseling[counsel$Counseling == "No_FP_Counseling"] <- "No FP Counseling"
counsel$Counseling[counsel$Counseling == "No_BCL_Counseling"] <- "No BCL Counseling"

c_avgs <- group_by(counsel, Counseling) %>%
  summarize(avg_rate = mean(Abortion_Rate, na.rm = TRUE))
colnames(c_avgs)[colnames(c_avgs)=="avg_rate"] <- "Average Abortion Rate"
colnames(c_avgs)[colnames(c_avgs)=="Counseling"] <- "Required Counseling"

p_involve_t <- p_involve[,1:6]
colnames(p_involve_t)[colnames(p_involve_t)=="p_involvement"] <- "Required Parental Involvement"
colnames(p_involve_t)[colnames(p_involve_t)=="rate.u15"] <- "Under 15"
colnames(p_involve_t)[colnames(p_involve_t)=="rate.15"] <- "Age 15"
colnames(p_involve_t)[colnames(p_involve_t)=="rate.16"] <- "Age 16"
colnames(p_involve_t)[colnames(p_involve_t)=="rate.17"] <- "Age 17"
colnames(p_involve_t)[colnames(p_involve_t)=="rate.minor"] <- "All Minors"