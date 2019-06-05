library(readxl)
library(dplyr)
library(ggplot2)

adoptions <- read_excel("data/Adoptions.xlsx")
foster <- read_excel("data/Kids_in_FosterCare.xlsx")
education <- read_excel("data/Education_Rank_States.xlsx")
abortions <- read_excel("data/Abortions_By_State_2014.xlsx")
access <- read_excel("data/Abortion_Access.xlsx")
pop <- read.csv("data/population.txt")
infant_mortality <- read.csv("data/infant_mortality_rates.csv")
teen <- read.csv("data/teen_births.csv")

colnames(adoptions) <- paste("ad_", colnames(adoptions), sep = "")
colnames(foster) <- paste("f_", colnames(foster), sep = "")
colnames(education) <- paste("ed_", colnames(education), sep = "")
colnames(infant_mortality) <- paste("inf_mort_", colnames(infant_mortality), sep = "")
colnames(teen) <- paste("teen_", colnames(teen), sep = "")


pop_sel <- pop %>%
  select(NAME, POPESTIMATE2017, BIRTHS2017)

inf_mort <- infant_mortality %>%
  filter(inf_mort_YEAR == 2017) %>%
  mutate(State = state.name[match(inf_mort_STATE,state.abb)]) %>%
  select(State, inf_mort_RATE)

teen_birth <- teen %>%
  filter(teen_YEAR == 2017) %>%
  mutate(State = state.name[match(teen_STATE,state.abb)]) %>%
  select(State, teen_RATE)


data <- abortions %>%
  left_join(education, by = c("State" = "ed_State")) %>%
  left_join(adoptions, by = c("State" = "ad_State")) %>%
  left_join(foster, by = c("State" = "f_State")) %>%
  left_join(access, by = c("State" = "State")) %>%
  left_join(pop_sel, by = c("State" = "NAME")) %>%
  left_join(inf_mort, by = c("State" = "State")) %>%
  left_join(teen_birth, by = c("State" = "State")) %>%
  mutate(Level_Of_Access = factor(Level_Of_Access, levels=c("Severely Restricted", "Restricted", "Some", "Protected", "Strongly Protected"))) %>%
  group_by(Level_Of_Access) %>% 
  mutate(med_ed = median(ed_Rank)) %>%
  mutate(med_ad_rate = median(`ad_FY 2017`/(`POPESTIMATE2017`/1000000))) %>%
  mutate(med_f_rate = median(`f_FY 2017`/(`POPESTIMATE2017`/1000000))) %>%
  mutate(med_b_rate = median(`BIRTHS2017`/(`POPESTIMATE2017`/1000000))) %>%
  mutate(med_inf_mort = median(inf_mort_RATE)) %>%
  mutate(med_ad_f = median(`ad_FY 2017`/`f_FY 2017`)) %>%
  mutate(med_teen = median(teen_RATE))
  
access_boxplot <- function(issue) {
  if(issue == "Education Rank"){
    ggplot(data, aes(Level_Of_Access, ed_Rank, fill=med_ed))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Education Ranking")+ggtitle("Education Rank by Abortion Access")
  }else if(issue == "Adoptions"){
    ggplot(data, aes(Level_Of_Access, `ad_FY 2017`/(`POPESTIMATE2017`/1000000), fill = med_ad_rate))+geom_boxplot()+scale_fill_gradientn(colors = c("red", "lightblue"))+xlab("Level of Access")+ylab("Adoptions per Million People")+ggtitle("Adoptions per Capita by Abortion Access")
  }else if(issue == "Foster Children"){  
    ggplot(data, aes(Level_Of_Access, `f_FY 2017`/(`POPESTIMATE2017`/1000000), fill = med_f_rate))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Kids in Foster Care per Million People")+ggtitle("Foster Kids per Capita by Abortion Access")
  }else if(issue == "Birth Rate"){
    ggplot(data, aes(Level_Of_Access, `BIRTHS2017`/(`POPESTIMATE2017`/1000000), fill = med_b_rate))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Births per Million People")+ggtitle("Births Per Capita by Abortion Access")
  }else if(issue == "Infant Mortality"){
    ggplot(data, aes(Level_Of_Access, inf_mort_RATE, fill = med_inf_mort))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Infant Mortality Rate")+ggtitle("Infant Mortality Rate by Abortion Access")
  }else if(issue == "Adoptions per Foster Child"){
    ggplot(data, aes(Level_Of_Access, `ad_FY 2017`/`f_FY 2017`, fill = med_ad_f))+geom_boxplot()+scale_fill_gradientn(colors = c("red", "lightblue"))+xlab("Level of Access")+ylab("Adoptions per Kid in Foster Care")+ggtitle("Adoptions per Foster Child by Abortion Access")
  }else if(issue == "Teen Birth Rate"){
    ggplot(data, aes(Level_Of_Access, teen_RATE, fill = med_teen))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Teen Birth Rate")+ggtitle("Teen Birth Rate by Abortion Access")
  }
}

issue_text <- function(issue) {
  if(issue == "Education Rank"){
    paste("Education Rank refers to the rankings by education reported by US news. States were ranked from 1 (best) to 50 (worst). The boxplot represents both the spread in Education rankings for states by abortion access and also the median education rank for each level of access.")
  }else if(issue == "Adoptions"){
    paste("Adoptions are shown in per capita values. The value used is the number of abortions per million people. These values are from 2017 and were obtained from the Children's Bureau.")  
  }else if(issue == "Foster Children"){  
    paste("Foster Children are shown in per capita values. The value used is the number of children in foster care per million people. These values are from 2017 and were obtained from the Children's Bureau.")  
  }else if(issue == "Birth Rate"){
    paste("Births are shown in per capita values. The value used is the number of births per million people. These values are from 2017 and were obtained from the Census.")  
  }else if(issue == "Infant Mortality"){
    paste("Infant Mortality refers to the rate of infant deaths. This data was obtained by the CDC.")  
  }else if(issue == "Adoptions per Foster Child"){
    paste("For this issue Adoptions are calculated per Foster Child. This allows for a good estimate of how many children are adopted per child in need. These values are from 2017 and were obtained from the Children's Bureau.")  
  }else if(issue == "Teen Birth Rate"){
    paste("Teen Birth Rate is the rate at which teens are giving birth. These values are from 2017 and were obtained from the CDC.")  
  }
}

table <- data %>% group_by(Level_Of_Access) %>% select(Level_Of_Access, starts_with("med"))
issue_table <- unique(table)