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
  if(issue == "Education"){
    ggplot(data, aes(Level_Of_Access, ed_Rank, fill=med_ed))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Education Ranking")+ggtitle("Education Rank by Abortion Access")
  }else if(issue == "Adoptions"){
    ggplot(data, aes(Level_Of_Access, `ad_FY 2017`/(`POPESTIMATE2017`/1000000), fill = med_ad_rate))+geom_boxplot()+scale_fill_gradientn(colors = c("red", "lightblue"))+xlab("Level of Access")+ylab("Adoptions per Million People")+ggtitle("Adoptions per Capita by Abortion Access")
  }else if(issue == "Foster"){  
    ggplot(data, aes(Level_Of_Access, `f_FY 2017`/(`POPESTIMATE2017`/1000000), fill = med_f_rate))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Kids in Foster Care per Million People")+ggtitle("Foster Kids per Capita by Abortion Access")
  }else if(issue == "Births"){
    ggplot(data, aes(Level_Of_Access, `BIRTHS2017`/(`POPESTIMATE2017`/1000000), fill = med_b_rate))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Births per Million People")+ggtitle("Births Per Capita by Abortion Access")
  }else if(issue == "Mortality"){
    ggplot(data, aes(Level_Of_Access, inf_mort_RATE, fill = med_inf_mort))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Infant Mortality Rate")+ggtitle("Infant Mortality Rate by Abortion Access")
  }else if(issue == "Adoptions/Foster"){
    ggplot(data, aes(Level_Of_Access, `ad_FY 2017`/`f_FY 2017`, fill = med_ad_f))+geom_boxplot()+scale_fill_gradientn(colors = c("red", "lightblue"))+xlab("Level of Access")+ylab("Adoptions per Kid in Foster Care")+ggtitle("Adoptions per Foster Child by Abortion Access")
  }else if(issue == "Teen"){
    ggplot(data, aes(Level_Of_Access, teen_RATE, fill = med_teen))+geom_boxplot()+scale_fill_gradientn(colors = c("lightblue", "red"))+xlab("Level of Access")+ylab("Teen Birth Rate")+ggtitle("Teen Birth Rate by Abortion Access")
  }
}