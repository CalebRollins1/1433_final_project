library(dplyr)
library(tidyverse)
library(ggplot2)
library(stargazer)


zip = read_csv("zip_to_census.csv")
cities = read_csv("uscities.csv")
#The unemployment dataset is too large to upload to github. The dataset can be 
#found at the link in the paper
Unemployment = read_csv("UI Claims - County - Weekly.csv")
Covid = read_csv("COVID - County - Daily.csv")

#Below is the code to find cities with close presidential spreads
#This was used to aid in the search for close mayoral races
votes = read_csv("Voting.csv")
pres_election = votes%>%
  filter(office=="US President")%>%
  group_by(county_fips)%>%
  summarise(D = sum(votes[party=="democratic"],na.rm=TRUE), R = sum(votes[party=="republican"],na.rm = TRUE))

pres_election = pres_election%>%
  mutate(pct_D = D/(D+R),
         county_fips = ifelse(length(county_fips)==4))

close_races = cities %>%
  mutate(county_fips = as.numeric(county_fips))%>%
  left_join(pres_election, by = "county_fips")%>%
  filter(pct_D>=0.49&pct_D<=0.51)



#Below begins the actual data analysis for this project


#Main dataset used. This is the data that was collected by hand
local_election = read_csv("Downloads/local_election.csv")%>%
  mutate(democrat_win = (winner=="D"),
         democrat_pct = as.numeric(democrat_pct))%>%
  mutate(gov_party = ifelse(state=="Texas","R",NA))%>%
  mutate(gov_party = ifelse(state=="Nebraska","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Pennsylvania","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Tennessee","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Kansas","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Colorado","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="California","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Florida","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Virginia","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Wisconsin","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="New Hampshire","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Illinois","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Ohio","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Alaska","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Arkansas","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Indiana","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="New York","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Michigan","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="North Carolina","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Washington","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Oklahoma","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Louisiana","D",gov_party))%>%
  mutate(gov_party = ifelse(state=="Wyoming","R",gov_party))%>%
  mutate(gov_party = ifelse(state=="Connecticut","D",gov_party))%>%
  mutate(same_party = gov_party==winner)


#Combines electoral data with cities unemployment data
ui_cities = Unemployment%>%
  left_join(local_election,by = c("countyfips" = "FIPS_1"))%>%
  mutate(time = 32*month+day_endofweek,
         t2 = time^2,
         t3 = time^3,
         t4 = time^4,
         t5 = time^5,
         t6 = time^6,
         rate = as.numeric(initclaims_rate_regular))%>%
  filter(!is.na(city))
#Combines electoral data with cities covid data
covid_cities = Covid%>%
  left_join(local_election,by = c("countyfips" = "FIPS_1"))%>%
  mutate(time = 32*month+day,
         t2 = time^2,
         t3 = time^3,
         t4 = time^4,
         t5 = time^5,
         t6 = time^6)%>%
  filter(!is.na(city))%>%
  mutate(new_case_rate = as.numeric(ifelse(new_case_rate=='.',0,new_case_rate)),
         new_death_rate = as.numeric(ifelse(new_death_rate=='.',0,new_death_rate)))

#Filters coivd data to only have entries fitting criteria
#Also splits into early and late periods
D = covid_cities%>%
  filter(time>=0&time<=500&democrat_pct>=0.44&democrat_pct<=0.56)
D_early = covid_cities%>%
  filter(time>=106&time<=212&democrat_pct>=0.44&democrat_pct<=0.56)
D_late = covid_cities%>%
  filter(time>=212&time<=318&democrat_pct>=0.44&democrat_pct<=0.56)

#Filters coivd data to only have entries fitting criteria
#Also splits into early and late periods
U = ui_cities%>%
  filter(time>=0&time<=500&democrat_pct>=0.44&democrat_pct<=0.56)
U_early = ui_cities%>%
  filter(time>=106&time<=212&democrat_pct>=0.44&democrat_pct<=0.56)
U_late = ui_cities%>%
  filter(time>=212&time<=318&democrat_pct>=0.44&democrat_pct<=0.56)

#All regressions in the paper
#Dfit regressions are covid death rate regressions
#Ufit are unemployment
#late/early denotes early period (March 10-June20) or late period (June 20- Sept 30)
Dfit = glm(new_death_rate ~ democrat_pct+ democrat_win +state+ as.factor(time), data = D)
Dfit_early = glm(new_death_rate ~ democrat_pct+ democrat_win +same_party+state+ as.factor(time), data = D_early)
Dfit_late = glm(new_death_rate ~ democrat_pct+ democrat_win +state+ as.factor(time), data = D_late)
Ufit = glm(rate ~ democrat_pct+ democrat_win +state+ as.factor(time), data = U)
Ufit_early = glm(rate ~ democrat_pct+ democrat_win +state+ as.factor(time), data = U_early)
Ufit_late = glm(rate ~ democrat_pct+ democrat_win +state+ as.factor(time), data = U_late)

#Makes death rate table
stargazer(Dfit,Dfit_early,Dfit_late,
          title = "Death Rate",
          dep.var.labels = c("Death Rate"),
          column.labels = c("Full Period","Early Period","Late Period"),
          covariate.labels = c("Democrat Percent","Democrat Win"),
          omit = c("state","time")
)

#Makes Unemployment table
stargazer(Ufit,Ufit_early,Ufit_late,
          title = "Unemployment Rate",
          dep.var.labels = c("Unemployment Rate"),
          column.labels = c("Full Period","Early Period","Late Period"),
          covariate.labels = c("Democrat Percent","Democrat Win"),
          omit = c("state","time")
)


#Death Rate graph from paper
D%>%
  ggplot(aes(x = time, y = new_death_rate, color = democrat_win))+
  geom_smooth()+
  scale_x_continuous(breaks=c(106,212,318),
                     labels=c("March 10", "June 20","Sept. 30"))+
  labs(title = "New Deaths Over Time", x = "Date", y = "New Death Rate",color = "Democrat")+
  theme_classic()

#Unemployment Graph from paper
U%>%
  ggplot(aes(x = time, y = rate, color = democrat_win))+
  scale_x_continuous(breaks=c(106,212,318),
                     labels=c("March 10", "June 20","Sept. 30"))+
  labs(title = "Unemployment Rate Over Time", x = "Date", y = "Unemployment Rate",color = "Democrat")+
  geom_smooth()+
  theme_classic()










