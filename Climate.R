install.packages("magrittr")
install.packages("palmerpenguins")
install.packages("dplyr")
install.packages("skimr")
install.packages("janitor")
install.packages("reshape2")
library(magrittr) 
library(dplyr)
library(palmerpenguins)
library(skimr)
library(tidyr)
library(janitor)
library(readxl)
library(ggplot2)
library(reshape2)
#read, bind, format,
one <- read_excel("Downloads/Climate/ceei.xls", sheet = "2007")
two <- read_excel("Downloads/Climate/ceei.xls", sheet = "2010")
climate<-rbind(one, two)
climate<-clean_names(climate)
climate$connections<-as.numeric(climate$connections)
climate$consumption<-as.numeric(climate$consumption)
climate$energy_gj<-as.numeric(climate$energy_gj)
climate$co2e_t<-as.numeric(climate$co2e_t)
#new cols
climate$local<-ifelse(climate$local_govt_type == "Island Municipality"| climate$local_govt_type == "Island Trust Area"|
  climate$local_govt_type == "Indian Government District"|climate$local_govt_type=="Regional Municipality"|
    climate$local_govt_type=="Town"|climate$local_govt_type=="Village","Island/Indian/RM/Twn/Vill",
  climate$local_govt_type)
climate$sectr<-ifelse(climate$sector == "Agriculture"| climate$sector=="Land-use Change - Deforestation"|
                        climate$sector=="Solid Waste","Luluc+Waste",climate$sector)

#inspect variables, distributions (histo/boxplot/density)
skim_without_charts(climate)
hist(climate$co2e_t)
table(climate$local)
round(prop.table(table(climate$local,climate$sectr), margin = 1),1)
my_data<-climate[,c("connections","consumption","energy_gj","co2e_t","population")]
round(cor(my_data,use = "complete.obs"),1)
pairs(~connections+consumption+energy_gj+co2e_t+population, data = climate)
boxplot(co2e_t~local,data=climate, main="connections by sector", outline=FALSE)
p<-ggplot(climate, aes(sectr, co2e_t)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust = 1))
ylim1 = boxplot.stats(climate$co2e_t)$stats[c(1, 5)]
p1 = p + coord_cartesian(ylim = ylim1*1.05)
p1
#unpivot, create policy (target) variable, cross-tab with explanatory
climate2<-climate %>% select(local, sectr, report_year,co2e_t)%>%
  group_by(local, sectr, report_year)%>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = report_year, values_from = co2e_t)

climate2$policy<- ifelse( (climate2$"2010"-climate2$"2007")*100/climate2$"2007" < -3, 
                                       "achieved", "no") 
freq<-round(100*prop.table(table( climate2$sectr,climate2$policy,exclude = NULL), margin = 1),0)

#change type to factor before regressing
climate2$policy<-factor(climate2$policy)
climate2$local<-factor(climate2$local)
climate2sectr<-factor(climate2$sectr)
climate2$policy <- relevel(climate2$policy, ref = "no")
climate2$local <- relevel(climate2$local, ref = "Island/Indian/RM")
climate2$sectr <- relevel(climate2$sectr, ref = "Buildings")
m <- glm(climate2$policy ~ climate2$sectr, family=binomial (link=logit)) 
summary(m) 

#group by and subtract 2010-2007
climate %>% group_by(regional_district, sectr,report_year) %>% summarise(mean = sum(co2e_t, na.rm=TRUE), n = n()) %>%
  mutate(change= ifelse(report_year == "2007", NA_character_, mean-lag(mean))) %>% 
  mutate(pct_change= as.numeric(change)*100/mean)
