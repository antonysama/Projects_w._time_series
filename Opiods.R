#read data, rename variables, examine and change var types
library(magrittr) 
library(dplyr)
library(palmerpenguins)
library(skimr)
library(tidyr)
library(janitor)
library(readxl)
library(ggplot2)
library(reshape2)
library(datetime)
library(readr)
OAT <- read_csv("Downloads/Opioids/OAT.csv",  col_names = FALSE, 
                col_types = cols(X2 = col_factor(levels = c("A","B", "C")), X3 = col_date(format = "%Y-%m")))
OAT<-OAT %>% rename("ID"="X1","Drug"="X2","Date"="X3")
skim_without_charts(OAT)
OAT$ID<-as.factor(OAT$ID)

#select only drug B (nalaxone) for policy analysis 
OAT2<-OAT %>% filter(Drug == "B") %>% group_by(Date)%>%summarise(num = n_distinct(ID))

#graph the policy date cutoffs
ggplot()+geom_line(aes(y=num, x=Date),size=.5, data=OAT2)+
  geom_vline(xintercept=as.numeric(OAT2$Date[c(95, 104)]), linetype="dotted")
#to find dates/months between policies
as.numeric(difftime(datetimes[2015-01-01], datetimes[2007-12-01], units = "days")) 
difftime("2007-12-1", "2016-7-1", units = "days")/30

#Before Policy (A) v Policy B v Policy C
OAT2$Policy<-ifelse(OAT2$Date <'2015-10-01',"A <2015-10", ifelse(OAT2$Date > '2016-07-01', "C >2016-07", "B 2015-2016"))
summary(lm(num~Date+ factor(Policy), data=OAT2))

#Policy B v C
OAT3<-OAT2 %>% filter(Date > '2015-10-1') 
table(OAT3$Policy)
summary(lm(num~Date+ factor(Policy), data=OAT3))

