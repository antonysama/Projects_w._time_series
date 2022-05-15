#to find difference & pct difference from base or budget
install.packages("dplyr","tidyr", "ggplot2","lubridate", "mice","readxl")
x<-c("tidyr", "dplyr", "ggplot2", "plotly","lubridate","zoo", "mice", "readxl") 
lapply(x, require, character.only = TRUE)

library(readxl)
df <- read_excel("Scenarios.xlsx", 
        col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))

#pivot longer
df2<- df %>% pivot_longer(c(-Scenario, -Product), names_to = "Year", values_to = "value") 

#diff from base  & pivot wider
df2 %>% group_by(Product) %>%  mutate(diff  = value - value[Scenario== 'Base']) %>% 
  select (-value) %>% pivot_wider(names_from = Year, values_from = diff) # pvot wider

# pct diff from base, filter, & pivot wider
df3<-df2 %>% group_by(Product) %>%  mutate(pct  = (value/value[Scenario== 'Base'] - 1) * 100) %>% 
  filter(Scenario=="Optimistic", Year %in% c("2020","2025","2030")) %>%   
  select (-value) %>% pivot_wider(names_from = Product, values_from = pct) # pvot wider

# ggplot time series v pct 
df2 %>% group_by(Product) %>%  mutate(pct  = (value/value[Scenario== 'Base'] - 1) * 100) %>% 
  filter(Scenario=="Optimistic", Year %in% c("2020","2025","2030")) %>%
    ggplot(., aes(x=as.numeric(Year), y=pct)) + geom_line(aes(colour=Product))

# plotly time series v pct 
df2 %>% group_by(Product) %>%  mutate(pct  = (value/value[Scenario== 'Base'] - 1) * 100) %>% 
  filter(Scenario=="Optimistic", Year %in% c("2020","2025","2030")) %>%
  plot_ly(., x = ~as.numeric(Year), y = ~pct, color = ~Product, hoverinfo = ~Product,
  type = 'scatter', mode = 'lines',     mode = 'markers')

#create dates df
mydates=seq.Date(from=as.Date("2020-01-01"), to=as.Date("2031-01-01"), by=366)
dates=data.frame(date = mydates) 
dates<- dates%>%  mutate(Year=lubridate::year(dates$date))

#padding df2 with dates
df4 = merge(df3, dates, by = "Year", all.y = T)

#pivot longer & interpolate
df5<- df4 %>% select(-c(2,12))%>% pivot_longer(cols =-c(Year), names_to = "Product", values_to = "value") 
mymice <- mice(df5, m=1, method = "cart") 
complete(mymice) %>% 
  ggplot(., aes(x=as.numeric(Year), y=value)) + geom_line(aes(colour=Product))


#ggplot
complete(mymice) %>% ggplot(., aes(x=as.numeric(Year), y=value)) + geom_line(aes(colour=Product))

#plotly
df5 %>% plot_ly(., x = ~as.numeric(Year), y = ~value, color = ~Product, hoverinfo = ~Product,
        type = 'scatter', mode = 'lines',     mode = 'markers')
