install.packages("dplyr",'lubridate', 'forecast')
library("dplyr", "lubridate","forecast")

#scan data & make a time series
mydata2=scan()
mydatats2 = ts(mydata2, start = c(2018,1,1), frequency = 52)

#ts to df
df <- data.frame(as.matrix(mydatats2), datet=time(mydatats2))
df$new_time <-time(mydatats2) %>% as.numeric() %>% date_decimal() %>% as_date()

#y-y & y-y% : subtract 52 wk lag
df<-df %>% 
  dplyr::mutate(y_diff = df[1]-lag(df[1], n = 52)) %>%
  dplyr::mutate(yoy_pct = (df[1]-lag(df[1], n = 52))/lag(df[1], n = 52))

#ytd, here only for years 2021-2022,  months 1--4 
df %>% mutate(year = year(df$new_time)) %>% mutate(month = month(df$new_time)) %>% 
  filter(year %in% c(2021, 2022)) %>% filter(month %in% c(1, 2, 3,4)) %>%
  group_by(year) %>% summarise (sum = sum(as.matrix.mydatats2.)) # %>% summarise (ytd = (sum/lag(sum, n = 1))-1)

#m-m & m-m% : subtract 52 wk lag
df<-df %>% 
  dplyr::mutate(m_diff = df[1]-lag(df[1], n = 1)) %>%
  dplyr::mutate(mom_pct = (df[1]-lag(df[1], n = 1))/lag(df[1], n = 1))
                
