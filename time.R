install.packages('dplyr', "plotly", 'forecast','lubridate', "devtools", "RJDemetra", dependencies = TRUE)
library(forecast, dplyr, lubridate, devtools, RJDemetra) # "Package currently not installed"
install_github("palatej/rjdhighfreq")
install.packages("xts", repos="http://cloud.r-project.org")

#scan data & make a time series
mydata2=scan()
mydatats2 = ts(mydata2, start = c(2019,1,4), frequency = 52)

#adjust for seasonality
autoplot(mydatats2)
ggseasonplot(mydatats2)
#sa_series2<-seasonal::seas(mydatats2)
#sa_series3<-seasonal::seas(mydatats2, x11 = "")
tsdata2<-stl(mydatats2, t.window = 13, s.window="periodic")
autoplot(tsdata2)
autoplot(tsdata2$time.series)
autoplot(tsdata2$time.series[,1])
stlsa2=mydatats2-tsdata2$time.series[,1]
ggtsdisplay(diff(mydatats2))
autoplot(cbind(mydatats2,stlsa2))
auto.arima(mydatats2, stepwise = T, approximation = F, trace = T)
q<-seasonal::seas(mydatats2, arima.model = c(0,1,0,0,1,1))
autoplot(cbind(mydatats2, sa_series2$series$s11, sa_series3$series$d11,stlsa2,q$series$s11))


#RJDemetra
dt= ts(df$bbl, start = c(2019,3,12), frequency = 52)
ggseasonplot(dt)
ggsubseriesplot(dt)
plot(stl(dt, s.window = "periodic"))

cleaning<-rjdhf::fractionalAirlineEstimation(df$bbl,periods = c(4.5, 52.18), outliers = c("ao","ls","wo"), criticalValue = 5)
c1<-rjdhf::fractionalAirlineDecomposition(cleaning$model$linearized, period=4.5)
c2<-rjdhf::fractionalAirlineDecomposition(c1$decomposition$sa, period=52.18)
sa<-xts(c2$decomposition$sa, order.by=as.Date(pull(df,'date')))
dygraph(cbind(sa,df$bbl))

#read csv
#rigs
df <- read_csv("rig.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
df$Year<-format(df$Date, format="%Y")
df$Month<-format(df$Date, format="%m")
#wti
dfa <- read_excel("RWTCm.xls", col_types = c("date", "numeric"))
dfa$Year<-format(dfa$Date, format="%Y")
dfa$Month<-format(dfa$Date, format="%m")

#filter fields to plot
df2 <- dfa %>% 
  filter(Year %in% c("2022","2021", "2019", "2018")) 
#plot
plot_ly(df2, x = ~Month, y = ~WTI, color = ~Year, 
        colors = c("red", "green", "blue", "black")) %>% 
  add_lines()

#forecast using holt winters
holttrend = holt(mydatats, h = 12)
plot(holttrend)
summary(holttrend)
plot(holt(mydatats, h = 35, damped = T,phi   = 0.8))
