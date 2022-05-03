install.packages(c('readxl','ggplot2',"dplyr","plm"))
library('readxl','ggplot2',"dplyr",'plm')

df2 <- read_excel("results_aggregated.xlsx", sheet = "Sheet1", col_types = c("date","text", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric","text"))
head(df2)
df <- read_excel("oil sands variance backup.xlsx", sheet = "Sheet3", col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric","skip","skip","skip","skip","skip"))
head(df)
df3 = merge(df, df2, by = "Date", all.y = T)
names(df3)[8]<-"Prod"
df3=dplyr::select(df3,-9)
names(df3)[9]<-"CapEx"
names(df3)[10]<-"OpEx"
names(df3)[11]<-"Revenue"
names(df3)[12]<-"Royalty"
head(df3)

mydata<-df3[,c('WTI','LH','ARP','CapEx','Royalty','Revenue')]
round(cor(mydata),2)
plotmeans(Royalty~OSR, data=df3)

ggplot(df3, aes(x=ARP, y=Revenue, shape=Payout, color=Payout)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

df3.p <- pdata.frame(df3, index=c("OSR", "Date"))

fixed<-plm(log(Royalty+0.000001)~WTI+ARP, data=df3.p, model = "within") 
summary(fixed)

fd<-plm(log(Royalty+0.0001)~WTI+LH+CapEx+ARP, data=df3.p, model = "fd") 
summary(fd)
