

#install.packages("smooth","ggplot2","tidyselect","dplyr","zoo","fpp2","Rssa")

requiredPackages <- c("smooth","ggplot2","tidyselect","dplyr","zoo","fpp2","Rssa","kableExtra","scales","GeneCycle")
sapply(requiredPackages, library,character.only=TRUE) #install required packages
visitors<-read.csv(choose.files(),header = T)#imput data set
knitr::kable(table(is.na(visitors$Actual.Counts)))#detect missing values
knitr::kable(str(visitors))#show me the structure of dataset
plot(na.omit(visitors$Actual.Counts),type="l")#remove the missing values and plot the visitors arrival in NZ 

knitr::kable(as.array(summary(na.omit(visitors$Actual.Counts))))#summary of total visitors arrival in NZ
visitors<-ts(visitors$Count, start=c(1921,04,01), end=c(2019), frequency=12)#change the dataset into a time series
visitors$Date<-as.Date(visitors$Date)
visitors$Count<-as.integer(visitors$Count)
visitors<-visitors %>% select(Date,Count)
train.split<-window(visitors,end=c(2017,1))#take observations from 1921 to 2017 as a training set

#Visualzing the time series
autoplot(train.split,ylab="Arrival Visitors in NZ")+labs(x="Time")
autoplot(second.split,ylab="")+labs(x="Time")+geom_vline(xintercept=c(2000,2001),col="red")


