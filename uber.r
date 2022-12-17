library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)
apr=read.csv("uber-raw-data-apr14.csv")
may=read.csv("uber-raw-data-may14.csv")
udata=rbind(apr,may)


head(udata)
str(udata)
summary(udata)

udata$Date.Time=as.POSIXct(udata$Date.Time,format="%m/%d/%Y %H:%M:%S")
str(udata)
#format(Sys.tdayzime(), "%a %b %d %X %Y %Z")
udata$time=format(as.POSIXct(udata$Date.Time,format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")
udata$Date.Time=ymd_hms(udata$Date.Time)
udata$Day=format(day(udata$Date.Time))
udata$month=format(month(udata$Date.Time,label=TRUE))
udata$year=format(year(udata$Date.Time))
udata$dayoftheweek=format(wday(udata$Date.Time,label=TRUE))
udata$hour=factor(hour(hms(udata$time)))
udata$minute=factor(minute(hms(udata$time)))
udata$second=factor(second(hms(udata$time)))
head(udata)


#visualisation of the data
#plotting the trip by hours in a day

hour_data=udata %>%
  group_by(hour) %>%
  summarise(Total= n())

datatable(hour_data)
#visualizing the data
 ggplot(hour_data,aes(hour,Total)) +
   geom_bar(stat="identity",fill="black",color="blue") +
   ggtitle("trips by hour") +
   theme(legend.position = "none") +
   scale_y_continuous(labels = waiver())
 
month_hour_data=udata %>%
   group_by(month,hour) %>%
   summarise(Total= n())
 
datatable(month_hour_data)

ggplot(month_hour_data,aes(hour,Total,fill=month)) +
  geom_bar(stat="identity") +
  ggtitle("trips by hour and month") 
  
  april_hour=udata %>%
    group_by(hour,month) %>%
    filter(month=="Apr") %>%
    summarise(Total =n())
  datatable(april_hour)
  
  
  ggplot(april_hour,aes(hour,Total,fill=hour)) +
    geom_bar(stat="identity") +
    ggtitle("trips by hour and month in april") 

  
 may_hour=udata %>%
    group_by(hour,month) %>%
    filter(month=="May") %>%
    summarise(Total =n())
  
  ggplot(may_hour,aes(hour,Total,fill=hour)) +
    geom_bar(stat="identity") +
    ggtitle("trips by hour and month in may") +
    scale_y_continuous(labels = waiver())
   
  day_data=udata %>%
    group_by(Day) %>%
    summarise(Total=n())
  
  datatable(day_data)
  
  ggplot(day_data,aes(Day,Total)) +
    geom_bar(stat="identity",fill="black",color="blue") +
    ggtitle("trips by day") +
    theme(legend.position = "none") +
    scale_y_continuous(labels = waiver())
  
  
  month_day_data=udata %>%
    group_by(month,Day) %>%
    summarise(Total= n())
  
  datatable(month_hour_data)
  
  ggplot(month_day_data,aes(Day,Total,fill=month)) +
    geom_bar(stat="identity") +
    ggtitle("trips by day and month") 
  
  april_day=udata %>%
    group_by(Day,month) %>%
    filter(month=="Apr") %>%
    summarise(Total =n())
  datatable(april_hour)
  
  
  ggplot(april_day,aes(Day,Total,fill=Day)) +
    geom_bar(stat="identity") +
    ggtitle("trips by day and month in april") 
  
  month_data=udata %>%
    group_by(month) %>%
    summarise(Total=n())
  datatable(month_data)
  
 
  month_weekday_data=udata %>%
    group_by(month,dayoftheweek) %>%
    summarise(Total=n())
  datatable(month_weekday_data)
  
  ggplot(month_weekday_data,aes(month,Total,fill=dayoftheweek)) +
    geom_bar(stat="identity") +
    ggtitle("trips by day of the week and month ") 
   
  
  weekday_data=udata %>%
    group_by(dayoftheweek) %>%
    summarise(Total=n())
  datatable(weekday_data)
  
  ggplot(month_weekday_data,aes(dayoftheweek,Total)) +
    geom_bar(stat="identity") +
    ggtitle("trips by day of the week ") 
  
  
  
  ggplot(udata,aes(Base))+
    geom_bar(fill="lightgreen") +
    ggtitle("trips by bases") +
  
    scale_y_continuous(labels = waiver())
  
  
  ggplot(udata,aes(Base,fill=month))+
    geom_bar(position="dodge") +
    ggtitle("trips by bases and months") +
    
    scale_y_continuous(labels = waiver())
  
  
  ggplot(udata,aes(Base,fill=dayoftheweek))+
    geom_bar(position="dodge") +
    ggtitle("trips by bases and day of the week") +
    
    scale_y_continuous(labels = waiver())
  
  dayhour=udata %>%
    group_by(Day,hour)%>%
    dplyr::summarize(Total=n())
  datatable(dayhour)
  
  
  
  ggplot(dayhour,aes(Day,hour,fill=Total))+
    geom_tile(color="white") +
    ggtitle("Heatmap by hour and day") 
    
  min_lat=40.07
  max_lat=42.12
  min_lon=-74.93
  max_lon=-72.07
  
  ggplot(udata,aes(x=lon,y=lat,color=Base))+
    geom_point(size=1) +
    scale_x_continuous(limits=c(min_lon,max_lon)) +
  scale_y_continuous(limits=c(min_lat,max_lat)) +
    theme_map() +
    ggtitle("NYC(lat-long chart) based on uberrides during 2014 (apr-sep) by base")
  
  
  
  
  
  
  