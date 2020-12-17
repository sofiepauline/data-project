library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)


setwd("/home/sri041@ad.uit.no/BED-2056/data-project")


#car trafic data

trafikkdata<- read_excel("trafikkdata.xlsx")

trafikkdata<-trafikkdata%>%
  filter(Lane=="Totalt")%>%
  select("Date","Passanger_cars")

trafikkdata$Passanger_cars<-as.numeric(trafikkdata$Passanger_cars)


trafikkdata<- trafikkdata %>%
  mutate(dates2=ymd(Date),month=month(dates2),day=day(dates2))


trafikkdata<-trafikkdata%>%
  select( "month","day","Passanger_cars")%>%
  drop_na()

trafikkdata<-trafikkdata[-c(361), ]

#Had to use drop_na to remove NA in the dataframe between 08.06-10.06 and 25.8 in this data frame, and cut 3.12 from weatherdata

#weather data
weatherdata<-read_excel("weatherdata.xlsx")

weatherdata<-weatherdata[-c(159,160,161,237,365),]



#bicycledata

bicycledata<- read_excel("bicycledata.xlsx")

bicycledata<-bicycledata%>%
  filter(Felt=="Totalt")%>%
  select("Dato","Volum")


bicycledata<- bicycledata %>%
  mutate(dates2=ymd(Dato),month=month(dates2),day=day(dates2))

bicycledata<- bicycledata[-c(159,160,161,237,365),]

bicycledata<- bicycledata%>%
  select("month","day","Volum")








#make seasons

trafikkdata<-trafikkdata%>%
  mutate(Seasons= month)

trafikkdata$Seasons[1:90]="Winter"
trafikkdata$Seasons[91:151]="Spring" 
trafikkdata$Seasons[152:239]="Summer"
trafikkdata$Seasons[240:330]="Fall"
trafikkdata$Seasons[331:360]="Winter"


##linje 20

Linje_20_2019 <- read_excel("Linje 20 - 2019.xlsx")

Linje_20_2019<- Linje_20_2019%>%
  select("Brukte filtre:\nÅr er 2019\nLinje er 20", "...2", "...5")%>%
  slice(-c(1:2))

colnames(Linje_20_2019)<-c("Month", "Date", "Passanger20")
Linje_20_2019$Passanger20<-as.numeric(Linje_20_2019$Passanger20)

Linje_20_2019<- Linje_20_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger20) %>%
  ungroup()

colnames(Linje_20_2019)<-c("Month","Date","Passengers20")

Linje_20_2019 <- Linje_20_2019 %>% mutate(Month = dplyr::recode(Month,
                                                                "april"="April",
                                                                "desember"="December",
                                                                "januar"="January",
                                                                "februar"="February",
                                                                "mars"="March",
                                                                "april"="May",
                                                                "juni"="June",
                                                                "juli"="July",
                                                                "august"="August",
                                                                "september"="September",
                                                                "oktober"="October",
                                                                "november"="November"))

Linje_20_2019$Month<-match(Linje_20_2019$Month, month.name)

Linje_20_2019<- Linje_20_2019[-c(159,160,161,237,365),]

##linje 33
Linje_33_2019 <- read_excel("Linje 33 - 2019.xlsx")



Linje_33_2019<- Linje_33_2019%>%
  select("Brukte filtre:\r\nÅr er 2019\r\nLinje er 33", "...2", "...5")%>%
  slice(-c(1:2))

colnames(Linje_33_2019)<-c("Month", "Date", "Passanger33")
Linje_33_2019$Passanger33<-as.numeric(Linje_33_2019$Passanger33)

Linje_33_2019<- Linje_33_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger33) %>%
  ungroup()

colnames(Linje_33_2019)<-c("Month","Date","Passengers33")

Linje_33_2019 <- Linje_33_2019 %>% mutate(Month = dplyr::recode(Month,
                                                                "april"="April",
                                                                "desember"="December",
                                                                "januar"="January",
                                                                "februar"="February",
                                                                "mars"="March",
                                                                "april"="May",
                                                                "juni"="June",
                                                                "juli"="July",
                                                                "august"="August",
                                                                "september"="September",
                                                                "oktober"="October",
                                                                "november"="November"))

Linje_33_2019$Month<-match(Linje_33_2019$Month, month.name)
Linje_33_2019<- Linje_33_2019[-c(159,160,161,237,365),]


##linje 34
Linje_34_2019 <- read_excel("Linje 34 - 2019.xlsx")


Linje_34_2019<- Linje_34_2019%>%
  select("Brukte filtre:\r\nÅr er 2019\r\nLinje er 34", "...2", "...5")%>%
  slice(-c(1:2))

colnames(Linje_34_2019)<-c("Month", "Date", "Passanger34")
Linje_34_2019$Passanger34<-as.numeric(Linje_34_2019$Passanger34)

Linje_34_2019<- Linje_34_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger34) %>%
  ungroup()

colnames(Linje_34_2019)<-c("Month","Date","Passengers34")

Linje_34_2019 <- Linje_34_2019 %>% mutate(Month = dplyr::recode(Month,
                                                                "april"="April",
                                                                "desember"="December",
                                                                "januar"="January",
                                                                "februar"="February",
                                                                "mars"="March",
                                                                "april"="May",
                                                                "juni"="June",
                                                                "juli"="July",
                                                                "august"="August",
                                                                "september"="September",
                                                                "oktober"="October",
                                                                "november"="November"))

Linje_34_2019$Month<-match(Linje_34_2019$Month, month.name)

Linje_34_2019<- Linje_34_2019[-c(159,160,161,237,365),]

##linje 24

Linje_24_2019 <- read_excel("Linje 24 -2019.xlsx")

names(Linje_24_2019)

Linje_24_2019<- Linje_24_2019%>%
  select("Brukte filtre:\nÅr er 2019\nLinje er 24", "...2", "...5")%>%
  slice(-c(1:2))

colnames(Linje_24_2019)<-c("Month", "Date", "Passanger24")
Linje_24_2019$Passanger24<-as.numeric(Linje_24_2019$Passanger24)

Linje_24_2019<- Linje_24_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger24) %>%
  ungroup()

colnames(Linje_24_2019)<-c("Month","Date","Passengers24")

Linje_24_2019 <- Linje_24_2019 %>% mutate(Month = dplyr::recode(Month,
                                                                "april"="April",
                                                                "desember"="December",
                                                                "januar"="January",
                                                                "februar"="February",
                                                                "mars"="March",
                                                                "april"="May",
                                                                "juni"="June",
                                                                "juli"="July",
                                                                "august"="August",
                                                                "september"="September",
                                                                "oktober"="October",
                                                                "november"="November"))

Linje_24_2019$Month<-match(Linje_24_2019$Month, month.name)

Linje_24_2019<- Linje_24_2019[-c(159,160,161,237,365),]
Linje_24_2019<-arrange(Linje_24_2019,as.numeric(Linje_24_2019$Month))


#combind the data frames

df<-bind_cols(trafikkdata, weatherdata, bicycledata, Linje_20_2019, Linje_24_2019, Linje_33_2019, Linje_34_2019)
df<-df%>%
  select("month","day", "Temp", "Passanger_cars","Volum","Rainfall/Snowfall","Passengers20","Passengers24","Passengers33","Passengers34")

df<-arrange(df,as.numeric(df$month))
df <- df %>% mutate(month = dplyr::recode(month, "4"="April", "12"="December",
"1"="January","2"="February","3"="March","5"="May", "6"="June", "7"="July",
 "8"="August","9"="September","10"="October", "11"="November"))




colnames(df)<-c("Month","Date","Temp", "Passanger_cars","Cyclists","Rainfall/Snowfall","Passengers20","Passengers24","Passengers33","Passengers34")

#cut down number of passanger cars to fit with the other variables


wplot<-ggplot(df, aes(x=Date))+
  geom_line(aes(y=Temp), color="red")+
  geom_line(aes(y=`Rainfall/Snowfall`), color="green")+
  geom_line(aes(y=Passanger_cars), color="blue")+
  geom_line(aes(y=Cyclists), color="black")+
  geom_line(aes(y=Linje_20_2019$Passengers20), color="yellow")+
  geom_line(aes(y=Linje_24_2019$Passengers24), color="white")+
  geom_line(aes(y=Linje_33_2019$Passengers33), color="pink")+
  geom_line(aes(y=Linje_34_2019$Passengers34), color="orange")+
  facet_wrap(~Month)+
  theme_minimal() +
  xlab("Day of month")+
  ylab(" ")
  

  ggplotly(wplot, tooltip = c("Temp","Rainfall/Snowfall","Passanger_cars","Cyclists","linje20","linje24","linje33","linje34"))















