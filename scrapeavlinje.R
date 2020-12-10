library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

##linje 20

Linje_20_2019 <- read_excel("Linje 20 - 2019.xlsx")

Linje_20_2019<- Linje_20_2019%>%
  select("Brukte filtre:\nÅr er 2019\nLinje er 20", "...2", "...5")%>%
  slice(-c(1:2))

colnames(Linje_20_2019)<-c("Month", "Date", "Passanger")
Linje_20_2019$Passanger<-as.numeric(Linje_20_2019$Passanger)

Linje_20.2019<- Linje_20_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger) 

colnames(Linje_20.2019)<-c("Month","Date","Passengers")
  


##linje 33
Linje_33_2019 <- read_excel("Linje 33 - 2019.xlsx")

Linje_33_2019<- Linje_33_2019%>%
  select("Brukte filtre:\r\nÅr er 2019\r\nLinje er 33", "...2", "...5")%>%
  slice(-c(1:2))

colnames(Linje_33_2019)<- c("Month", "Date", "Passanger")
Linje_33_2019$Passanger<-as.numeric(Linje_33_2019$Passanger)

Linje_33.2019<- Linje_33_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger) 

colnames(Linje_33.2019)<-c("Month","Date","Passengers")


##linje 34
Linje_34_2019 <- read_excel("Linje 34 - 2019.xlsx")


Linje_34_2019<- Linje_34_2019%>%
  select("Brukte filtre:\r\nÅr er 2019\r\nLinje er 34", "...2", "...5")%>%
  slice(-c(1:2))

names(Linje_34_2019)

colnames(Linje_34_2019)<- c("Month", "Date", "Passanger")
Linje_34_2019$Passanger<-as.numeric(Linje_34_2019$Passanger)

Linje_34.2019<- Linje_34_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger) 

colnames(Linje_34.2019)<-c("Month","Date","Passengers")


##linje 24

library(readxl)
Linje_24_2019 <- read_excel("Linje 24 -2019.xlsx")


names(Linje_24_2019)

Linje_24_2019<- Linje_24_2019%>%
  select("Brukte filtre:\nÅr er 2019\nLinje er 24", "...2", "...5")%>%
  slice(-c(1:2))


colnames(Linje_24_2019)<- c("Month", "Date", "Passanger")
Linje_24_2019$Passanger<-as.numeric(Linje_24_2019$Passanger)

Linje_24.2019<- Linje_24_2019 %>%
  group_by(Month,Date)%>%
  tally(Passanger)

colnames(Linje24)<-c("Month","Date","Passengers")



allelinjer<- rbind(linje20,linje24,linje33,linje34)%>%
  select("Month","Date","Passengers")%>%
  arrange(allelinjer,as.numeric(allelinjer$Date))
  


  

