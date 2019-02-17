#########################################################################################################################
##### RFID DATA OF YELKOUAN SHEARWATER IN MALTA ####################################
#########################################################################################################################
### original code provided by Martin Austad
### updated by steffen.oppel@rspb.org.uk on 8 Aug 2017

### updated on 24 Aug 2017 to insert n ind moving per night based on email by Martin Austad
### also added number of tagged individuals overall, as this increased during the season


### CONCERN THAT ONLY IND IN FIRST PART OF NIGHT ARE COUNTED - manual count gives higher number [noted by Martin on 4 Sept 2017]
### fixed on 4 Sept 2017 by swapping '+' to '-' sign


### just list detections at 1 antenna 
## list n individuals at antenna 1 and 2 regardless of whether detected at both 

##updated by MA for use on 2018 data on 08022019 and 11022019

library(data.table)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(lubridate)
library(oce)
library(maptools)
library(sp)
library(fasttime)

################################################################################################################
###################### YESH COLONY RFID DATA FROM MALTA   ######################################################
################################################################################################################

#setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\RFID")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\RFID")
setwd("C:\\Users\\Martin\\Documents\\working_folder\\RFID\\2018\\Analysis")

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

#raw <- fread("RFID_Eggshellcave_22032017_27072017.txt",fill=TRUE)
raw <- fread("RFID2018.csv", header = TRUE)
#raw$DateTime<-fastPOSIXct(paste(raw$Date, raw$Time, " "),tz="GMT",required.components=6L)		### only works if string is in yyyy/mm/dd order
raw <- raw %>%
  mutate(Time=trim.trailing(Time)) %>%
  mutate(DateTime=as.POSIXct(paste(raw$Date, raw$Time, " "), format="%d/%m/%Y %H:%M:%S", tz="UTC"))

head(raw)
raw[is.na(raw$DateTime),]


## read in deployments
deployments<-fread("PITdeployments2018raw.csv")

locs<-fread("Location_coordinates_SQM_RFID_2017.csv")
locs<-SpatialPoints(locs[,c(8,7)], proj4string=CRS("+proj=longlat +datum=WGS84"))



################################################################################################################
###################### MANIPULATE DATA AND DELETE UNNECESSARY ROWS   ###########################################
################################################################################################################
######To Do for 2018 deployment data: 
#######Remove birds tagged in 2017 but not registered in 2018. Checked manually and have NA under TAGID2018(lost tag or did not return to EC in 2018)
#######check for birds with very few registrations, f.eg. 8000E1349EA964F2 (only 1 registration); or not at all 8000E1349EA964CB (broken pit tag?)

#######one bird with two tags during 2018 due to replacement of broken ring


#change name of first column
colnames(raw)[1] <- 'Detectiontype'

#old data with dates from 2017 breeding season seem to pop up in data even after format. create interval with wanted dates

recordingperiod <-interval(ymd("2017-10-10", tz="UTC"),ymd("2018-07-20", tz = "UTC"))

RFIDdata <- raw %>%
  filter(Detectiontype == "D") %>%
  filter(Type!="HW") %>%
  #filter(TagID %in% unique(deployments$TagID)) %>%
  #filter(DateTime > "2017-10-01 00:00:00")
  filter(DateTime %within% recordingperiod) %>%
select(DateTime,Detectiontype,Duration,Type,TagID,Ant,Count,Gap) %>%
  #mutate(Ant=as.factor(Ant))%>%                                            ### this is not useful, character matching is easier without factor
  mutate(Gap=as.numeric(Gap))%>%
  arrange(DateTime)
head(RFIDdata)
tail(RFIDdata)

###check how many of 2017 pit tags registered in 2018 recording period
RFIDcheck <- RFIDdata %>%
  #filter(Detectiontype == "D") %>%
  #filter(TagType!="HW") %>%
  filter(TagID %in% unique(deployments$TagID2017)) %>%
  #filter(DateTime > "2017-10-01 00:00:00")
  select(DateTime,Detectiontype,Duration,Type,TagID,Ant,Count,Gap) %>%
  #mutate(Ant=as.factor(Ant))%>%                                            ### this is not useful, character matching is easier without factor
  #mutate(Gap=as.numeric(Gap))%>%
  arrange(DateTime)
head(RFIDdata)
tail(RFIDdata)
#ggplot(RFIDdata, aes(x=Gap))+geom_histogram()

Tagcheck <- (unique(RFIDcheck$TagID))
length(Tagcheck) #number of birds pit tagged 2017 and registed in 2018: N=70 (all adults)


#EE02220 had its pit tag replaced from TagID2017 8000E1349EA966E4 to TagID2018 8000E1349EA964FD but both were registed in 2018
## replace all 8000E1349EA966E4 with 8000E1349EA964FD in 2018 data:

for(i in 1:nrow(RFIDdata)){
  if(RFIDdata$TagID[i]=="8000E1349EA966E4"){
    RFIDdata$TagID[i] <- "8000E1349EA964FD"
  }
}

deployments <- deployments %>%
  select(TagID2018,PRn2018,Ring,Deploydate,Age,Sex) %>%
  filter(TagID2018!="NA")%>%  #16 birds pit tagged birds in 2017 not registered in 2018 and not handled
  filter (Age== "4") %>%
  mutate(Deploydate=dmy(Deploydate, tz="Europe/Berlin")) %>%
  arrange(Deploydate)

n_deployed<- deployments %>%
  group_by(Deploydate) %>%
  summarise(n=length(unique(TagID2018)))

taggedby<-function(x){sum(n_deployed$n[n_deployed$Deploydate<x])}


length(unique(deployments$TagID2018))
##filter out tags that are RFID reading errors 

RFIDdata <- RFIDdata %>%
  #filter(Detectiontype == "D") %>%
  #filter(Type!="HW") %>%
  filter(TagID %in% unique(deployments$TagID2018)) %>%
  #filter(DateTime > "2017-10-01 00:00:00")
  #filter(DateTime %within% recordingperiod) %>%
  select(DateTime,Detectiontype,Duration,Type,TagID,Ant,Count,Gap) %>%
  #mutate(Ant=as.factor(Ant))%>%                                            ### this is not useful, character matching is easier without factor
  filter(Ant!= "A0") %>% #appears when testing RFID
  mutate(Gap=as.numeric(Gap))%>%
  arrange(DateTime)
head(RFIDdata)
tail(RFIDdata)

length(unique(RFIDdata$TagID)) #97 cause 8000E1349EA964CB was never recorded - pit tag not working?
length(RFIDdata$TagID)
fwrite(RFIDdata,"RFID2018_cleaned.csv")

################################################################################################################
###### RFID operating nights from marker tag  ##############################
##################################################################################################

RFIDoper <- raw %>%
  filter(Detectiontype == "D") %>%
  filter(Type=="HW") %>%
  #filter(TagID %in% unique(deployments$TagID)) %>%
  #filter(DateTime > "2017-10-01 00:00:00")
  filter(DateTime %within% recordingperiod) %>%
  select(DateTime,Detectiontype,Duration,Type,TagID,Ant,Count,Gap) %>%
  #mutate(Ant=as.factor(Ant))%>%                                            ### this is not useful, character matching is easier without factor
  mutate(Gap=as.numeric(Gap))%>%
  arrange(DateTime)
head(RFIDoper)
tail(RFIDoper)

midday<-as.POSIXct("12:00:00", format="%H:%M:%S")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S")

opernights<- RFIDoper %>%
  mutate(det=1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting, Ant) %>%                              
  summarise(n_detections=sum(det)) %>%
  spread(key=Ant, value=n_detections) %>%
  arrange(NightStarting)

head(opernights)
tail(opernights)

length(opernights$NightStarting)
plot(opernights$NightStarting, opernights$A2)
  

##Done: merge marker tag data with number of HA tag detections per Ant - are nights were marker tag was not registered in one of the two Ant to be removed? -cause only one Ant working? alternatively marker is only registered in one Ant on these nights cause of being moved out of correct position


################################################################################################################
###### SUMMARISE THE NUMBER OF INDIVIDUALS DETECTED AT EACH ANTENNA IN EACH NIGHT  ##############################
################################################################################################################


#### SUMMARISE THE MOVEMENTS FOR EACH NIGHT ACROSS ALL INDIVIDUALS ###

#midday<-as.POSIXct("13:00:00", format="%H:%M:%S")   # create a reference time to split the day
#midday<-format(midday, format="%H:%M:%S")

NIGHTDETECTIONS<- RFIDdata %>%
  mutate(det=1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting,Ant,TagID) %>%                              
  summarise(n_detections=sum(det)) %>%
  spread(key=Ant, value=n_detections) %>%
  arrange(NightStarting)
#fwrite(NIGHTDETECTIONS,"YESH_nocturnal_detections_per_antenna.csv")

length(unique(NIGHTDETECTIONS$TagID))


#####TO DO: represent ind detections graphically but not needed for bunkering analysis
pdf("YESH_IND_movements_per_night.pdf", width=8, height=1000)
NIGHTDETECTIONS %>% mutate(count=1) %>%
  group_by(TagID,NightStarting) %>%
  summarise(n=sum(count))%>%
ggplot(aes(x=NightStarting, y= n ))+    
  geom_point(size=0.5, colour = "red")+                                    
  facet_wrap("TagID", ncol=1, scales = "free")+
  ylab("detections") +
  #xlab("Date") +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(limits = c(0, 2))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

dev.off()


       
NIGHTINDIVIDUALS<- NIGHTDETECTIONS %>%
  gather(key=Ant, value=n_detections, A1,A2,na.rm = T) %>%
  mutate(n_detections=1) %>%
  group_by(NightStarting,Ant) %>%
  summarise(N_individuals=sum(n_detections)) %>%
  spread(key=Ant, value=N_individuals) %>%
  arrange(NightStarting)
fwrite(NIGHTINDIVIDUALS,"YESH_nocturnal_individuals_per_antenna.csv")

###################################################################################
##########merge marker tag data with number of HA tag detections per Ant ##########
########## are nights were marker tag was not registered in one of the two Ant ############
###########to be removed? -cause only one Ant working? ##########
############alternatively marker is only registered in one Ant on these nights cause of being moved out of correct position

opernights_detectionnights <- merge(opernights, NIGHTINDIVIDUALS, by = "NightStarting", all.x = TRUE)
fwrite(opernights_detectionnights, "operational_nights_20172018.csv")

#clearly there was a problem with A1 between 21.11.2017 and 09.12.2017. for now analysis will consider data from 10.12.2018. 


####RE-Filter RFID data

RFIDdata<- RFIDdata %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))

recordingperiod2 <-interval(ymd("2017-12-10", tz="UTC"),ymd("2018-07-20", tz = "UTC"))

RFIDdata <- RFIDdata %>%
  filter(Detectiontype == "D") %>%
  filter(Type!="HW") %>%
  #filter(TagID %in% unique(deployments$TagID)) %>%
  #filter(DateTime > "2017-10-01 00:00:00")
  filter(NightStarting %within% recordingperiod2) %>%
  select(DateTime,NightStarting,Detectiontype,Duration,Type,TagID,Ant,Count,Gap) %>%
  #mutate(Ant=as.factor(Ant))%>%                                            ### this is not useful, character matching is easier without factor
  mutate(Gap=as.numeric(Gap))%>%
  arrange(DateTime)
head(RFIDdata)
tail(RFIDdata)

length(unique(RFIDdata$TagID))

opernights_sel <- opernights  %>%
filter(NightStarting %within% recordingperiod2)

length(opernights_sel$NightStarting) #some missing or non-functional nights within recordingperiod2; all good from 14/02/2018
min(opernights_sel$NightStarting)
max(opernights_sel$NightStarting)


#RFIDdata[is.na(RFIDdata$DateTime),] #no missing time values

### remove pit tag tests before actual deployment

dim(RFIDdata)
### LOOP OVER EACH ANIMAL ###
YESH<-unique(RFIDdata$TagID)

for(i in 1:nrow(RFIDdata)){
  for(j in 1:nrow(deployments)){
    if(RFIDdata$TagID[i]==deployments$TagID2018[j]){
      RFIDdata$deploydate[i] <- deployments$Deploydate[j]
    }
  }
}

RFIDdata$deploydate <- as.POSIXct(RFIDdata$deploydate, origin = "1970-01-01 UTC")

for (A in YESH){
  RFIDdata <- RFIDdata %>%
    filter(!(DateTime<=deploydate))
}
dim(RFIDdata)
###works only partially cause some tags were tested (to get pit tag number) on the same same day they were deployed

###REMOVE RECORDS BEFORE AND AFTER SUNRISE/SUNSET OF EACH DAY - due to testing pit tags before deployment

RFIDdata <- RFIDdata %>%
  group_by(NightStarting) %>%
  #summarise(firstdet=min(DateTime), lastdet=max(DateTime))%>%
  mutate(NightEnding=NightStarting+days(1))%>%
  mutate(sunset=sunriset(locs[1,], as.POSIXct(NightStarting, tz="UTC"),direction=c("sunset"), POSIXct.out=T)[,2])%>%
  mutate(sunrise=sunriset(locs[1,], as.POSIXct(NightEnding, tz="UTC"),direction=c("sunrise"), POSIXct.out=T)[,2])%>%
  select(DateTime,NightStarting,Detectiontype,Duration,Type,TagID,Ant,Count,Gap,sunset,sunrise)%>%
  arrange(DateTime)

head(RFIDdata)
dim(RFIDdata)

for (A in YESH){
  RFIDdata <- RFIDdata %>%
    group_by(NightStarting)%>%
    filter(!(DateTime<=sunset))
}
length(unique(RFIDdata$TagID))
################################################################################################################
###################### LOAD SHIP BUNKERING FROM MALTA   ######################################################
################################################################################################################



ships <- fread("Malta_ship_bunkering2018.csv",fill=TRUE) #problem due to long remarks, remarks column deleted in csv file
ships <- ships %>%
  filter(BerthInMalta=="AREA 6") %>%
  filter(PurposeOfCall=="BUNKERS") %>%
  mutate(Arrival=dmy_hm(paste(ArrivalDate, ArrivalTime,sep=" "),tz = "Europe/Berlin")) %>% #changed to Europe/Berlin time from GMT, now conversion is done to UTC with_tz function
  mutate(Departure=dmy_hm(paste(DepartureDate, DepartureTime,sep=" "), tz= "Europe/Berlin")) %>%
  mutate(Arrival=with_tz(Arrival,tz = "UTC")) %>%
  mutate(Departure=with_tz(Departure,tz="UTC")) %>%
  mutate(duration=difftime(Departure,Arrival,units='hours')) %>%
  mutate(presence=interval(Arrival,Departure))%>%
  select(VesselName,Arrival,Departure,duration,presence)


head(ships)
ships %>% filter(is.na(Departure))

################################################################################################################
###### SPECIFY THE NIGHT TO AVOID GROUPING ISSUES AFTER MIDNIGHT  ##############################
################################################################################################################

#### CALCULATE LENGTH OF EACH MONITORED NIGHT #####
head(RFIDdata)

RFIDoper<- RFIDoper %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))

monitornights<- RFIDoper %>% #monitoring nights should not be based on RFIDdata - nights when no birds are recorded are not included
  group_by(NightStarting) %>%
  filter(NightStarting %within% recordingperiod2) %>%
  summarise(firstdet=min(DateTime), lastdet=max(DateTime))%>%
  mutate(NightEnding=NightStarting+days(1))%>%
  mutate(sunset=sunriset(locs[1,], as.POSIXct(NightStarting, tz="UTC"),direction=c("sunset"), POSIXct.out=T)[,2])%>%
  mutate(sunrise=sunriset(locs[1,], as.POSIXct(NightEnding, tz="UTC"),direction=c("sunrise"), POSIXct.out=T)[,2])%>%
  mutate(nightdur=interval(sunset,sunrise))%>%
  mutate(ships=0)

#### COUNT THE NUMBER OF SHIPS IN EACH MONITORED NIGHT ###

for (n in 1:length(monitornights$sunrise)){
  x<-monitornights$nightdur[n]
  shipsthisnight<-int_overlaps(ships$presence,x)
  monitornights$ships[n]<- sum(shipsthisnight, na.rm=T)
}

################################################################################################################
###### LOAD AND MANIPULATE THE SKY QUALITY METER DATA   ##############################
################################################################################################################
#setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\LightPollution")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\LightPollution")

sqm<-fread("MT24_SQM_nov2017_july2018.csv")
names(sqm)[7]<-"MAG"

## need to remove all daytime measurements and ACCount for moon phase

SQM<-sqm %>%
  mutate(DateTime=dmy_hms(paste(date_UTC, time_UTC,sep=" "),tz = "UTC")) %>% 
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%
  #filter(volts>4.5) %>%     ## remove locs when battery was dead
  mutate(sunset=crepuscule(locs[3,], DateTime,direction=c("dusk"), solarDep=18, POSIXct.out=T)[,2])%>%
  mutate(sunrise=crepuscule(locs[3,], DateTime,direction=c("dawn"), solarDep=18, POSIXct.out=T)[,2])%>%
  mutate(duringday=if_else(Time>midday,if_else(DateTime<sunset,1,0),if_else(DateTime>sunrise,1,0)))%>%
  filter(duringday==0) %>%
  filter(NightStarting %within% recordingperiod2) %>%
  filter(!(NightStarting=="2018-05-15")) %>%
  filter(!(NightStarting=="2018-05-16"))%>% #stones were covering the SQM on two nights 15 & 16 May, much darker than all other nights
  mutate(prop.illuminated=moonAngle(t=DateTime, longitude=coordinates(locs)[3,1], latitude=coordinates(locs)[3,2])$illuminatedFraction)%>%### This is how 'full the moon is - 1 is full moon, 0 is new moon
  mutate(moon.elevation=moonAngle(t=DateTime, longitude=coordinates(locs)[3,1], latitude=coordinates(locs)[3,2])$altitude) %>%
  mutate(moon.light=prop.illuminated*(moon.elevation/coordinates(locs)[3,2]))


#sqmfault <- sqm %>%
  #filter(MAG>=22.05)
#fwrite(SQM, "SQM_night.csv")
#SQM <- SQM %>%
#filter (!(NightStarting>="2017/07/24")) 


head(SQM)
tail(SQM)

length(unique(SQM$NightStarting))

################################################################################################################
###### QUESTION 1: DOES SHIP BUNKERING AFFECT SKY QUALITY METER DATA?   ##############################
################################################################################################################


### plot variation in darkness with ship bunkering events ###
## PLOT 1 - OVER TIME
shipnights<-monitornights 
shipnights <- subset.data.frame(shipnights, shipnights$ships>0) 
##originally: 
##shipnights<-monitornights %>% 
## filter(ships>0) # %>%
## select(sunset,ships) ## but gave following error Error in filter_impl(.data, quo) : 
## Column `nightdur` classes Period and Interval from lubridate are currently not supported.

shipnights$sunset <- as.numeric(as.POSIXct(shipnights$sunset)) #changed to as.numeric - graph was not complete 
#fwrite(shipnights,"Malta_bunkering_ships_2017.csv")

#pdf("SQM_season_ships.pdf", width=8, height=6)
SQM %>%
  ggplot(aes(x=DateTime, y=MAG))+geom_point(colour="black", size=1.5) +
  geom_vline(aes(xintercept=shipnights$sunset[1]), color='red', size=shipnights$ships[1]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[2]), color='red', size=shipnights$ships[2]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[3]), color='red', size=shipnights$ships[3]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[4]), color='red', size=shipnights$ships[4]/10) + #this bunkering night is in a period without SQM data
  geom_vline(aes(xintercept=shipnights$sunset[5]), color='red', size=shipnights$ships[5]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[6]), color='red', size=shipnights$ships[6]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[7]), color='red', size=shipnights$ships[7]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[8]), color='red', size=shipnights$ships[8]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[9]), color='red', size=shipnights$ships[9]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[10]), color='red', size=shipnights$ships[10]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[11]), color='red', size=shipnights$ships[11]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[12]), color='red', size=shipnights$ships[12]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[13]), color='red', size=shipnights$ships[13]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[14]), color='red', size=shipnights$ships[14]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[15]), color='red', size=shipnights$ships[15]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[16]), color='red', size=shipnights$ships[16]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[17]), color='red', size=shipnights$ships[17]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[18]), color='red', size=shipnights$ships[18]/10) +
  geom_vline(aes(xintercept=shipnights$sunset[19]), color='red', size=shipnights$ships[19]/10) +
  xlab("Date") +
  ylab("Darkness of cliff face") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.background = element_rect(colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())


#dev.off()

## PLOT 2 - Darkness against moon - illumination
## need to make symbol size and colour reflect number of bunkering ships
SQM$ships<-0
x<-SQM$DateTime[1]-minutes(10)
x1<-SQM$DateTime[1]
sqmint<-interval(x,x1)
shipsthisinterval<-int_overlaps(ships$presence,sqmint)
SQM$ships[1]<- sum(shipsthisinterval, na.rm=T)

for (n in 2:length(SQM$DateTime)){
  x<-SQM$DateTime[n-1]
  x1<-SQM$DateTime[n]
  sqmint<-interval(x,x1)
  shipsthisinterval<-int_overlaps(ships$presence,sqmint)
  SQM$ships[n]<- sum(shipsthisinterval, na.rm=T)
}

#pdf("SQM_moonlight_ships.pdf", width=8, height=6)
SQM %>% mutate(ships=ships*2) %>%   ### usually 2 boats operate for a bunkering event
  ggplot(aes(x=moon.light, y=MAG))+
  geom_point(aes(size=ships, color=ships)) +
  scale_color_gradient(low="black", high="yellow")+
  scale_size(range=c(0,4)) +
  guides(color=guide_legend(title="Number of ships"), size = guide_legend(title="Number of ships"))+
  geom_vline(aes(xintercept=0), color='blue', size=1) +
  annotate("text", x=1, y=22, label= "moon in the sky", size=5) + 
  annotate("text", x = -0.5, y=22, label = "before moonrise", size=5)+
  xlab("level of moon light in the sky") +
  ylab("SQM measured darkness of cliff face") +
  
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.background = element_rect(fill="white", colour="white"),
        legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#dev.off()



#########################################################
#### STATISTICAL TEST OF BUNKERING EFFECT ON DARKNESS ###
head(SQM)
SQM$bunker<-ifelse(SQM$ships>0,1,0)
hist(SQM$MAG)
Q1m<-glm(MAG~moon.light*bunker, data=SQM)
outQ1m<-summary(Q1m)
outQ1m$coefficients

################################################################################################################
###### PLOT ACTIVITY PATTERN OVER SEASON AND OVER NIGHTTIME  ##############################
################################################################################################################
season<- RFIDdata %>%
  filter(Ant=="A1") %>%
  mutate(count=1) %>%
  group_by(NightStarting,TagID) %>%
  summarise(activity=sum(count))

season %>% group_by(NightStarting) %>%
  summarise(activity=length(unique(TagID))) %>%
  mutate(n_deployed=sapply(NightStarting,taggedby)) %>%
  mutate(prop_moving=activity/n_deployed)%>%
  
  
  ggplot(aes(x=NightStarting, y=prop_moving))+
  geom_point(colour="black", size=1.5) +
  xlab("Date") +
  ylab("Prop of tagged individuals at A1") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=15, color="black"), 
        axis.title=element_text(size=15), 
        strip.text.x=element_text(size=15, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.background = element_rect(colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

head(RFIDdata)
RFIDdata <- as.data.frame(RFIDdata)


nighttime<- RFIDdata %>%
  filter(Ant=="A1") %>%
  mutate(count=1) %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  group_by(HR,TagID) %>%
  summarise(activity=sum(count))
  
  nighttime %>%
  ggplot(aes(x=HR, y=activity, width=1))+
  geom_boxplot(colour="black") +
  xlab("hour of the day") +
  ylab("N detections at A1") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=15, color="black"), 
        axis.title=element_text(size=15), 
        strip.text.x=element_text(size=15, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.background = element_rect(colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())





recordingperiod3 <-interval(ymd("2018-02-14", tz="UTC"),ymd("2018-06-30", tz = "UTC"))

RFIDdata_all <-RFIDdata
#RFIDdata <-RFIDdata_all

#goodinterval<-interval(ymd("2018-04-01", tz= "UTC"),ymd("2018-05-31", tz = "UTC"))
RFIDdata<- RFIDdata %>%
  filter(DateTime %within% recordingperiod3)


SQM_all <-SQM
#SQM<-SQM_all
SQM<-SQM %>%
  filter(DateTime %within% recordingperiod3)


################################################################################################################
###### Moves for all period   ##############################
##########################################################################################
YESH_all<-unique(RFIDdata_all$TagID)

MOVES_all<-data.frame()

for (A in YESH_all){
  IND <- RFIDdata_all %>%
    filter(TagID == A) %>%
    arrange(DateTime) %>%
    
    #nextAntV<-RFIDdata$Ant[RFIDdata$TagID==A]
    
    mutate(nextAnt=c(Ant[-1],NA)) %>%
    mutate(nextTime=c(DateTime[-1],NA)) %>% ### insert column for date at next antenna to compare time  
    mutate(move=ifelse(Ant==nextAnt,0,1)) %>%
    mutate(direction=ifelse(move==1,ifelse(nextAnt=="A2","IN","OUT"),"STAY")) %>%
    mutate(timediff=difftime(nextTime,DateTime, units="secs")) %>% ### calculate time between two registrations
    mutate(DateTime=if_else(timediff>3600,nextTime,DateTime))%>% ### insert condition that if time between A1 and A2 is > 5min then take the date for the next registration
    mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
    mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))
  
  MOVES_all<-bind_rows(MOVES_all,IND)
  
}
sum(MOVES_all$move, na.rm=TRUE)





################################################################################################################
###### DETERMINE ARRIVAL AND DEPARTURE FROM SEQUENTAL REGISTRATION AT A1 and A2   ##############################
################################################################################################################


### LOOP OVER EACH ANIMAL ###
YESH<-unique(RFIDdata$TagID)

### IN for A1 followed by A2 for same tag

MOVES<-data.frame()

for (A in YESH){
  IND <- RFIDdata %>%
    filter(TagID == A) %>%
    arrange(DateTime) %>%
    
    #nextAntV<-RFIDdata$Ant[RFIDdata$TagID==A]
    
    mutate(nextAnt=c(Ant[-1],NA)) %>%
    mutate(nextTime=c(DateTime[-1],NA)) %>% ### insert column for date at next antenna to compare time  
    mutate(move=ifelse(Ant==nextAnt,0,1)) %>%
    mutate(direction=ifelse(move==1,ifelse(nextAnt=="A2","IN","OUT"),"STAY")) %>%
    mutate(timediff=difftime(nextTime,DateTime, units="secs")) %>% ### calculate time between two registrations
    mutate(DateTime=if_else(timediff>3600,nextTime,DateTime))%>% ### insert condition that if time between A1 and A2 is > 5min then take the date for the next registration
    mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
    mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))
  
  MOVES<-bind_rows(MOVES,IND)
  
}

head(MOVES)
write.csv(MOVES, "Moves.csv")
sum(MOVES$move, na.rm=TRUE)

#at this point RFIDdata times are still correct but not sure if MOVES time are correct

################################################################################################################
###### QUESTION 2: ARE NUMBER OF MOVEMENTS RELATED TO SQM LIGHT LEVEL MEASUREMENTS? ###########
################################################################################################################


### summarise HOURLY light-level measurements
head(SQM)

SQMhr<- SQM %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  mutate(MONTH=as.factor(month(DateTime)))%>% #, label=T, abbr=T, used as.factor instead
  group_by(MONTH,NightStarting,HR) %>%
  summarise(MAG=mean(MAG),prop.illuminated=mean(prop.illuminated), moon.elevation=mean(moon.elevation), ships=max(ships), bunker=max(bunker))%>%
  mutate(moon.light=prop.illuminated*(moon.elevation/coordinates(locs)[3,2]))

MOVEShr <- MOVES %>%
  filter(direction=="IN") %>%
  mutate(count=1) %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  mutate(MONTH=as.factor(month(DateTime)))%>% #, label=T, abbr=T, used as.factor instead
  group_by(MONTH,NightStarting,HR) %>%
  summarise(activity=length(unique(TagID)))


MOVE_SQM<-merge(SQMhr,MOVEShr,by=c('MONTH','NightStarting','HR'), all.x=T)
MOVE_SQM$activity[is.na(MOVE_SQM$activity)]<-0


MOVE_SQM$HRordered <- factor(MOVE_SQM$HR, levels = c("19","20","21","22","23","0","1","2","3"))
levels(MOVE_SQM$HR)
MOVE_SQM %>% #filter(HR %in% c(22,23,0,1))%>%
  ggplot(aes(x=MAG, y=activity))+
  geom_point(colour="black") +
  ylab("N individuals entering the cave") +
  xlab("Darkness of cliff face (magnitude measured by SQM)") +
  #geom_boxplot(colour="black") +
  #facet_wrap('HRordered',scales = "fixed", shrink = TRUE, ncol=2)+
  geom_smooth(fill="lightblue", size=1.5, method='lm', se=T)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=15, color="black"), 
        axis.title=element_text(size=15),
        axis.title.y=element_text(margin=margin(0,15,0,0)),
        axis.title.x=element_text(margin=margin(15,0,0,0)), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#########################################################
#### STATISTICAL TEST OF BUNKERING EFFECT ON DARKNESS ###

Q2m<-glm(activity~-1+HR+MONTH+MAG, data=MOVE_SQM[MOVE_SQM$HR %in% c("19", "20", "21", "22","23","0","1","2","3"),], family=poisson)
outQ2m<-summary(Q2m)
outQ2m$coefficients #not significant for April May but TO DO REPEAT APRIL MAY TEST AFTER IMPROVED FILTERING OF TESTING PITS

################################################################################################################
###### QUANTIFY REGISTRATIONS AT A1 FOR EACH 10 MIN INTERVAL OF SQM   ##############################
################################################################################################################
head(SQM)
head(RFIDdata)

SQM$A1det<-0
SQM$A1ind<-0

for (n in 2:length(SQM$DateTime)){
  x<-SQM$DateTime[n-1]
  x1<-SQM$DateTime[n]
  
  movs<- RFIDdata %>%
    filter(DateTime>=x)%>%
    filter(DateTime<=x1)%>%
    filter(Ant=="A1")
  SQM$A1det[n]<-dim(movs)[1]
  
  inds<-movs %>%
    group_by(TagID)%>%
    mutate(count=1)%>%
    summarise(count=sum(count))
  SQM$A1ind[n]<-dim(inds)[1]
}



################################################################################################################
###### MANUALLY SELECTING TIME INTERVALS AROUND BUNKERING EVENTS   ##############################
################################################################################################################

withships<-as.numeric(rownames(monitornights)[monitornights$ships>0])
before<-withships-1
after<-withships+1
selection<-unique(c(withships,before,after))
selection<-selection[selection>0]

monitornights_withoutnightdur <- monitornights[,-7] #to remove problematic nightdur
CaseControl<-monitornights_withoutnightdur%>% #monitornights
  filter(row_number() %in% selection)%>%
  select(NightStarting,sunset,sunrise,ships)%>% #nightdur
  filter(sunrise %within% recordingperiod3)
#TO DO DISCUSS WHETHER IT MAKES SENSE TO HAVE BEFORE AND AFTER BUNKERING PERIODS EQUAL TO THE BUNKERING PERIOD(i.e. NOT JUST 1 DAY BEFORE ND AFTER A 5 DAY BUNKERING EVENT)

### remove the lines outside the Case Control time periods
SQMcc<-data.frame()
#for(i in selection){
  #x<-SQM %>%
    #filter(DateTime %within% monitornights$nightdur[i])
  #SQMcc<-rbind(SQMcc,x)
#}

SQMcc <- merge(CaseControl, SQM, by= "NightStarting") 

#TO DO CHECK IF THERE ARE ERRORS IN SCRIPT - F. EG 7 SHIPS APPEARING IN 1 ONE ROW OF SQM FOR NIGHTSTARTING 2018-04-15 BUT 4 IN CASECONTROL AND SHIPNIGHTS??

SQMcc <- SQMcc %>%
  filter(!(NightStarting=="2018-02-13"))%>%
  arrange(DateTime)



#pdf("SQM_YESH_activity.pdf", width=8, height=6)
SQMcc %>% mutate(ships=ships.y*2) %>%   ### usually 2 boats operate for a bunkering event
  ggplot(aes(x=A1ind, y=MAG))+
  geom_point(aes(size=ships, color=ships)) +
  scale_color_gradient(low="black", high="yellow")+
  scale_size(range=c(0,4)) +
  guides(color=guide_legend(title="Number of ships"), size = guide_legend(title="Number of ships"))+
  xlab("Activity in YESH colony") +
  ylab("SQM measured darkness of cliff face") +
  
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.background = element_rect(fill="white", colour="white"),
        legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#dev.off()






################################################################################################################
###### QUESTION 3: SUMMARISE THE MOVEMENTS FOR EACH HOUR FOR NIGHTS WITH AND WITHOUT SHIPS BUNKERING   #####################
################################################################################################################

### remove the lines outside the Case Control time periods
MOVEScc<-data.frame()
for(i in selection){
  x<-MOVES %>%
    filter(DateTime %within% monitornights$nightdur[i]) %>%
    mutate(ships=monitornights$ships[i])%>%
    mutate(bunker=ifelse(ships>0,'with bunkering','no bunkering'))
  MOVEScc<-rbind(MOVEScc,x)
}



MOVEScc <- MOVEScc %>%
  filter(direction=="IN") %>%
  mutate(count=1) %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  mutate(MONTH=as.factor(month(DateTime)))%>% #, label=T, abbr=T, as.factor instead
  group_by(HR,MONTH,bunker,NightStarting) %>%
  summarise(activity=length(unique(TagID)))

MOVEScc_medians<-MOVEScc %>%
  group_by(MONTH,bunker) %>%
  summarise(MED=median(activity))

levels(MOVEScc$HR)

MOVEScc$HRordered <- factor(MOVEScc$HR, levels = c("19", "20","21","22","23","0","1","2","3","4","5")) #4 and 5 to be removed?

#pdf("Bunkering_YESH_activity.pdf", width=8, height=6)
ggplot(data=MOVEScc, aes(x=HRordered, y=activity, width=1))+
  geom_boxplot(colour="black") +
  facet_grid(MONTH ~ bunker,scales = "fixed", shrink = TRUE)+
  ylab("N individuals entering the cave") +
  geom_hline(data=MOVEScc_medians,aes(yintercept=MED), color='red')+
  scale_x_discrete(name="Hour of day", labels=c(19,20,21,22,23,0,1,2,3,4,5))+ #,4,5 should be removed?
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20),
        axis.title.y=element_text(margin=margin(0,15,0,0)),
        axis.title.x=element_text(margin=margin(15,0,0,0)), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
#dev.off()



#########################################################
#### STATISTICAL TEST OF BUNKERING EFFECT ON DARKNESS ###

Q3m<-glm(activity~HR+MONTH+bunker, data=MOVEScc, family=poisson)
outQ3m<-summary(Q3m)
outQ3m$coefficients

##################################################################
### PRODUCE OUTPUT REPORT WITH KEY TABLES AND FIGURES ###
##################################################################
#detach(packages:htmlwidgets)
#detach(name="package:htmlwidgets", unload=TRUE, character.only=TRUE)
#install.packages(c('plotly','htmlwidgets'), dependencies=T)

library(markdown)
library(rmarkdown)
library(knitr)
library(plotly)

### create HTML report for overall summary report
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files (x86)/RStudio/bin/pandoc")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")


#rmarkdown::render('C:\\STEFFEN\\RSPB\\Malta\\Analysis\\ColonyAttendance\\Malta_LightPollution_Summary.Rmd',
#output_file = "Malta_LightPollution_Report.html",
#output_dir = 'C:\\STEFFEN\\RSPB\\Malta\\Analysis\\ColonyAttendance')

rmarkdown::render('C:\\Users\\Martin\\Documents\\working_folder\\RFID\\2018\\Analysis\\RFID\\Malta_LightPollution_Summary_2018.Rmd',
                  output_file = "Malta_LightPollution_Report_2018_DRAFT.html",
                  output_dir = 'C:\\Users\\Martin\\Documents\\working_folder\\RFID\\2018\\Analysis')






################################################################################################################
###### DETERMINE ARRIVAL AND DEPARTURE FROM SEQUENTAL REGISTRATION AT A1 and A2   ##############################
################################################################################################################

### LOOP OVER EACH ANIMAL ###
YESH<-unique(RFIDdata$TagID)


### IN for A1 followed by A2 for same tag

MOVES<-data.frame()

for (A in YESH){
  nextAntV<-RFIDdata$Ant[RFIDdata$TagID==A]
  IND <- RFIDdata %>%
    filter(TagID == A) %>%
    mutate(nextAnt=c(nextAntV[-1],NA)) %>%
    mutate(move=ifelse(Ant==nextAnt,0,1)) %>%
    mutate(direction=ifelse(move==1,ifelse(nextAnt=="A2","IN","OUT"),"STAY"))
  
  MOVES<-bind_rows(MOVES,IND)
  
}

head(MOVES)



################################################################################################################
###### SUMMARISE THE IN AND OUT MOVEMENTS FOR EACH NIGHT   ##############################
################################################################################################################
#setwd("A:\\RSPB\\Malta\\Analysis\\ColonyAttendance")

### SUMMARISE THE MOVEMENTS OF EACH INDIVIDUAL ###

INDMOVES<- MOVES %>%
  filter(move==1) %>%
  group_by(TagID,direction) %>%
  summarise(nmoves=sum(move)) %>%
  spread(key=direction, value=nmoves)
fwrite(INDMOVES,"YESH_ind_movements_RFID.csv")

#### SUMMARISE THE MOVEMENTS FOR EACH NIGHT ACROSS ALL INDIVIDUALS ###

NIGHTMOVES<- MOVES %>%
  filter(move==1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting,direction) %>%                              
  summarise(nmoves=sum(move)) %>%
  spread(key=direction, value=nmoves) %>%
  arrange(NightStarting)
head(NIGHTMOVES)


#### MARTIN AUSTAD NOTICED ON 24 Aug 2017 that a lot of movements are based on few INDIVIDUALS ###
#### SUMMARISE THE N INDIVIDUALS MOVING EACH NIGHT ###


NIGHTIND<- MOVES %>%
  filter(move==1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting,direction) %>%                              
  summarise(nind=length(unique(TagID))) %>%
  spread(key=direction, value=nind) %>%
  arrange(NightStarting)
#fwrite(NIGHTIND,"YESH_n_ind_nightly_movements_RFID.csv")

### the above provides the number of individuals moving in or out
### that is not useful because we don't know whether they are the same, so we summarise all together

NIGHTIND<- MOVES %>%
  filter(move==1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting) %>%                              
  summarise(nind=length(unique(TagID))) %>%
  arrange(NightStarting)

## merge with n movements
dim(NIGHTMOVES)
dim(NIGHTIND)           ## has more rows - solved on 4 Sept: NIGHTIND[!(NIGHTIND$NightStarting %in% NIGHTMOVES$NightStarting),]
dim(NIGHTINDIVIDUALS)   ## this is the number of individuals at each antenna
NIGHTMOVES<-as.data.frame(NIGHTMOVES)
NIGHTMOVES$N_individuals=NIGHTIND$nind[match(NIGHTMOVES$NightStarting,NIGHTIND$NightStarting)]
NIGHTMOVES$N_ind_A1=NIGHTINDIVIDUALS$A1[match(NIGHTMOVES$NightStarting,NIGHTINDIVIDUALS$NightStarting)]
NIGHTMOVES$N_ind_A2=NIGHTINDIVIDUALS$A2[match(NIGHTMOVES$NightStarting,NIGHTINDIVIDUALS$NightStarting)]





################################################################################################################
###################### READ IN INDIVIDUAL DEPLOYMENT DATA   ###########################################
################################################################################################################


deployments<-deployments %>%
  select(TagID2018,PRn2018,Ring,Deploydate,Age,Sex) %>%
  mutate(Deploydate=dmy(Deploydate, tz="GMT")) %>%
  arrange(Deploydate)

n_deployed<- deployments %>%
  group_by(Deploydate) %>%
  summarise(n=length(unique(TagID2018)))

taggedby<-function(x){sum(n_deployed$n[n_deployed$Deploydate<x])}

NIGHTMOVES <- NIGHTMOVES %>%
  mutate(n_deployed=sapply(NightStarting,taggedby)) %>%
  mutate(prop_moving=N_individuals/n_deployed)


fwrite(NIGHTMOVES,"YESH_nightly_movements_RFID.csv")






################################################################################################################
###################### PLOT THE RAW DATA OVER TIME   ###########################################
################################################################################################################


#### PLOT THE NUMBER OF INDIVIDUALS MOVING PER NIGHT ###


#pdf("YESH_IND_movements_per_night_with_prop.pdf", width=8, height=6)

  ggplot(NIGHTMOVES)+
  geom_line(aes(x=NightStarting, y=N_individuals), colour='darkred', size=1.5)+
  geom_line(aes(x=NightStarting, y=prop_moving*100), colour='darkblue', size=1)+
    
  ## format axis ticks
  scale_x_date(name="Date", date_breaks="5 days", date_labels="%d-%b")+
  scale_y_continuous(name="N individual Yelkouan Shearwaters moving", breaks=seq(0,70,10), labels=as.character(seq(0,70,10)))+
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=12, color="black",angle=45,hjust = 1),
        axis.text.y=element_text(size=18, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

#dev.off()


#### PLOT THE NUMBER OF INDIVIDUALS MOVING PER NIGHT ###


#pdf("YESH_IND_movements_A2.pdf", width=8, height=6)

ggplot(NIGHTMOVES)+
  geom_line(aes(x=NightStarting, y=N_ind_A2), colour='darkred', size=1.5)+
  #geom_line(aes(x=NightStarting, y=prop_moving*100), colour='darkblue', size=1)+
  
  ## format axis ticks
  scale_x_date(name="Date", date_breaks="5 days", date_labels="%d-%b")+
  scale_y_continuous(name="N individual Yelkouan Shearwaters in A2", breaks=seq(0,70,10), labels=as.character(seq(0,70,10)))+
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=12, color="black",angle=45,hjust = 1),
        axis.text.y=element_text(size=18, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

#dev.off()




#### PLOT THE NUMBER OF MOVEMENTS PER NIGHT ###


pdf("YESH_movements_per_night.pdf", width=8, height=6)

NIGHTMOVES %>%
  gather(key=direction, value=nmoves,IN,OUT) %>%

  ggplot()+
  geom_line(aes(x=NightStarting, y=nmoves, colour=direction), size=1.5)+
  scale_colour_manual(values = c('darkred','darkblue'))+
  guides(fill=guide_legend(title="Movement direction", size=18, hjust=9))+
    
  ## place the legend in the top left corner
  theme(legend.position=c(0.05,0.9),
        legend.key = element_blank(),
        #legend.title = element_text("Movement direction", size=18, hjust=9),
        legend.background = element_blank(),
        legend.text = element_text(colour="darkgray", size = 16, face = "bold"))+
  
  
  ## format axis ticks
  scale_x_date(name="Date", date_breaks="5 days", date_labels="%d-%b")+
  scale_y_continuous(name="Yelkouan Shearwater arrivals and departures", breaks=seq(0,250,50), labels=as.character(seq(0,250,50)))+
  
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=12, color="black",angle=45,hjust = 1),
        axis.text.y=element_text(size=18, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"))

dev.off()


  