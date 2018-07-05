install.packages("lubridate")
install.packages("chron")
install.packages("suncalc")
install.packages("dtplyr")
library(parallel)
library(foreach)
library(doParallel)
library(lubridate)
library(chron)
library(suncalc)
library(dplyr)
library(dtplyr)

dogact1<-read.csv("dogact1.csv")
dogact2<-read.csv("dogact2.csv")

alldogs<- rbind(dogact1,dogact2)
write.csv(alldogs,"alldogs.csv")

dog.data<-alldogs
head(dog.data)

#read in table - note you can join the project and then you shouldn't need to change the file pathways
#this file is just a small sample from 1 dog - you will wat to create a table of all dogs in a similar format
#and run the code on that
dog.data<-read.table("WDM131_activity_final.txt",header=TRUE,sep="\t")
#View data
View(dog.data)

#writes date column as date
dog.data$UTC.Date<-as.Date(dog.data$UTC.Date,format="%Y-%m-%d")
dog.data$UTC.Time<-chron(times=dog.data$UTC.Time)
dog.data$LocalDate<-NA
dog.data$LocalTime<-NA
dog.data$timedate<-NA
dog.data$timedate.local<-NA
dog.data[,10:15]<-NA

dog.data$timedate<-as.POSIXct(paste(dog.data$UTC.Date, dog.data$UTC.Time), format="%Y-%m-%d %H:%M:%S")
dog.data$timedate.local<-dog.data$timedate+3*60*60
dog.data$LocalDate<-as.Date(dog.data$timedate.local)
dog.data$LocalTime<-strftime(dog.data$timedate.local, format="%H:%M:%S")

write.csv(alldogs,"alldogs.csv")

#adds dawn
dog.data[1:15,10:15]<-getSunlightTimes(date = dog.data$LocalDate[1:15], lat = 0.2922, lon = 36.8980, , keep = c("dawn"), tz = "Africa/Nairobi")
#deletes columns we don't need
dog.data<-dog.data[,-c(7:9,11)]

cl <- makeCluster(11)
registerDoParallel(cl)

dawntime<-matrix(NA, ncol = 5, nrow = nrow(dog.data))

foreach(xc=dog.data$LocalDate[1:2], .errorhandling="pass", .packages=c("lubridate","chron","suncalc"),.combine=rbind) %dopar% {
  dawntime=getSunlightTimes(date = dog.data$LocalDate[1:2], lat = 0.2922, lon = 36.8980, , keep = c("dawn"), tz = "Africa/Nairobi")
}

View(dog.data2)


dog.data[1:15,]
View(dog.data2)

#same for dusk
dog.data[,8:12]<-getSunlightTimes(date = dog.data$LocalDate, lat = 0.2922, lon = 36.8980, , keep = c("dusk"), tz = "Africa/Nairobi")
dog.data<-dog.data[,-c(8:10,12)]

#change local time to time
dog.data$LocalTime<-chron(times=dog.data$LocalTime)


#sum activity across two axes and name column
dog.data[,9]<-dog.data[,4]+dog.data[,5]
colnames(dog.data)[9] <- "Activity.Both"
dog.data$Activity.Both<-as.numeric(dog.data$Activity.Both)

dog.data[,"Duration"]<-NA    # adding column for duration and correcting format
dog.data[,"SumAct"]<-NA
dog.data$SumAct<-as.numeric(dog.data$SumAct)
dog.data[,"SSDate"]<-NA       # adding new column for SSDate that = NA
dog.data$SSDate<-as.Date(dog.data$SSDate,format="%Y-%m-%d")
dog.data[,"SSTime"]<-"00:00:00"     # adding new column for SSDate that = NA
dog.data$SSTime<-chron(times=dog.data$SSTime)
dog.data[,"StopStart"]<-NA
    


#fills a column with starts and stops of bouts
for(i in 1:nrow(dog.data)){
  if(sum(dog.data[i+1,9],dog.data[i+2,9],dog.data[i+3,9])==0 && dog.data[i,9]>0 && dog.data[i+1,1]==dog.data[i,1]){
    dog.data[i+1,14]<-"Stop"          # && dog.data[i+1,1]==dog.data[i,1]) this bit controls for same dog
  }
  if(dog.data[i+1,9]>0 && dog.data[i,9]==0 && dog.data[i+1,1]==dog.data[i,1]){
    dog.data[i+1,14]<-"Start"
  }
}

#renames column 14 Bout
colnames(dog.data)[14] <- "Bout"

#changes NA to 0
dog.data$Bout[is.na(dog.data$Bout)] <- 0


Starts<-which(dog.data[,14]=="Start")      # making vectors of row numbers in which have Start or Stop
Stops<-which(dog.data[,14]=="Stop")
StartStops<-which(dog.data[,14]>0)

View(StartStops)

for (i in 2:length(StartStops)){
  if(StartStops[i-1] %in% Starts && StartStops[i] %in% Starts){
    dog.data[StartStops[i],14]<-0    # removing duplicate starts
  }
}
 
for(i in 1:nrow(dog.data)){         # printing time stamp of start/stop
  if(dog.data[i,14]=="Start" || dog.data[i,14]=="Stop"){
    dog.data[i,12]<-dog.data[i,2]
    dog.data[i,13]<-dog.data[i,3]
  }
}

# to combine SSDate and SSTime columns
dog.data$SSDateTime = (paste(dog.data$SSDate, dog.data$SSTime))
dog.data$SSDateTime<-parse_date_time(dog.data$SSDateTime,"%Y-%m-%d H:M:S")

dog.data<-dog.data[,c(1:11,15,12:14)]    # re-ordering columns so SSDateTime next to SumAct

StartStops<-which(dog.data[,15]>0)  # making vector of all rows with starts and stops

for(i in 2:length(StartStops)){
  if(i %%2 ==0){ #this bit says if it is even sum the bout and write to rows with start in - this only works if your first one is stop. if it start you will want to change the 0 to 1 I think
    dog.data[StartStops[i],11]<-sum(dog.data[StartStops[i]:StartStops[i+1],9])
  }         # working out sum of activity between start and stop
}


#just keep starts and stops
allbouts<-dog.data[(dog.data$Bout)!=0, ]

View(allbouts)


#this gives you the time since the last start or stop. Start will get the bout duration
#and stop rows will have the time since the last activity bout
for(i in 1:nrow(allbouts)){      
  allbouts[i,10]<-difftime(allbouts$SSDateTime[i+1],allbouts$SSDateTime[i], units="mins")
}


View(allbouts)

#writes csv table - you don't have to do this but good for if you leave code and don't want to lose progress
write.table(allbouts,"All.dogs.all.bouts",sep=",",row.names=FALSE) 


#caluclate intensity of each bout
allbouts[,"Intensity"]<-NA

for(i in 1:nrow(allbouts)){
  allbouts[i,"Intensity"]<-allbouts[i,"SumAct"]/allbouts[i,"Duration"]
}

#add time of day the bout comes under
allbouts[,"PartOfDay"]<-NA

is.between<-function(a,b,c){
  a>=b & a<=c
}

## the first one is a Stop, so go from first Start in row 2
for(i in 2:nrow(allbouts)){
  if(is.between(allbouts[i,"SSTime"],'04:55:39','06:56:04')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'04:55:39','07:30:40')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"EarlyMorning"    }      
  
  if(is.between(allbouts[i,"SSTime"],'04:55:39','06:56:04')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'07:30:41','12:51:33')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"Morning"          
  }
  if(is.between(allbouts[i,"SSTime"],'06:56:39','12:51:33')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'07:30:41','12:51:33')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"LateMorning"          
  }
  if(is.between(allbouts[i,"SSTime"],'04:55:39','06:56:04')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'12:51:34','18:47:02')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"MorningToDay"          
  }
  if(is.between(allbouts[i,"SSTime"],'06:56:05','16:01:54')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'12:51:34','18:47:02')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"Daytime"          
  }
  if(is.between(allbouts[i,"SSTime"],'16:01:55','18:47:02')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'12:51:34','18:47:02')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"LateDaytime"          
  }
  if(is.between(allbouts[i,"SSTime"],'06:56:05','16:01:54')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'18:47:03','21:06:56')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"DayToEvening"          
  }
  if(is.between(allbouts[i,"SSTime"],'16:01:55','18:26:52')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'18:47:03','21:06:56')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"Evening"          
  }
  if(is.between(allbouts[i,"SSTime"],'18:26:53','21:06:56')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'18:47:03','21:06:56')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"LateEvening"          
  }
  if(is.between(allbouts[i,"SSTime"],'16:01:55','18:26:52')=="TRUE" && is.between(allbouts[i+1,"SSTime"],'21:06:57','23:59:59')=="TRUE" || is.between(allbouts[i,"SSTime"],'00:00:00','07:30:40')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"EveningToNight"          
  }
  if(is.between(allbouts[i,"SSTime"],'18:26:53','23:59:59')=="TRUE" || is.between(allbouts[i,"SSTime"],'00:00:00','04:55:38')=="TRUE" && is.between(allbouts[i,"SSTime"],'21:06:57','23:59:59')=="TRUE" || is.between(allbouts[i,"SSTime"],'00:00:00','07:30:40')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"Nighttime"
  }
  if(is.between(allbouts[i,"SSTime"],'18:26:53','23:59:59')=="TRUE" || is.between(allbouts[i+1,"SSTime"],'00:00:00','04:55:38')=="TRUE" && is.between(allbouts[i,"SSTime"],'07:30:41','12:51:33')=="TRUE"){
    allbouts[i,"PartOfDay"]<-"NightToMorning"          
  }
} 

#extract out inactive periods into a data frame called inactivity
Inactivity<-subset(allbouts,allbouts$Bout=="Stop")

View(Inactivity)

#extract out bouts only
Bouts.only<-subset(allbouts,allbouts$Bout=="Start")


#extract out bouts that have a sum of activity over 1000 and a length over 20 - what are most likely hunts
#you are welcome to have a play with these numbers and see if you think the cut-offs are right
Hunts.only<-subset(allbouts,allbouts$SumAct>=1000)
Hunts.only<-subset(allbouts,allbouts$Duration>=20)

View(Hunts.only)

#you can plot out your bouts like this
hist(Hunts.only$StartTime,breaks=1440)

#remove duplicate stops (we only want the ones immediately after the starts - ie the end of the long bouts)
for(i in 2:nrow(Hunts.only)){
  if(Hunts.only[i-1,"Bout"]=="Stop" && Hunts.only[i,"Bout"]=="Stop"){
    Hunts.only[i,"Bout"]<-0
  }
  if(Hunts.only[i-1,"Bout"]==0 && Hunts.only[i,"Bout"]=="Stop"){
    Hunts.only[i,"Bout"]<-0
  }
}

Hunts.only<-subset(Hunts.only,Hunts.only$Bout>0)

#add stop time of the previous bout to long bouts
Hunts.only[,"StopTime_Previous"]<-"1/1/1900 00:00:00"
Hunts.only$StopTime_Previous<-as.POSIXct(Hunts.only$StopTime_Previous,format="%d/%m/%Y %H:%M",tz="UTC")

#adds the previous stop time to the start rows and start time to the stop rows
for(i in 2:nrow(Hunts.only)){
  if(Hunts.only[i-1,1]==Hunts.only[i,1]){
    Hunts.only[i,16]<-Hunts.only[i-1,12]
  }
}

#example of how to extract start of bout relative to sunrise.
#I would suggest doing start and stop times in the day relative to sunrise and at night relative to sunset
#we probably want to talk to Rosie about this though

