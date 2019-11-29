## Soil respiration
## 31/05/2018

library(readxl) ## To read the excell file
library(readr) ## To read csv file
library(lubridate) ## To extract the hour
library(plyr) ## To use SummarySE
library(dplyr) ## To use SummarySE
library(ggplot2) ## To make ggplot graphs (you need ggpubr too)
library(ggthemes) ## To modify features in graphs
library(ggpubr) ## to make ggplot graphs (you need ggplot2 too)
library(xts) ## To get daily values
library (nlme)
library(rmcorr) ## for repetead measurement correlation 
library(lavaan) ## SEM models didnt work

setwd("E:/Dropbox/Respiracion de suelo/Outputs")## Laptop P
setwd("C:/Users/FISICA/Dropbox/Respiracion de suelo/Outputs")## Lab p
setwd("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs")## Lab E

# Temperature/Precipitation vs Time-----------------

D10 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2010.txt",skip = 1) 
D11 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2011.txt",skip = 1) 
D12 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2012.txt",skip = 1) 
D13 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2013.txt",skip = 1) 
D14 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2014.txt",skip = 1) 
D15 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2015.txt",skip = 1) 
D16 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2016.txt",skip = 1) 
D17 <- read.delim("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/2017.txt",skip = 1)

Data<- rbind(D10,D11,D12,D13,D14,D15,D16,D17)
head(Data)
tail(Data)
names(Data)
summary(Data)
colnames(Data)[3:32]<-c("TempOut","Tmax","Tmin","HumOut","DewOut","WindSp","WindDir","WindRun","WindSpMax","WindDirMax",
                        "Chill","HeatIndex","THWIndex","THRWIndex","Bar","Rain","RainRate","SolarRad","SolarE","RadMax","RadUV","UVdose","UVMax",
                        "DDHeat","DDCold","TempIn","HumIn","DewIn","HeatIn","ET")
Data1<-Data[,-c(33:36),drop=F]
Data1[,3:32]<-lapply(Data1[,3:32], function(x) as.numeric(x))
Data1<-Data1[,-c(9,12),drop=F]
Data1<-cbind(Data1,Data$WindDir,Data$WindDirMax)
colnames(Data1)[31:32]<-c("WindDir","WindDirMax")
summary(Data1)
head(Data1)

## PP_T_Years----------
df.xts <- as.xts(Data1[,3:30], order.by=as.Date(Data1[,1], format="%d/%m/%y")) ## To transform the data into xts obj
DData <- apply.yearly(df.xts, mean, na.rm=TRUE) ## Mean of all data
d.rain<-apply.yearly(df.xts$Rain,sum) ## but rain has to be sum, so
DData<- DData[,-14,drop=F]
DData<-cbind(DData,d.rain)
DData<-DData[-9,,drop=F]
DData

Year<-c(2010,2011,2012,2013,2014,2015,2016,2017) #to create a vector (2010:2017)
DData<-cbind(Year,DData)
colnames(DData)[1]<-"Year"
DData<-as.data.frame(DData)

f<-ggplot(DData,aes(x=Year))+ 
  Tema + theme(legend.position = "top") +
  geom_line(aes(y=DData$TempOut*150-3000,color ="firebrick1"), size=1) +
  labs(y="Annual Precipitation (mm)", x="Time (years)") +
  geom_line(aes(y=DData$Rain, color="dodgerblue2"), size=1) +
  scale_y_continuous(sec.axis=sec_axis(~./150+20,name="Mean Annual Temperature (C)", breaks=c(20:25))) + 
  scale_x_continuous(breaks=c(2010:2017))+
  labs (colour="") + scale_color_manual(values=c("dodgerblue2","firebrick1"), labels=c("Annual Precipitation","Mean Annual Temperature"))
f
tiff("PP_T_Year_legend.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
f
dev.off()
## Rad_T_Hours-----
Date<-as.POSIXct(Data1$Date,format="%d/%m/%y")
Time<-as.POSIXct(Data1$Time,format="%H:%M")
hour<-format(Time,"%H") # To extract the hour
min<-format(Time,"%M")
day<-format(Date,"%d")
Month<-format(Date,"%m")
Year<-format(Date,"%y")

Data2<-cbind(Year,Month,day,hour,min,Data1[3:32])
# names(Data2)
# summary(Data2)
Data3<-as.data.frame(Data2)
DataHourly<-lapply(Data3[,6:33], function(x) tapply(x, Data3$hour, mean,na.rm = TRUE)) ## to get clasify data ##careful with rain, not the correct way measured (average, not sum)
DataHourly<-as.data.frame(DataHourly)
Hour<-c(0:23)
Data4<-cbind(Hour,DataHourly)

Tema <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", size=1,
                                          linetype= "solid", fill=NA),
              axis.text=element_text(size=20,colour="black"),
              #axis.text.x= element_text(face ="bold"),
              axis.title=element_text(size=24),
              legend.text=element_text(size=24),
              legend.title=element_text(size=24))
              #legend.position="none")
e<- seq(0,23,2) ## to generate a leap sequence 
g<-ggplot(Data4,aes(x=Hour))+ 
  Tema+ theme(legend.position = "top")+
  geom_line(aes(y=Data4$TempOut*60-1200,colour ="firebrick1"), size=1) +
  labs(y=expression("Solar Radiation"~group("(",W~m^-2,")")), x="Time (hours)") + 
  geom_line(aes(y=Data4$SolarRad, colour="darkorchid4"), size=1) + 
  scale_y_continuous(sec.axis=sec_axis(~./60+20,name="Hourly Temperature (C)", breaks=c(19,21,23,25,27,29,31,33))) + 
  scale_x_continuous(breaks=e)+
  labs (colour="") + scale_colour_manual(values=c("darkorchid4","firebrick1"), labels=c("Solar Radiation","Hourly Temperature"))
g
tiff("Rad_T_Hour_legend.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
g
dev.off()
g
## Merge PP_T and Rad_T graphs
v<-ggarrange(f,g, nrow=2, ncol=1, align="v", labels = c("A)", "B)"),
             font.label = list(size = 22, face = "bold"))
v
tiff("PP_Rad_T_legend.tiff", width = 6, height = 9, units = 'in', res = 300, compression = 'lzw')
v
dev.off()



## SummarySE function ----------------
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
# Data Logger-extracting and cleaning_2018-------------------

DD <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Humedad de suelo_DATALOGGER_2.xlsx",sheet="Dentro")
DF <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Humedad de suelo_DATALOGGER_2.xlsx",sheet="Fuera")
summary(DD)
Fecha<-as.POSIXct(DD$Fecha,format="%Y.%m.%d %H:%M:%S")
Dia<-format(Fecha,"%d") # To extract the day
Hora<-format(Fecha,"%H")

DD2<-cbind(DD[1:5], Dia, Hora)
# DD2<-subset(DD2[,c(1,3,4,5,6,7), drop=F])
DD2<-subset(DD2[c(84:1091),,drop=F])
DD2

Fecha<-as.POSIXct(DF$Fecha,format="%Y.%m.%d %H:%M:%S")
Dia<-format(Fecha,"%d") # To extract the day
Hora<-format(Fecha,"%H")

DF2<-cbind(DF[1:5], Dia, Hora)
# DF2<-subset(DF2[,c(1,3,4,5,6,7), drop=F])
DF2<-subset(DF2[c(93:956),,drop=F])
DF2

LD<-rbind(DF2,DD2)

write.csv(LD,file= "LOGGER_DATA_2018I.csv")

# Statistical measurements of logger data_2018------------

LOGGER_DATA <- read_csv("LOGGER_DATA_2018I.csv", 
                        col_types = cols(Dia = col_character(), 
                                         Hora = col_character()))

Temperatura <- summarySE(LOGGER_DATA, measurevar=c("Temperatura"), groupvars=c("Hora","Zona"),na.rm=TRUE) ## na.rm to ignore text in a values data frame ## summarySE to find mean, standar dev , standar error and confidence interval (annually)
Hum_Rel <- summarySE(LOGGER_DATA, measurevar=c("Hum_Rel"), groupvars=c("Hora","Zona"),na.rm=TRUE)

T_HR_2018I <- cbind(Temperatura,Hum_Rel$Hum_Rel,Hum_Rel$sd,Hum_Rel$se,Hum_Rel$ci)
colnames(T_HR_2018I)[8:11]<-c("Hum_Rel","HR_sd","HR_se","HR_ci")
write.csv(T_HR_2018I,file= "T_HR_LOGGER_2018I.csv")

## T_in_out_Hour_2018 graph----------
T_HR_2018I[,1]=as.numeric(T_HR_2018I[,1])
h<-ggplot(data=subset(T_HR_2018I,Zona=="Dentro"),aes(x=Hora)) + 
  Tema + theme(legend.position = "none" ) +
  geom_line(data=subset(T_HR_2018I,Zona=="Dentro"),aes(y=Temperatura, color=factor(Zona)),size=1) +
  labs(y=expression("Temperature (C)"), x="Time (hours)",color="") +
  geom_line(data=subset(T_HR_2018I,Zona=="Fuera"),aes(y=Temperatura, color=factor(Zona)),size=1) + 
  scale_colour_manual(values=c("darksalmon","firebrick"), labels=c("Under canopy", "Outside canopy")) + 
  scale_y_continuous(limits = c(17,43))
h
tiff("T_in_out_H.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
h
dev.off()
h
## HR_in_out_Hour_2018 graph-----
i<-ggplot(data=subset(T_HR_2018I,Zona=="Dentro"),aes(x=Hora)) +
  Tema + theme(legend.position = "none" ) + 
  geom_line(data=subset(T_HR_2018I,Zona=="Dentro"),aes(y=Hum_Rel, color=factor(Zona)),size=1) + 
  labs(y=expression("Relative Humidity (%)"), x="Time (hours)",color="") + 
  geom_line(data=subset(T_HR_2018I,Zona=="Fuera"),aes(y=Hum_Rel,color=factor(Zona)),size=1) +
  scale_colour_manual(values=c("darkslategray3","dodgerblue4"), labels=c("Under canopy", "Outside canopy")) +
  scale_y_continuous(limits = c(37,85))
i
tiff("HR_in_out_H.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
i
dev.off()
i
## Statistical measurements of logger data_2017----
DD <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Humedad de suelo_DATALOGGER1.xlsx")
write.csv(DD,file= "LOGGER_DATA_2017I.csv")
LOGGER_DATA <- read_csv("LOGGER_DATA_2017I.csv", 
                        col_types = cols(Dia = col_character(), 
                                         Hora = col_character()))

Temperatura <- summarySE(LOGGER_DATA, measurevar=c("Temperatura"), groupvars=c("Hora","Zona"),na.rm=TRUE) ## na.rm to ignore text in a values data frame ## summarySE to find mean, standar dev , standar error and confidence interval (annually)
Hum_Rel <- summarySE(LOGGER_DATA, measurevar=c("Hum_Rel"), groupvars=c("Hora","Zona"),na.rm=TRUE)

T_HR_2017I <- cbind(Temperatura,Hum_Rel$Hum_Rel,Hum_Rel$sd,Hum_Rel$se,Hum_Rel$ci)
colnames(T_HR_2017I)[8:11]<-c("Hum_Rel","HR_sd","HR_se","HR_ci")
write.csv(T_HR_2017I,file= "T_HR_LOGGER_2017I.csv")

## T_in_out_Hour_2017-----
T_HR_2017I[,1]=as.numeric(T_HR_2017I[,1])
j<-ggplot(data=subset(T_HR_2017I,Zona=="Dentro"),aes(x=Hora)) + 
  Tema + theme(legend.position = "top", axis.text.x=element_blank(),axis.title.x = element_blank()) +
  geom_line(data=subset(T_HR_2017I,Zona=="Dentro"),aes(y=Temperatura, color=factor(Zona), group=1),size=1) + 
  labs(y=expression("Temperature (C)"), x="Time (hours)",color="") +
  geom_line(data=subset(T_HR_2017I,Zona=="Fuera"),aes(y=Temperatura, color=factor(Zona), group=1),size=1) + 
  scale_colour_manual(values=c("darksalmon","firebrick"), labels=c("Under canopy", "Outside canopy")) +
  scale_y_continuous(limits = c(17,43))
j
tiff("T_in_out_H_2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
j
dev.off()
j
## HR_in_out_Hour_2017------
k<-ggplot(data=subset(T_HR_2017I,Zona=="Dentro"),aes(x=Hora)) +
  Tema + theme(legend.position = "top", axis.text.x=element_blank(),axis.title.x = element_blank() ) +
  geom_line(data=subset(T_HR_2017I,Zona=="Dentro"),aes(y=Hum_Rel, color=factor(Zona)),size=1) + 
  labs(y=expression("Relative Humidity (%)"), x="Time (hours)",color="") +
  geom_line(data=subset(T_HR_2017I,Zona=="Fuera"),aes(y=Hum_Rel,color=factor(Zona)),size=1) + 
  scale_colour_manual(values=c("darkslategray3","dodgerblue4"), labels=c("Under canopy", "Outside canopy")) +
  scale_y_continuous(limits = c(37,85))
k
tiff("HR_in_out_H_2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
k
dev.off()
k
## Merge 4 graphs T_HR_2017_2018
v<-ggarrange(j,k,h,i, nrow=2, ncol=2, align="v", labels = c("A)", "B)", "C)","D)"),
             font.label = list(size = 22, face = "bold"))
v <-annotate_figure(v,right=text_grob("2017                                      2018",face="bold", size=22, rot=270))
v
tiff("T_HR.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
v
dev.off()

## merge 4 graphs + soil humidity graphs

# Soil humidity graphs

soil_humidity <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Humedad de suelo_Análisis.xlsx")
colnames(soil_humidity)[10]<-"SH"
soil_humidity<-soil_humidity[-1, ,drop=F]

SHC <- summarySE(soil_humidity, measurevar="SH", groupvars=c("Cobertura","Year"), na.rm=TRUE)

SHC[1,1]<- "Under canopy"
SHC[2,1]<- "Under canopy"
SHC[3,1]<- "Outside canopy"
SHC[4,1]<- "Outside canopy"

Q<-ggplot(data=filter(SHC,Year== "2017"), aes(x=Cobertura, y=SH, fill=Cobertura)) +
  Tema + theme(legend.position = "none", legend.title=element_blank(), 
               axis.text=element_text(size=16)) + labs(x="") +
  geom_errorbar(aes(ymin=SH-se, ymax=SH+se),
                width=0.2,                    # Width of the error bars
                position=position_dodge(0.7)) + 
  geom_bar(width = 0.65, position=position_dodge(), stat="identity") +
  scale_fill_manual("Cobertura",
                    values = c("Outside canopy"= "tan","Under canopy" = "springgreen4"), drop=FALSE) +
  scale_y_continuous(name="Soil Humidity (%)",expand=c(0,0), limits=c(0,1.5)) 
Q

R<-ggplot(data=filter(SHC,Year== "2018"), aes(x=Cobertura, y=SH, fill=Cobertura)) +
  Tema + theme(legend.position = "none", axis.text=element_text(size=16) ) + labs(x="") +
  geom_errorbar(aes(ymin=SH-se, ymax=SH+se),
                width=0.2,                    # Width of the error bars
                position=position_dodge(0.7)) + 
  geom_bar(width = 0.65, position=position_dodge(), stat="identity") +
  scale_fill_manual("Cobertura",
                    values = c("Outside canopy"= "tan","Under canopy" = "springgreen4"), drop=FALSE) +
  scale_y_continuous(name="Soil Humidity (%)",expand=c(0,0), limits=c(0,1.5), breaks = c(0.0,0.5,1.0))
R

O<-ggarrange(Q,R, nrow=1, ncol=2, align="hv",
             font.label = list(size = 22, face = "bold"))
tiff("SH_Out_In.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
O
dev.off()

## SR_ENSO_non-ENSO-----

sr_2017 <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/soil respiration 2017 A2.xlsx", 
                      sheet = "SR)") ## Oficina E
sr_2017 <- read_excel("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/soil respiration 2017 A2.xlsx", 
                      sheet = "SR)") ## oficina P
# sr_2017 <- read_excel("E:/Dropbox/Respiracion de suelo/Data/soil respiration 2017 A2.xlsx", 
#                      sheet = "SR)" ## laptop P
# sr_2017<- sr_2017[c(151:300),,drop=F ]
Tiempo<-as.POSIXct(sr_2017$Tiempo,format="%Y-%m-%d %H:%M:%S")
Hora<-format(sr_2017$Tiempo,"%H:%M")
sr_2017<-cbind(sr_2017,Hora)
sr_2017<-subset(sr_2017[,-9,drop=F])
colnames(sr_2017)[1]<-"Codigo"
write.csv(sr_2017,file= "sr_2017.csv")

sr_2017 <- read_csv("sr_2017.csv")

year <- rep("2017",time=150)
sr_2017 <-cbind(year,sr_2017)
sr_2017 <- sr_2017[,-(2),drop=FALSE] 

sr_2018 <-read_excel("E:/Dropbox/Respiracion de suelo/Data/soil respiration 2018 I.xlsx", 
                     sheet = "2018_I") ## Laptop P

sr_2018 <-read_excel("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/soil respiration 2018 I.xlsx", 
                     sheet = "2018_I") ## oficina P

sr_2018 <-read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/soil respiration 2018 I.xlsx", 
                     sheet = "2018_I") ## oficina E

# To convert Photosynthesis values to respiration ones

VSR1<- rep(-1, time=150)
VSR2<- rep(1, time=150)
VSR <- c(VSR2,VSR1)
year <- rep("2018",time=300)
sr_2018 <-cbind(year,sr_2018, VSR)
colnames(sr_2018)[10] <-"Hora"
sr_2018["Soil_respiration_R"] <- sr_2018$Soil_respiration*VSR
sr_2018$Soil_respiration <- sr_2018$Soil_respiration_R # to replace one column for another
sr_2018 <- sr_2018 [,-c(11,12),drop=FALSE] 

sr_2017_2018 <- rbind(sr_2017,sr_2018)
sr_2017_2018[,8][sr_2017_2018[,8] < 0] <- 0 #to replace negative values for 0

write.csv(sr_2017_2018,file= "sr_2017_2018_complete.csv")
SR2 <- summarySE(sr_2017_2018, measurevar="Soil_respiration", groupvars=c("year","Season","Iluminacion","Cobertura"),na.rm=TRUE)
names(SR2)
write.csv(SR2,file= "SR2.csv")

# SR comparisson between 2017 and 2018 (AP and AG)-----

sr_2017_2018 <- read_csv("sr_2017_2018_complete.csv")

SRC <- summarySE(filter(sr_2017_2018,year=="2017"), measurevar="Soil_respiration", groupvars=c("Season","Cobertura"),na.rm=TRUE)
names(SRC)

R<-ggplot(SRC, aes(x=Season, y=Soil_respiration, fill=Cobertura))+
  geom_errorbar(aes(ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2,                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_bar(width = 0.65, position=position_dodge(), stat="identity")+
  Tema+theme(axis.text.x= element_text(face ="bold",size=20),axis.title.y = element_text(size=24),
             legend.position="top",legend.title=element_blank(),plot.title = element_text(hjust = 0.5),
             plot.caption = element_text(face="bold",hjust=0.5, size=24), plot.margin = unit(c(1,1,1,1), "cm")) + 
  scale_x_discrete("") +
  scale_y_continuous(expand= c(0,0),limits = c(0,1)) +
  labs(y=expression(paste(Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")")))) + 
  scale_fill_manual(values = c("Under canopy" = "springgreen4", "Canopy interspace"= "tan"),labels=c("Canopy interspace"="Outside canopy","Under canopy"="Under canopy"))
R
tiff("SR_2017.tiff", width = 9, height = 4.5, units = 'in', res = 300, compression = 'lzw')
R
dev.off()

SRC1 <- summarySE(filter(sr_2017_2018, year=="2018"), measurevar="Soil_respiration", groupvars=c("Season","Cobertura"),na.rm=TRUE)
names(SRC1)

Q<-ggplot(SRC1, aes(x=Season, y=Soil_respiration, fill=Cobertura))+
  geom_errorbar(aes(ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                   width=0.2,                    # Width of the error bars
                   position=position_dodge(0.7))+
  geom_bar(width = 0.65, position=position_dodge(), stat="identity")+
  Tema+theme(axis.text.x= element_text(face ="bold"),legend.position="top",
             legend.title=element_blank(),plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(face="bold",hjust=0.5, size=24),plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_x_discrete("") +
  scale_y_continuous(expand= c(0,0),limits = c(0,1),position = "right") +
  labs(y="") +
  scale_fill_manual(values = c("Under canopy" = "springgreen4", "Canopy interspace"= "tan"),
                    labels=c("Under canopy"="Under canopy","Canopy interspace"="Outside canopy"))
Q
tiff("SR_2018.tiff", width = 9, height = 4.5, units = 'in', res = 300, compression = 'lzw')
Q
dev.off()

S<-ggarrange(R,Q, nrow=1, ncol=2, align="h", common.legend = TRUE, legend="top",
             font.label = list(size = 22, face = "bold"))
S

tiff("SR_2017_2018_V3.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
S
dev.off()

p<-ggplot(data=SR2)+
Tema + theme(legend.position = "top", axis.text.x=element_blank())+
  labs(y="", x="",color="") +
geom_line(data=filter(SR2,year== "2017" & Season== "April" & Cobertura=="Under canopy"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+
geom_line(data=filter(SR2,year== "2017" & Season== "April" & Cobertura=="Canopy interspace"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+  
geom_errorbar(data=filter(SR2,year== "2017" & Season== "April" & Cobertura=="Under canopy"),
                   aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                   width=0.2, color= "springgreen4",                    # Width of the error bars
                   position=position_dodge(0.7))+
geom_errorbar(data=filter(SR2,year== "2017" & Season== "April" & Cobertura=="Canopy interspace"),
                   aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                   width=0.2, color="tan",                    # Width of the error bars
                   position=position_dodge(0.7))+
geom_point(data=filter(SR2,year== "2017" & Season== "April" & Cobertura=="Under canopy"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
geom_point(data=filter(SR2,year== "2017" & Season== "April" & Cobertura=="Canopy interspace"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy"))+
scale_y_continuous(sec.axis=sec_axis(~.*1, breaks = NULL))
p

tiff("SR_2017_summer.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
p
dev.off()


d<-ggplot(data=SR2)+
  Tema + theme(legend.position = "top",plot.caption = element_text(face="bold",hjust=0.5, size=24))+
  labs(y="", x="Time (hours)",color="", caption= "2017") +
  geom_line(data=filter(SR2,year== "2017" & Season== "August" & Cobertura=="Under canopy"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+
  geom_line(data=filter(SR2,year== "2017" & Season== "August" & Cobertura=="Canopy interspace"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+  
  geom_errorbar(data=filter(SR2,year== "2017" & Season== "August" & Cobertura=="Under canopy"),
                aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2, color= "springgreen4",                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_errorbar(data=filter(SR2,year== "2017" & Season== "August" & Cobertura=="Canopy interspace"),
                aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2, color="tan",                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_point(data=filter(SR2,year== "2017" & Season== "August" & Cobertura=="Under canopy"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
  geom_point(data=filter(SR2,year== "2017" & Season== "August" & Cobertura=="Canopy interspace"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy"))+
  scale_y_continuous(sec.axis=sec_axis(~.*1, breaks = NULL))

d

tiff("SR_2017_Autumn.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
d
dev.off()

w<-ggplot(data=SR2)+
  Tema + theme(legend.position = "top", axis.text.x=element_blank())+ # delete axis names
  labs(y="", x="",color="") +
  geom_line(data=filter(SR2,year== "2018" & Season== "April" & Cobertura=="Under canopy"),
            aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+
  geom_line(data=filter(SR2,year== "2018" & Season== "April" & Cobertura=="Canopy interspace"),
            aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+  
  geom_errorbar(data=filter(SR2,year== "2018" & Season== "April" & Cobertura=="Under canopy"),
                aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2, color= "springgreen4",                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_errorbar(data=filter(SR2,year== "2018" & Season== "April" & Cobertura=="Canopy interspace"),
                aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2, color="tan",                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_point(data=filter(SR2,year== "2018" & Season== "April" & Cobertura=="Under canopy"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
  geom_point(data=filter(SR2,year== "2018" & Season== "April" & Cobertura=="Canopy interspace"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy"))+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="April", breaks = NULL))
w

tiff("SR_2018_summer.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
w
dev.off()

u<-ggplot(data=SR2)+
  Tema + theme(legend.position = "top",plot.caption = element_text(face="bold",hjust=0.5, size=24))+
  labs(y="", x="Time (hours)",color="", caption="2018") +
  geom_line(data=filter(SR2,year== "2018" & Season== "August" & Cobertura=="Under canopy"),
            aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+
  geom_line(data=filter(SR2,year== "2018" & Season== "August" & Cobertura=="Canopy interspace"),
            aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=1)+  
  geom_errorbar(data=filter(SR2,year== "2018" & Season== "August" & Cobertura=="Under canopy"),
                aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2, color= "springgreen4",                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_errorbar(data=filter(SR2,year== "2018" & Season== "August" & Cobertura=="Canopy interspace"),
                aes(x= Iluminacion, ymin=Soil_respiration-se, ymax=Soil_respiration+se),
                width=0.2, color="tan",                    # Width of the error bars
                position=position_dodge(0.7))+
  geom_point(data=filter(SR2,year== "2018" & Season== "August" & Cobertura=="Under canopy"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
  geom_point(data=filter(SR2,year== "2018" & Season== "August" & Cobertura=="Canopy interspace"),aes(x= Iluminacion, y=Soil_respiration, color=factor(Cobertura)),size=4)+  
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy"))+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="August", breaks = NULL))

u

tiff("SR_2018_Autumn_0.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
u
dev.off()


z<- ggarrange(p,w,d,u,
              ncol=2,nrow=2,align="v",
              font.label = list(size = 22, face = "bold"),
              common.legend = TRUE) 
z<- annotate_figure(z,left=text_grob(expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), size=24,rot =90))
z

tiff("SR.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
z
dev.off()

## SR_T_2017 -----------
## April
uc<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$T_suelo, data=sr_2017, subset=(Cobertura=="Under canopy" & Season=="April"))  
uc
ci<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$T_suelo, data=sr_2017, subset=(Cobertura=="Canopy interspace" & Season=="April" & Soil_respiration > 0))  
ci

l<-ggplot(data=subset(sr_2017),aes(x=T_suelo)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=subset(sr_2017, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2) + 
  geom_smooth(data=subset(sr_2017, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2,method=lm,se=FALSE) +
  labs(y=expression(paste(Log~Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=" Soil temperature (C)",color="") +
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy")) +
  geom_text(aes(x=45.25, y=1.4), label=paste("r =",round(ci$estimate,2),"***"),color="tan",size=6) +
  geom_text(aes(x=44, y=2), label=paste("r =",round(uc$estimate,2)),color="springgreen4",size=5.7) +
  scale_y_log10(breaks=c(0.02,0.04,0.1,0.2,0.5,1,2),sec.axis=sec_axis(~.*1,name="April 2017", breaks = NULL))+
  scale_x_continuous(breaks=c(25,30,35,40,45))

l
tiff("SR_T_2017_April.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()
## August

uc<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$T_suelo, data=sr_2017, subset=(Cobertura=="Under canopy" & Season=="August" & Soil_respiration > 0))  
uc
ci<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$T_suelo, data=sr_2017, subset=(Cobertura=="Canopy interspace" & Season=="August" & Soil_respiration > 0))  
ci

m<-ggplot(data=subset(sr_2017),aes(x=T_suelo)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=subset(sr_2017, Season=="August"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2) + 
  geom_smooth(data=subset(sr_2017,Season=="August"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2, method=lm,se=FALSE) +
  labs(y=expression(paste(Log~Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=" Soil temperature (C)",color="") +
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy")) +
  geom_text(aes(x=38.5, y=0.35), label=paste("r =",round(ci$estimate,2)),color="tan",size=6) +
  geom_text(aes(x=39, y=0.5), label=paste("r =",round(uc$estimate,2),"*"),color="springgreen4",size=5.7) +
  scale_y_log10(breaks=c(0,0.02,0.04,0.1,0.2,0.5),sec.axis=sec_axis(~.*1,name="August 2017", breaks = NULL))+
  scale_x_continuous(breaks=c(25,30,35,40))
m
tiff("SR_T_2017_August.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
m
dev.off()

## SR_T_2018-----
uc<-cor.test(~log10(sr_2018$Soil_respiration)+sr_2018$T_suelo, data=sr_2018, subset=(Cobertura=="Under canopy" & Season=="April" & Soil_respiration > 0))  
uc
ci<-cor.test(~log10(sr_2018$Soil_respiration)+sr_2018$T_suelo, data=sr_2018, subset=(Cobertura=="Canopy interspace" & Season=="April" & Soil_respiration > 0))  
ci

n<-ggplot(data=subset(sr_2018),aes(x=T_suelo)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=subset(sr_2018, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2) + 
  geom_smooth(data=subset(sr_2018,Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2, method=lm,se=FALSE) +
  labs(y=expression(paste(Log~Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=" Soil temperature (C)",color="") +
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy")) +
  geom_text(aes(x=26, y=0.04), label=paste("r =",round(ci$estimate,2)),color="tan",size=6) +
  geom_text(aes(x=40, y=0.04), label=paste("r =",round(uc$estimate,2),"*"),color="springgreen4",size=5.7) +
  scale_y_log10(breaks=c(0.01,0.02,0.04),sec.axis=sec_axis(~.*1,name="April 2018", breaks = NULL))+
  scale_x_continuous(breaks=c(25,30,35,40,45,50), limits=c(20,50))
n
tiff("SR_T_2018_April.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
n
dev.off()

o<- ggarrange(l,m,n,
              ncol=1,nrow=3,align="v",
              font.label = list(size = 22, face = "bold"),
              common.legend = TRUE)
tiff("SR_T.tiff", width = 6, height = 13.5, units = 'in', res = 300, compression = 'lzw')
o
dev.off()


## SR_PAR_2017--------------

# To calculate p correlation values uc (under canopy) and ci (canopy interspace)
uc<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$PAR, data=sr_2017, subset=(Cobertura=="Under canopy" & Season=="April"))  
uc
ci<-cor.test(~log10(sr_2017$Soil_respiration) +sr_2017$PAR, data=sr_2017, subset=(Cobertura=="Canopy interspace" & Season=="April" & Soil_respiration > 0))
ci
l<-ggplot(data=subset(sr_2017),aes(x=PAR)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=subset(sr_2017, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2) + 
  geom_smooth(data=subset(sr_2017, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2,method=lm,se=FALSE) +
  labs(y=expression(paste(Log~Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Photosynthetic active radiation",color="") +
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy")) +
  geom_text(aes(x=2750, y=1.4), label=paste("r =",round(ci$estimate,2),"***"),color="tan",size=6) +
  geom_text(aes(x=2600, y=2), label=paste("r =",round(uc$estimate,2)),color="springgreen4",size=5.7) +
  scale_y_log10(breaks=c(0.02,0.04,0.1,0.2,0.5,1,2),sec.axis=sec_axis(~.*1,name="April 2017", breaks = NULL))+
  scale_x_continuous(breaks=c(0,1000,2000,3000))
l
tiff("SR_PAR_2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()


## August

uc<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$PAR, data=sr_2017, subset=(Cobertura=="Under canopy" & Season=="August" & Soil_respiration > 0))  
uc
ci<-cor.test(~log10(sr_2017$Soil_respiration)+sr_2017$PAR, data=sr_2017, subset=(Cobertura=="Canopy interspace" & Season=="August" & Soil_respiration > 0))  
ci

m<-ggplot(data=subset(sr_2017),aes(x=PAR)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=subset(sr_2017, Season=="August"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2) + 
  geom_smooth(data=subset(sr_2017,Season=="August"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2, method=lm,se=FALSE) +
  labs(y=expression(paste(Log~Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=" Photosynthetic active radiation",color="") +
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy")) +
  geom_text(aes(x=1400, y=0.45), label=paste("r =",round(ci$estimate,2)),color="tan",size=6) +
  geom_text(aes(x=1400, y=0.6), label=paste("r =",round(uc$estimate,2)),color="springgreen4",size=5.7) +
  scale_y_log10(breaks=c(0,0.02,0.04,0.1,0.2,0.5),sec.axis=sec_axis(~.*1,name="August 2017", breaks = NULL))+
  scale_x_continuous(breaks=c(0,500,1000,1500))
m
tiff("SR_PAR_2017_August.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
m
dev.off()
## SR_PAR 2018---------------
## April
uc<-cor.test(~log10(sr_2018$Soil_respiration)+sr_2018$PAR, data=sr_2018, subset=(Cobertura=="Under canopy" & Season=="April" & Soil_respiration > 0))  
uc
ci<-cor.test(~log10(sr_2018$Soil_respiration) +sr_2018$PAR, data=sr_2018, subset=(Cobertura=="Canopy interspace" & Season=="April" & Soil_respiration > 0))
ci
n<-ggplot(data=subset(sr_2018),aes(x=PAR)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=subset(sr_2018, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2) + 
  geom_smooth(data=subset(sr_2018, Season=="April"),aes(y=Soil_respiration, color=factor(Cobertura)),size=2,method=lm,se=FALSE) +
  labs(y=expression(paste(Log~Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Photosynthetic active radiation",color="") +
  scale_colour_manual(values=c("tan","springgreen4"), labels=c("Outside canopy","Under canopy")) +
  geom_text(aes(x=2150, y=0.035), label=paste("r =",round(ci$estimate,2),"*"),color="tan",size=6) +
  geom_text(aes(x=2200, y=0.03), label=paste("r =",round(uc$estimate,2),"***"),color="springgreen4",size=5.7) +
  scale_y_log10(breaks=c(0.01,0.02,0.03,0.04),sec.axis=sec_axis(~.*1,name="April 2018", breaks = NULL))+
  scale_x_continuous(breaks=c(0,1000,2000,2500))
n
tiff("SR_PAR_2018_April.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
n
dev.off()

p<- ggarrange(l,m,n,
              ncol=1,nrow=3,align="v",
              font.label = list(size = 22, face = "bold"),
              common.legend = TRUE)
tiff("SR_PAR.tiff", width = 6, height = 13.5, units = 'in', res = 300, compression = 'lzw')
p
dev.off()
## New Graphs----

THR2017 <- read_csv("T_HR_LOGGER_2017I.csv")
THR2018 <- read_csv("T_HR_LOGGER_2018I.csv")

# To select some categories inside a column

THR2017b <- filter(THR2017, Hora%in%c("5","9","13","17","21"))
write.csv(THR2017b,file= "THR2017b.csv")
THR2018b <- filter(THR2018, Hora%in%c("05","09","13","17","21"))
write.csv(THR2018b,file= "THR2018b.csv")

# To call the datasets

SR2 <- read_csv("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs/SR2.csv")
THR2017b <- read_csv("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs/THR2017b.csv")
THR2018b <- read_csv("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs/THR2018b.csv")

# To separate the years and seasons

SRAP2017 <- filter(SR2,year== "2017" & Season== "April")
SRAU2017 <- filter(SR2, year=="2017" & Season=="August")
SRAP2018 <- filter(SR2, year=="2018" & Season=="April")

# To change the name of se columns

colnames(SRAP2017)[9]<-"SRAP2017_se"
colnames(SRAU2017)[9]<-"SRAU2017_se"
colnames(SRAP2018)[9]<-"SRAP2018_se"

# To select the columns of SR and THR that will be use

SRAP2017i<-subset(SRAP2017[,c(4,7,9), drop=F])
SRAU2017i<-subset(SRAU2017[,c(4,7,9), drop=F])
SRAP2018i<-subset(SRAP2018[,c(4,7,9), drop=F])

colnames(SRAP2017i)[1]<-"Hora"
colnames(SRAU2017i)[1]<-"Hora"
colnames(SRAP2018i)[1]<-"Hora"

# To merge dataframes

## APRIL 2017----

SRAP2017i <- cbind(SRAP2017i,c("Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro"))
colnames(SRAP2017i)[4]<-"Zona"
X1 <-c(8,7,10,9,2,1,4,3,6,5) 
SRAP2017i <-cbind(X1, SRAP2017i)
SRTHRAP2017 <- merge(THR2017b,SRAP2017i, by="X1")

# Hum_Rel
ci<- cor.test(~SRTHRAP2017$Hum_Rel+SRTHRAP2017$Soil_respiration, data=SRTHRAP2017, subset=Zona.x=="Dentro") ## to find Pearson´s correlation between 2 variables
ci
cu<- cor.test(~SRTHRAP2017$Hum_Rel+SRTHRAP2017$Soil_respiration, data=SRTHRAP2017, subset=Zona.x=="Fuera")
cu

l<-ggplot(data=SRTHRAP2017,aes(x=Hum_Rel)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SRTHRAP2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=2) + 
  geom_smooth(data=SRTHRAP2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=1,method=lm,se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Relative Humidity (%)",color="", title="April 2017") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy","Outside the canopy")) +
  geom_text(aes(x=72, y=0.77), label=paste("r =",round(ci$estimate,2)),color="springgreen4",size=4) +
  geom_text(aes(x=72, y=0.30), label=paste("r =",round(cu$estimate,2)),color="tan",size=4) +
  geom_text(aes(label=Hora.x, y=Soil_respiration),fontface="bold",hjust=-0.3, vjust=-0.4, size=3)
l
tiff("SRHRAP2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()

# Temperature

ci<- cor.test(~SRTHRAP2017$Temperatura+SRTHRAP2017$Soil_respiration, data=SRTHRAP2017, subset=Zona.x=="Dentro") ## to find Pearson´s correlation between 2 variables
ci
cu<- cor.test(~SRTHRAP2017$Temperatura+SRTHRAP2017$Soil_respiration, data=SRTHRAP2017, subset=Zona.x=="Fuera")
cu

l<-ggplot(data=SRTHRAP2017,aes(x=Temperatura)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SRTHRAP2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=2) + 
  geom_smooth(data=SRTHRAP2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=1,method=lm,se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature (C)",color="", title="April 2017") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy","Outside the canopy")) +
  geom_text(aes(x=32.5, y=0.90), label=paste("r =",round(ci$estimate,2)),color="springgreen4",size=4) +
  geom_text(aes(x=32.5, y=0.25), label=paste("r =",round(cu$estimate,2)),color="tan",size=4) +
  geom_text(aes(label=Hora.x, y=Soil_respiration),fontface="bold",hjust=-0.3, vjust=-0.4, size=3)
l
tiff("SRTAP2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()

## AUGUST 2017-----

SRAU2017i <- cbind(SRAU2017i,c("Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro"))
colnames(SRAU2017i)[4]<-"Zona"
X1 <-c(8,7,10,9,2,1,4,3,6,5) 
SRAU2017i <-cbind(X1, SRAU2017i)
SRTHRAU2017 <- merge(THR2017b,SRAU2017i, by="X1")

# Hum_Rel
ci<- cor.test(~SRTHRAU2017$Hum_Rel+SRTHRAU2017$Soil_respiration, data=SRTHRAU2017, subset=Zona.x=="Dentro") ## to find Pearson´s correlation between 2 variables
ci
cu<- cor.test(~SRTHRAU2017$Hum_Rel+SRTHRAU2017$Soil_respiration, data=SRTHRAU2017, subset=Zona.x=="Fuera")
cu

l<-ggplot(data=SRTHRAU2017,aes(x=Hum_Rel)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SRTHRAU2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=2) + 
  geom_smooth(data=SRTHRAU2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=1,method=lm,se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Relative Humidity (%)",color="", title="August 2017") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy","Outside the canopy")) +
  geom_text(aes(x=75, y=0.20), label=paste("r =",round(ci$estimate,2)),color="springgreen4",size=4) +
  geom_text(aes(x=75, y=0.08), label=paste("r =",round(cu$estimate,2)),color="tan",size=4) +
  geom_text(aes(label=Hora.x, y=Soil_respiration),fontface="bold",hjust=-0.3, vjust=-0.4, size=3)
l
tiff("SRHRAU2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()

# Temperature

ci<- cor.test(~SRTHRAU2017$Temperatura+SRTHRAU2017$Soil_respiration, data=SRTHRAU2017, subset=Zona.x=="Dentro") ## to find Pearson´s correlation between 2 variables
ci
cu<- cor.test(~SRTHRAU2017$Temperatura+SRTHRAU2017$Soil_respiration, data=SRTHRAU2017, subset=Zona.x=="Fuera")
cu

l<-ggplot(data=SRTHRAU2017,aes(x=Temperatura)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SRTHRAU2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=2) + 
  geom_smooth(data=SRTHRAU2017,aes(y=Soil_respiration, color=factor(Zona.x)),size=1,method=lm,se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature (C)",color="", title="August 2017") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy","Outside the canopy")) +
  geom_text(aes(x=30, y=0.10), label=paste("r =",round(ci$estimate,2)),color="springgreen4",size=4) +
  geom_text(aes(x=30, y=0.05), label=paste("r =",round(cu$estimate,2)),color="tan",size=4) +
  geom_text(aes(label=Hora.x, y=Soil_respiration),fontface="bold",hjust=-0.3, vjust=-0.4, size=3)
l
tiff("SRTAU2017.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()

## APRIL 2018 ----

SRAP2018i <- cbind(SRAP2018i,c("Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro","Fuera", "Dentro"))
colnames(SRAP2018i)[4]<-"Zona"
X1 <-c(2,1,4,3,6,5,8,7,10,9) 
SRAP2018i <-cbind(X1, SRAP2018i)
SRTHRAP2018 <- merge(THR2018b,SRAP2018i, by="X1")

# Hum_Rel
ci<- cor.test(~SRTHRAP2018$Hum_Rel+SRTHRAP2018$Soil_respiration, data=SRTHRAP2018, subset=Zona.x=="Dentro") ## to find Pearson´s correlation between 2 variables
ci
cu<- cor.test(~SRTHRAP2018$Hum_Rel+SRTHRAP2018$Soil_respiration, data=SRTHRAP2018, subset=Zona.x=="Fuera")
cu

l<-ggplot(data=SRTHRAP2018,aes(x=Hum_Rel)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SRTHRAP2018,aes(y=Soil_respiration, color=factor(Zona.x)),size=2) + 
  geom_smooth(data=SRTHRAP2018,aes(y=Soil_respiration, color=factor(Zona.x)),size=1,method=lm,se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Relative Humidity (%)",color="", title="April 2018") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy","Outside the canopy")) +
  geom_text(aes(x=69, y=0.0072), label=paste("r =",round(ci$estimate,2)),color="springgreen4",size=4) +
  geom_text(aes(x=69, y=0.0098), label=paste("r =",round(cu$estimate,2)),color="tan",size=4) +
  geom_text(aes(label=Hora.x, y=Soil_respiration),fontface="bold",hjust=-0.3, vjust=-0.4, size=3)
l
tiff("SRHRAP2018.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()

# Temperature

ci<- cor.test(~SRTHRAP2018$Temperatura+SRTHRAP2018$Soil_respiration, data=SRTHRAP2018, subset=Zona.x=="Dentro") ## to find Pearson´s correlation between 2 variables
ci
cu<- cor.test(~SRTHRAP2018$Temperatura+SRTHRAP2018$Soil_respiration, data=SRTHRAP2018, subset=Zona.x=="Fuera")
cu

l<-ggplot(data=SRTHRAP2018,aes(x=Temperatura)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SRTHRAP2018,aes(y=Soil_respiration, color=factor(Zona.x)),size=2) + 
  geom_smooth(data=SRTHRAP2018,aes(y=Soil_respiration, color=factor(Zona.x)),size=1,method=lm,se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature (C)",color="", title="April 2018") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy","Outside the canopy")) +
  geom_text(aes(x=32, y=0.0125), label=paste("r =",round(ci$estimate,2)),color="springgreen4",size=4) +
  geom_text(aes(x=32, y=0.0095), label=paste("r =",round(cu$estimate,2)),color="tan",size=4) +
  geom_text(aes(label=Hora.x, y=Soil_respiration),fontface="bold",hjust=-0.3, vjust=-0.4, size=3)
l
tiff("SRTAP2018.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
l
dev.off()

## Soil humidity graphs --------------------

soil_humidity <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Humedad de suelo_Análisis.xlsx")
colnames(soil_humidity)[10]<-"SH"
soil_humidity<-soil_humidity[-1, ,drop=F]

SHC <- summarySE(soil_humidity, measurevar="SH", groupvars=c("Cobertura","Year"), na.rm=TRUE)

SHC[1,1]<- "Under canopy"
SHC[2,1]<- "Under canopy"
SHC[3,1]<- "Outside canopy"
SHC[4,1]<- "Outside canopy"

Q<-ggplot(SHC, aes(x=Cobertura, y=SH, fill=Cobertura)) +
  Tema + theme(legend.position = "none" ) + labs(x="") +
  geom_errorbar(aes(ymin=SH-se, ymax=SH+se),
                width=0.2,                    # Width of the error bars
                position=position_dodge(0.7)) + 
  facet_grid(~Year,space="free_x") +
  geom_bar(width = 0.65, position=position_dodge(), stat="identity") +
  scale_fill_manual("Cobertura",
                       values = c("Outside canopy"= "tan","Under canopy" = "springgreen4", "Outside canopy"= "tan","Under canopy" = "springgreen4"), drop=FALSE) +
  scale_y_continuous(name="Soil Humidity (%)",expand=c(0,0), limits=c(0,1.5))
Q
tiff("Soil_Humidity_2017&2018.tiff", width = 7.5, height = 3, units = 'in', res = 300, compression = 'lzw')
Q
dev.off()

## t-test to see if little values are 0 (they are)

sr_2018 <- read_csv("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs/sr_2018.csv")
ttSH3 <- t.test(sr_2018$Soil_respiration, mu=0) 

capture.output(ttSH3,file="t-test_SR", append=TRUE) ## to save an output of a test
ttSH3

## Q10----------

sr_2017_2018 <- read_csv("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs/sr_2017_2018.csv")
sr_2017_2018 <- read_csv("C:/Users/FISICA/Dropbox/Respiracion de suelo/Outputs/sr_2017_2018.csv")

lm <- lmList(log10(Soil_respiration)~T_suelo|Punto_de_muestreo,pool=FALSE,data=subset(sr_2017_2018, 
              year=="2017" & Season=="April"))
lm
coe <-coef(lm)
write.csv(coe,file= "lm.csv")
D1<-summary(lm)
D3<-as.data.frame(D1$r.squared)
D2<-cbind(D1$coefficients,D3)

a<-10^(coe$T_suelo*10)
a
Punto<-c(1:30)
Q10 <- cbind(Punto,a)
Q10 <- cbind(Q10,D1$r.squared)
Q10
SH<-subset(soil_humidity,Year=="2017")
SH<-merge(Q10,SH,by="Punto") ## to combine all the dataframe even thought they are different, add: all=TRUE

SH1<-subset(SH, V3>0.42)

Anova<-aov(a~Cobertura,data=SH)
summary(Anova)

# SH<-SH[-1, ,drop=F]

A<-ggplot(SH1,aes(x=SH)) +
  Tema + theme(legend.position = "top") +
              geom_point(data=SH1,aes(y=a, color=factor(Cobertura)),size=2) + 
  scale_colour_manual(values=c("springgreen4","tan"))+
  scale_fill_discrete(name = "") +
  geom_smooth(data=SH1,aes(y=a, color=factor(Punto)),size=1,method=loess,se=FALSE)+
  labs(y="Q10", x="Soil humidity (%)",color="", title="April 2017") +
  labs(caption="Points discriminated by R2>0.42") +
  scale_colour_manual(values=c("springgreen4","tan"), labels=c("Under canopy",
                                                               "Outside the canopy"))
A
tiff("Q10_SH_R2.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
A
dev.off()
  
Corr <- cor.test(SH$SH,SH$a, data=SH, subset=(Cobertura=="Dentro"))
Corr
plot(SH$a~SH$SH)

Corr1 <- cor.test(SH$SH,SH$a, data=SH, subset=(Cobertura=="Fuera"))
Corr1

capture.output(Corr,file="Corr_SH_a", append=TRUE)

## Soil respiration global-----------------

SR <- read_csv("sr_2017_2018_complete.csv")
SR_DB <- read_csv("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/SR_DB.csv") ## PC P
SR_DB <- read_csv("E:/Dropbox/Respiracion de suelo/Data/SR_DB.csv") ## Laptop P

Tema <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", size=1,
                                          linetype= "solid", fill=NA),
              axis.text=element_text(size=20,colour="black"),
              #axis.text.x= element_text(face ="bold"),
              axis.title=element_text(size=24),
              legend.text=element_text(size=24),
              legend.title=element_text(size=24))

hist(SR_DB$Rs_summer)
hist(SR_DB$Rs_winter)
hist(SR$Soil_respiration)
names(SR_DB)
hist(SR_DB$MAP)
hist(SR_DB$MAT)
hist(SR_DB$Rs_annual)

m<-ggplot(data=SR_DB,aes(x=Study_temp)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=filter(SR_DB,Rs_annual<4000),aes(y=Rs_annual),size=2) + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature",color="", title="Global database")
m

m<-ggplot(data=SR_DB,aes(x=MAT)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SR_DB,aes(y=Rs_summer),size=2) + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature",color="", title="Global database")
m

m<-ggplot(data=SR_DB,aes(x=MAT)) + 
  Tema + theme(legend.position = "top",axis.title.y = element_text(size=12),axis.title.x = element_text(size=12)) +
  geom_point(data=SR_DB,aes(y=Rs_winter),size=2) + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature",color="", title="Global database")
m


loc<-cbind(SR_DB$Rs_summer,SR_DB$MAT)
loc<-as.data.frame(loc)
loc<-na.omit(loc, cols="V1")
colnames(loc)<-c("SrSum","MAT")
gb<-"GB"
gb<-rep(gb, times = 106)
loc<-cbind(gb,loc)
SR_summer<-filter(SR, Season=="April")
SR_summer<-SR_summer[,c(2,8,10),drop=F]
names(SR_summer)
colnames(SR_summer)<-c("GB","SrSum","MAT")
colnames(loc)<-c("GB","SrSum","MAT")
SR_summer$GB<-as.factor(SR_summer$GB)
loc<-rbind(loc,SR_summer)
names(loc)
m<-ggplot(data=loc,aes(x=MAT,y=SrSum, color=GB)) + 
  Tema + theme(legend.position = "bottom",axis.title.y = element_blank(),axis.text = element_text(size=24)) +
  geom_point(size=2) + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature (C)",color="") +
  scale_colour_manual(name = "", values=c("black","blue","red"), labels=c("global","2017","2018"))+
  annotate('text', x = 40, y = 7, label="Summer",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,8))
m
tiff("Global_summer.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
m
dev.off()


loc2<-cbind(SR_DB$Rs_winter,SR_DB$MAT)
loc2<-as.data.frame(loc2)
loc2<-na.omit(loc2, cols="V1")
colnames(loc2)<-c("SrWin","MAT")
gb<-"GB"
gb<-rep(gb, times = 142)
loc2<-cbind(gb,loc2)
SR_winter<-filter(SR, Season=="August")
SR_winter<-SR_winter[,c(2,8,10),drop=F]
names(SR_winter)
colnames(SR_winter)<-c("GB","SrWin","MAT")
colnames(loc2)<-c("GB","SrWin","MAT")
SR_winter$GB<-as.factor(SR_winter$GB)
loc2<-rbind(loc2,SR_winter)
names(loc2)
n<-ggplot(data=loc2,aes(x=MAT)) + 
  Tema + theme(legend.position = "bottom",axis.title.y=element_blank(),axis.text.x = element_text(size=24),axis.text.y=element_blank()) +
  geom_point(data=loc2,aes(y=SrWin, color=GB),size=2) + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x="Temperature (C)",color="") +
  scale_colour_manual(name="",values=c("black","blue","red"), labels=c("global","2017","2018"))+
  annotate('text', x = 40, y = 7, label="Winter",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,8))
n
tiff("Global_winter.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
n
dev.off()

p<-ggarrange(m,n, nrow=1, ncol=2, align="v", common.legend=TRUE,legend="bottom",
             font.label = list(size = 22, face = "bold"),labels = c("A)","B)"),
             hjust=c(-1.5,-1.5))
p
tiff("Global3.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
annotate_figure(p,left=text_grob(expression(paste(Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), size=24,rot =90))
dev.off()



## To include tree size here --------------

SR_summer2<-filter(SR, year=="2017",Cobertura=="Under canopy")
SR_summer2<-SR_summer2[,c(2,7,8,10),drop=F]
names(SR_summer2)
colnames(SR_summer2)<-c("year","cover","SrSum","temperature")
SR_summer2$year<-as.factor(SR_summer2$year)
SR_size <- read_excel("E:/Dropbox/Respiracion de suelo/Data/Datos_dasometricos.xlsx") 
SR_size <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Datos_dasometricos.xlsx") ## Lab E 
SR_summer2$canopy<-SR_size$`Canopy area`
ID<-c(1:20)
SR_summer2$ID<-ID
SR_summer2$ID<-as.factor(SR_summer2$ID)
o<-ggplot(data=SR_summer2,aes(x=canopy,y=SrSum, color=year)) + 
  Tema + theme(legend.position = "none",axis.title.y = element_blank(),axis.text.x = element_blank(),axis.title.x=element_blank()) +
  geom_point() + 
  geom_smooth(aes(canopy,SrSum), method=lm, se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=expression(paste(Canopy~area~~group("(",m^2,")"))),color="") +
  scale_colour_manual(name = "", values="blue", labels="2017")+
  #annotate('text', x = 125, y = 2.3, label="2017",size = 12, color = "black") +
  annotate('text', x = 108, y = 1, label="r = 0.17*",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,2.5))
o

cor.test(SR_summer2$canopy,SR_summer2$SrSum)  
w1<-rmcorr(ID,canopy,SrSum,SR_summer2) ## Repetead measurement correlation
w2<-rmcorr(ID,temperature,SrSum,SR_summer2)
summary(SR_summer2)

w2 <- rmcorr(ID,temperature, canopy , SR_summer2)

oo<-ggplot(data=SR_summer2,aes(x=temperature,y=SrSum, color=year)) + 
  Tema + theme(legend.position = "none",axis.title = element_blank(),axis.text = element_blank()) +
  geom_point() + 
  geom_smooth(aes(temperature,SrSum), method=lm, se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=expression(paste(Soil~temperature~~group("(","C",")"))),color="") +
  scale_colour_manual(name = "", values="blue", labels="2017")+
  #annotate('text', x = 35, y = 2.3, label="2017",size = 12, color = "black") +
  annotate('text', x = 30, y = 1.5, label="r = 0.43***",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,2.5))
oo

cor.test(SR_summer2$temperature,SR_summer2$SrSum) 

SR_winter2<-filter(SR, year=="2018",Cobertura=="Under canopy")
SR_winter2<-SR_winter2[,c(2,7,8,10),drop=F]
names(SR_winter2)
colnames(SR_winter2)<-c("year","cover","SrSum","temperature")
SR_winter2$year<-as.factor(SR_winter2$year)
SR_size <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Datos_dasometricos.xlsx") 
SR_winter2$canopy<-SR_size$`Canopy area`
q<-ggplot(data=SR_winter2,aes(x=canopy,y=SrSum, color=year)) + 
  Tema + theme(legend.position = "none",axis.title.y=element_blank(),axis.text.x = element_text(size=24)) +
  geom_point() + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=expression(paste(Canopy~area~~group("(",m^2,")"))),color="") +
  scale_colour_manual(name = "", values="red", labels="2018")+
  #scale_size_continuous(name = "canopy size")+
  geom_smooth(aes(canopy,SrSum), method=lm, se=FALSE)+
  #annotate('text', x = 125, y = 2.3, label="2018",size = 12, color = "black") +
  annotate('text', x = 105, y = 0.1, label="r = -0.02",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,0.2))
q

cor.test(SR_winter2$canopy,SR_winter2$SrSum)  

qq<-ggplot(data=SR_winter2,aes(x=temperature,y=SrSum, color=year)) + 
  Tema + theme(legend.position = "none",axis.title.y=element_blank(),axis.text.x = element_text(size=24),axis.text.y = element_blank()) +
  geom_point() + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=expression(paste(Soil~Temperature~~group("(",C,")"))),color="") +
  scale_colour_manual(name = "", values="red", labels="2018")+
  #scale_size_continuous(name = "canopy size")+
  geom_smooth(aes(temperature,SrSum), method=lm, se=FALSE)+
   #annotate('text', x = 40, y = 0.18, label="2018",size = 12, color = "black") +
   annotate('text', x = 37, y = 0.1, label="r = -0.2**",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,0.2))
qq

cor.test(SR_winter2$temperature,SR_winter2$SrSum)  

SR_winter2$ID<-ID
SR_winter2$ID<-as.factor(SR_winter2$ID)
w<-rmcorr(ID,canopy,SrSum,SR_winter2) ## Repetead measurement correlation
w<-rmcorr(ID,temperature,SrSum,SR_winter2)

p<-ggplot(data = SR_SH_2017,aes(x = SH, y = SR, color = Year)) + 
  Tema + theme(legend.position = "none",axis.title = element_blank(),axis.text = element_blank()) +
  geom_point() +
  geom_smooth(aes(x = SH, y = SR), method=lm, se=FALSE) +
  labs(y="", x="",color="")+
  scale_colour_manual(name = "", values="blue", labels="2017") +
  #annotate('text', x = 35, y = 2.3, label="2017",size = 12, color = "black") 
  annotate('text', x = 2.2, y = 1.5, label="r = 0.68***",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,2.5))
p

pp<-ggplot(data=SR_SH_2018,aes(x=SH,y=SR, color=Year)) + 
  Tema + theme(legend.position = "none",
               axis.title.y=element_blank(),
               axis.text.x = element_text(size=24),
               axis.text.y = element_blank()) +
  geom_point() + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))),
       x=expression(paste(Soil~Humidity~group("(",%,")"))),color="") +
  #scale_colour_manual(name = "", values="red", labels="2018")+
  geom_smooth(aes(x=SH, y=SR), method=lm, se=FALSE) +
  #annotate('text', x = 40, y = 0.18, label="2018",size = 12, color = "black") +
  #annotate('text', x = 37, y = 0.1, label="r = 0.004",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,0.2))
pp

# plot
pp<-ggplot(data = SR_SH_2018, aes(x = SH, y = SR)) +
  geom_point(color = "red") +
  geom_smooth(aes(x = SH, y = SR), method = lm, se = FALSE, color = "red") +
  scale_y_continuous(limits=c(0,0.2)) +
  annotate('text', x = 1.1, y = 0.05, label="r = 0.004",size = 10, color = "black") +
  labs(x = "Soil Humidity (%)") +
  Tema + 
  theme(legend.position = "none",
               axis.title.y=element_blank(),
               axis.text.x = element_text(size=24),
               axis.text.y = element_blank())
pp
  
r<-ggarrange(o,oo,p,q,qq,pp, nrow=2, ncol=3, align="hv", common.legend=TRUE,legend="none",
             font.label = list(size = 22, face = "bold"),labels = c("A)","B)","C)","D)", "E)", "F)"),
             hjust=c(-2.5,-2.5,-2.5,-2.5))
r
tiff("Fig3ASE.tiff", width = 18, height = 9, units = 'in', res = 300, compression = 'lzw')
annotate_figure(r,left=text_grob(expression(paste(~~~~~~~~~~~~Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")")~~~~~~~~~~~~Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), size=24,rot =90),
                right=text_grob(expression("2017                                   2018          "), size=24,rot =-90))
dev.off()

w<-ggarrange(m,n,o,q, nrow=2, ncol=2, align="hv", common.legend=TRUE,legend="top",
             font.label = list(size = 22, face = "bold"),labels = c("A)","B)","C)","D)"),
             hjust=c(-2.5,-2.5,-2.5,-2.5))
w

tiff("Figu4.tiff", width = 12, height = 9, units = 'in', res = 300, compression = 'lzw')
annotate_figure(w,left=text_grob(expression(paste(~~~~Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")")~~~~~~~~~~Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), size=24,rot =90))
dev.off()

## Temperature and Rs outside canopy -----

cor.test(~Soil_respiration + T_suelo, data = SR, subset=(year=="2017"|Cobertura=="Canopy interspace"))  
cor.test(~Soil_respiration + T_suelo, data = SR, subset=(year=="2018"|Cobertura=="Canopy interspace"))  

## SR and Soil humidity
sr_2017 <- read_csv("sr_2017.csv")
sr_2017_<- subset(sr_2017[sr_2017$Season=="April",, drop=F])
sr_2017_ <- summarySE(sr_2017_, measurevar="Soil_respiration", groupvars="Punto_de_muestreo",na.rm=TRUE)

sr_2018 <- read_csv("sr_2018.csv")
sr_2018 <- summarySE(sr_2018, measurevar="Soil_respiration", groupvars="Punto_de_muestreo",na.rm=TRUE)

Hum_slo <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Hum_slo.xlsx") ##Lab E
Hum_slo<- subset(Hum_slo[,c(1,2,3,10), drop=F])
Hum_slo_2017<-subset(Hum_slo[Hum_slo$Year==2017,, drop=F])
colnames(Hum_slo_2017)[1]<-"Punto_de_muestreo"
Hum_slo_2018<-subset(Hum_slo[Hum_slo$Year==2018,, drop=F])
colnames(Hum_slo_2018)[1]<-"Punto_de_muestreo"

## To  combine 2 dataframes with different number of data
SR_SH_2017 <-merge(x=sr_2017_, y=Hum_slo_2017, by="Punto_de_muestreo", all=TRUE)
colnames(SR_SH_2017)[3]<-"SR"
colnames(SR_SH_2017)[9]<-"SH"
SR_SH_2017$Year <- as.factor(SR_SH_2017$Year)
SR_SH_2018 <-merge(x=sr_2018, y=Hum_slo_2018, by="Punto_de_muestreo", all=TRUE)
colnames(SR_SH_2018)[3]<-"SR"
colnames(SR_SH_2018)[9]<-"SH"
SR_SH_2018$Year <- as.factor(SR_SH_2018$Year)
cor.test(SR_SH_2017$SR,SR_SH_2017$SH, method="pearson")
g<-ggplot(SR_SH_2017,aes(x=SH,y=SR)) +
  geom_point() +
  geom_smooth(method=lm)  
  
cor.test(SR_SH_2018$SR,SR_SH_2018$SH, method="pearson")
f<-ggplot(SR_SH_2018,aes(x=SH,y=SR)) +
  geom_point() +
  geom_smooth(method=lm)
f
