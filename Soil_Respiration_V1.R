## Pablo Salazar 
## Elva Palacios Mccubbin 
## Universidad de Piura
## Soil respiration data analysis
## 31/05/2018


library(readxl) ## To read the excel file
library(readr) ## To read csv file
library(plyr) ## To use SummarySE
library(dplyr) ## To use SummarySE
library(ggplot2) ## To make ggplot graphs (you need ggpubr too)
library(ggthemes) ## To modify features in graphs
library(ggpubr) ## to make ggplot graphs (you need ggplot2 too)
library(xts) ## To get daily values
library (nlme)
library(rmcorr) ## for repetead measurement correlation 


setwd("E:/Dropbox/Respiracion de suelo/Outputs")## Laptop P
setwd("C:/Users/FISICA/Dropbox/Respiracion de suelo/Outputs")## Lab p
setwd("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Outputs")## Lab E

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
## SR_ENSO_non-ENSO-----

sr_2017 <- read_excel("/Users/RStudio/Documents/Dropbox/SoilResp/Data/Data_v2/soil_resp2017.xlsx", 
                      sheet = "SR)") ## Oficina E
# sr_2017 <- read_excel("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/soil respiration 2017 A2.xlsx", 
#                       sheet = "SR)") ## oficina P
# sr_2017 <- read_excel("E:/Dropbox/Respiracion de suelo/Data/soil respiration 2017 A2.xlsx", 
#                      sheet = "SR)" ## laptop P
# sr_2017<- sr_2017[c(151:300),,drop=F ]
Tiempo <- as.POSIXct(sr_2017$Tiempo, format="%Y-%m-%d %H:%M:%S")
Hora <- format(sr_2017$Tiempo,"%H:%M")
sr_2017 <- cbind(sr_2017, Hora)
sr_2017 <- subset(sr_2017[,-9,drop=F])
colnames(sr_2017)[1] <- "Codigo"
# write.csv(sr_2017, file = "/Users/RStudio/Documents/Dropbox/Respiración de suelo/Data/Data_v2/sr_2017.csv")

# sr_2017 <- read_csv("sr_2017.csv")

year <- rep("2017",time=300)
sr_2017 <-cbind(year,sr_2017)
# sr_2017 <- sr_2017[,-(2),drop=FALSE] 

sr_2018 <-read_excel("/Users/RStudio/Documents/Dropbox/SoilResp/Data/Data_v2/soil_resp2018.xlsx", 
                     sheet = "2018_I") ## Laptop P

# sr_2018 <-read_excel("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/soil respiration 2018 I.xlsx", 
#                      sheet = "2018_I") ## oficina P
# 
# sr_2018 <-read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/soil respiration 2018 I.xlsx", 
#                      sheet = "2018_I") ## oficina E

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

write.csv(sr_2017_2018,file= "/Users/RStudio/Documents/Dropbox/SoilResp/Data/Data_v2/soil_resp_17&18.csv")

SR2 <- summarySE(sr_2017_2018, measurevar="Soil_respiration", 
                 groupvars=c("year","Season","Iluminacion","Cobertura"),na.rm=TRUE)
names(SR2)
write.csv(SR2,file= "SR2.csv")

# SR comparisson between 2017 and 2018 -------------------

sr_2017_2018 <- read_csv("sr_2017_2018_complete.csv")

SRC <- summarySE(filter(sr_2017_2018,year=="2017"), measurevar="Soil_respiration", 
                 groupvars=c("Season","Cobertura"),na.rm=TRUE)

Tema <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", size=1,
                                          linetype= "solid", fill=NA),
              axis.text=element_text(size=20,colour="black"),
              #axis.text.x= element_text(face ="bold"),
              axis.title=element_text(size=24),
              legend.text=element_text(size=24),
              legend.title=element_text(size=24))

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
  scale_fill_manual(values = c("Under canopy" = "springgreen4", "Canopy interspace"= "tan"),
                    labels=c("Canopy interspace"="Outside canopy","Under canopy"="Under canopy"))
R
tiff("SR_2017.tiff", width = 9, height = 4.5, units = 'in', res = 300, compression = 'lzw')
R
dev.off()

SRC1 <- summarySE(filter(sr_2017_2018, year=="2018"), measurevar="Soil_respiration", groupvars=c("Season","Cobertura"),na.rm=TRUE)

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

tiff("Fig2A&B.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
S
dev.off()

## Soil respiration global-----------------

SR <- read_csv("sr_2017_2018_complete.csv")
SR_DB <- read_csv("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/SR_DB.csv") ## PC P
SR_DB <- read_csv("E:/Dropbox/Respiracion de suelo/Data/SR_DB.csv") ## Laptop P

# Summer global + piura 
loc<-cbind(SR_DB$Rs_summer,SR_DB$MAT)
loc<-as.data.frame(loc)
loc<-na.omit(loc, cols="V1")
colnames(loc)<-c("SrSum","MAT")
gb<-"GB"
gb<-rep(gb, times = 106)
loc<-cbind(gb,loc)
SR_summer<-filter(SR, Season=="April")
SR_summer<-SR_summer[,c(2,8,10),drop=F]
colnames(SR_summer)<-c("GB","SrSum","MAT")
colnames(loc)<-c("GB","SrSum","MAT")
SR_summer$GB<-as.factor(SR_summer$GB)
loc<-rbind(loc,SR_summer)

# Summer
m<-ggplot(data=loc,aes(x=MAT,y=SrSum, color=GB)) + 
  Tema + theme(legend.position = "bottom",
               axis.title.y = element_blank(),
               axis.text = element_text(size=24)) +
  geom_point(size=2) + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), 
       x="Temperature (C)",color="") +
  scale_colour_manual(name = "", values=c("black","blue","red"), labels=c("global","2017","2018"))+
  annotate('text', x = 40, y = 7, label="Summer",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,8))
m

## --------------------------------------------
# tiff("Global_summer.tiff", width = 6, height = 4.5, units = 'in', res = 300, compression = 'lzw')
# m
# dev.off()


# Winter global + piura
loc2<-cbind(SR_DB$Rs_winter,SR_DB$MAT)
loc2<-as.data.frame(loc2)
loc2<-na.omit(loc2, cols="V1")
colnames(loc2)<-c("SrWin","MAT")
gb<-"GB"
gb<-rep(gb, times = 142)
loc2<-cbind(gb,loc2)
SR_winter<-filter(SR, Season=="August")
SR_winter<-SR_winter[,c(2,8,10),drop=F]
colnames(SR_winter)<-c("GB","SrWin","MAT")
colnames(loc2)<-c("GB","SrWin","MAT")
SR_winter$GB<-as.factor(SR_winter$GB)
loc2<-rbind(loc2,SR_winter)

# Winter
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
             font.label = list(size = 22, face = "bold"), labels = c("A)","B)"),
             hjust=c(-1.5,-1.5))
p

tiff("Figure2C&D.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
annotate_figure(p,left=text_grob(expression(paste(Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), size=24,rot =90))
dev.off()


## SR correlation with independent factors --------------

## SR and Soil humidity
sr_2017 <- read_csv("sr_2017.csv")
sr_2017 <- summarySE(sr_2017, measurevar="Soil_respiration", groupvars="Punto_de_muestreo",na.rm=TRUE)

sr_2018 <- read_csv("sr_2018.csv")
sr_2018 <- summarySE(sr_2018, measurevar="Soil_respiration", groupvars="Punto_de_muestreo",na.rm=TRUE)

Hum_slo <- read_excel("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/Hum_slo.xlsx") ##Lab P
Hum_slo<- subset(Hum_slo[,c(1,2,3,10), drop=F])
Hum_slo_2017<-subset(Hum_slo[Hum_slo$Year==2017,, drop=F])
colnames(Hum_slo_2017)[1]<-"Punto_de_muestreo"
Hum_slo_2018<-subset(Hum_slo[Hum_slo$Year==2018,, drop=F])
colnames(Hum_slo_2018)[1]<-"Punto_de_muestreo"

## To  combine 2 dataframes with different number of data
SR_SH_2017 <-merge(x=sr_2017, y=Hum_slo_2017, by="Punto_de_muestreo", all=TRUE)
colnames(SR_SH_2017)[3]<-"SR"
colnames(SR_SH_2017)[9]<-"SH"
SR_SH_2017$Year <- as.factor(SR_SH_2017$Year)
SR_SH_2018 <-merge(x=sr_2018, y=Hum_slo_2018, by="Punto_de_muestreo", all=TRUE)
colnames(SR_SH_2018)[3]<-"SR"
colnames(SR_SH_2018)[9]<-"SH"
SR_SH_2018$Year <- as.factor(SR_SH_2018$Year)











## Correlation with canopy area --------------------

SR_summer2<-filter(SR, year=="2017",Cobertura=="Under canopy")
SR_summer2<-SR_summer2[,c(2,7,8,10),drop=F]
colnames(SR_summer2)<-c("year","cover","SrSum","temperature")
SR_summer2$year<-as.factor(SR_summer2$year)

SR_size <- read_excel("C:/Users/FISICA/Dropbox/Respiracion de suelo/Data/Datos_dasometricos.xlsx") ## Lab P
SR_size <- read_excel("C:/Users/Laboratorio/Dropbox/Respiracion de suelo/Data/Datos_dasometricos.xlsx")## Lab E 


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
w1 <- rmcorr(ID,canopy,SrSum,SR_summer2) ## Repetead measurement correlation
w2 <- rmcorr(ID,temperature,SrSum,SR_summer2)
w2 <- rmcorr(ID,temperature, canopy , SR_summer2)

oo<-ggplot(data=SR_summer2,aes(x=temperature,y=SrSum, color=year)) + 
  Tema + theme(legend.position = "none",axis.title = element_blank(),axis.text = element_blank()) +
  geom_point() + 
  geom_smooth(aes(temperature,SrSum), method=lm, se=FALSE)+
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), 
       x=expression(paste(Soil~temperature~~group("(","C",")"))),color="") +
  scale_colour_manual(name = "", values="blue", labels="2017")+
  #annotate('text', x = 35, y = 2.3, label="2017",size = 12, color = "black") +
  annotate('text', x = 30, y = 1.5, label="r = 0.43***",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,2.5))
oo

cor.test(SR_summer2$temperature,SR_summer2$SrSum) 

SR_winter2<-filter(SR, year=="2018",Cobertura=="Under canopy")
SR_winter2<-SR_winter2[,c(2,7,8,10),drop=F]
colnames(SR_winter2)<-c("year","cover","SrSum","temperature")
SR_winter2$year<-as.factor(SR_winter2$year)
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

qq <- ggplot(data=SR_winter2,aes(x=temperature,y=SrSum, color=year)) + 
  Tema + theme(legend.position = "none",axis.title.y=element_blank(),axis.text.x = element_text(size=24),axis.text.y = element_blank()) +
  geom_point() + 
  labs(y=expression(paste(Soil~respiration~rate~~group("(",mu~molCO[2]~m^-2~s^-1,")"))), x=expression(paste(Soil~Temperature~~group("(",C,")"))),color="") +
  scale_colour_manual(name = "", values="red", labels="2018")+
  geom_smooth(aes(temperature,SrSum), method=lm, se=FALSE)+
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
  annotate('text', x = 2.2, y = 1.5, label="r = 0.68***",size = 10, color = "black") +
  scale_y_continuous(limits=c(0,2.5))
p

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
  
r<-ggarrange(o,oo,p,q,qq,pp, nrow=2, ncol=3, 
             align="hv", common.legend=TRUE,legend="none",
             font.label = list(size = 22, face = "bold"),labels = c("A)","B)","C)","D)", "E)", "F)"),
             hjust=c(-2.5,-2.5,-2.5,-2.5))
r
tiff("Fig3.tiff", width = 18, height = 9, units = 'in', res = 300, compression = 'lzw')
annotate_figure(r,left=text_grob(expression(paste(~~~~~~~~~~~~Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")")~~~~~~~~~~~~Rs~~group("(",mu~molCO[2]~m^-2~s^-1,")"))),
                                 size=24,rot =90),
                right=text_grob(expression("2017                                   2018          "), size=24,rot =-90))
dev.off()
