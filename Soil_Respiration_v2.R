## Pablo Salazar 
## Elva Palacios Mccubbin 
## Universidad de Piura
## Soil respiration data analysis
## 31/05/2018


# Libraries
library(dplyr) # to summarise the data x
library(ggplot2) # to visualize the data
library(ggpubr)  
library(ggthemes) # to add some aesthetics to the ggplots
library(plyr) # to use function ddply for SummarySE x
library(readr) ## To read csv file
library(readxl) # to read excel files
library(rmcorr) ## for repetead measurement correlation 

# Data
soilResp <- read.csv(".../Data/soil_resp_17&18.csv") # csv file cointaing soil respiration data for 2 years: 2017 and 2018

# Created functions for a summarising analysis
## SummarySE function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function(x, na.rm=FALSE) {
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

# Soil respiration comparisson between 2017 and 2018 
SRC <- summarySE(filter(sr_2017_2018, year == "2017"),
                 measurevar = "Soil_respiration",
                 groupvars = c("Season","Cobertura"),
                 na.rm = TRUE)

custom_theme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),  ## delete background
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      panel.border = element_rect(colour = "black", size=1, linetype = "solid", fill = NA),
                      axis.text=element_text(size = 20, colour = "black"),
                      # axis.text.x= element_text(face = "bold"),
                      axis.title=element_text(size=24),
                      legend.text=element_text(size=24),
                      legend.title=element_text(size=24))















