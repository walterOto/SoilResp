## Pablo Salazar 
## Elva Palacios Mccubbin 
## Universidad de Piura
## Soil respiration data analysis
## 31/05/2018

# Libraries --------------------------------------------------------------------------------------
library(dplyr)    # to summarise the data x
library(forcats)  # fct_recode to change level names of a factor
library(ggplot2)  # to visualize the data
library(ggpubr)   # ggarrange()
library(ggthemes) # to add some aesthetics to the ggplots
library(rmcorr)   ## for repetead measurement correlation 

# Data --------------------------------------------------------------------------------------------
setwd("/Users/RStudio/Documents/Dropbox/SoilResp/Data/Data_v2/")
Rs_piura <- read.csv("Rs_Piura_2017&2018.csv") # piura data
Rs_glob  <- read.csv("soil_resp_global.csv") # global data
Soil_Hum <- read.csv("Hum_slo.csv") # Need to change the name of this object and file
Das_dat  <- read.csv("Dasometric_data.csv")

# summary dataframe for 2017 data
Rs_piura17 <- Rs_piura %>%
  filter(Year == "2017") %>%
  group_by(Season,Coverage) %>%
  summarise(N = n(), 
            Rs_mean = mean(Soil_resp, na.rm = TRUE),
            sd = sd(Soil_resp, na.rm = T)) %>% 
  mutate(se = sd/sqrt(N))
  
# summary dataframe for 2018 data
Rs_piura18 <- Rs_piura %>%
  filter(Year == "2018") %>%
  group_by(Season,Coverage) %>%
  summarise(N = n(), 
            Rs_mean = mean(Soil_resp, na.rm = TRUE),
            sd = sd(Soil_resp, na.rm = T)) %>% 
  mutate(se = sd/sqrt(N))

# Plotting ----------------------------------------------------------------------------------------------

# Custom theme for ggplots
custom_theme <- theme(panel.grid.major = element_blank(), # blank background 
                      panel.grid.minor = element_blank(), # blank background 
                      panel.background = element_blank(), # blank background
                      axis.line = element_line(colour = "black"), # black axis
                      panel.border = element_rect(colour = "black", size=1,
                                                  linetype = "solid", fill = NA), # solid line border
                      axis.text = element_text(size = 20, colour = "black"), # axis color and size
                      axis.title = element_text(size = 24), # axis font size
                      legend.text = element_text(size = 24), # legend font size
                      legend.title = element_text(size = 24)) # legend title font size


# Plot of Soil Respiration of 2017 Piura data
Rs_piura17_plot <- ggplot(Rs_piura17, aes(x = Season, y = Rs_mean, fill = Coverage)) +
  geom_errorbar(aes(ymin = Rs_mean - se, ymax = Rs_mean + se), # error geom first so the bars stay in front
                position = position_dodge(width = .7), width = .2) +
  geom_bar(position = position_dodge(),  # bars second
           stat = "identity", width = .65) + 
  scale_fill_manual(values = c("Outside canopy" = "#D2B48C", "Under canopy" = "#008B45")) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  labs(x = "",
       y = expression(paste(Rs~"(",mu~"molC",O[2] ~ m^-2 ~ s^-1,")"))) +
  custom_theme +
  theme(axis.text.x = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank())
  
# Plot of Soil Respiration of 2018 data
Rs_piura18_plot <- ggplot(Rs_piura18, aes(x = Season, y = Rs_mean, fill = Coverage)) +
  geom_errorbar(aes(ymin = Rs_mean - se, ymax = Rs_mean + se),
                width = 0.2,
                position = position_dodge(0.7)) +
  geom_bar(width = 0.65, position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = c("Outside canopy" = "#D2B48C", "Under canopy" = "#008B45")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), position = "right") +
  labs(x = "", y = "") +
  custom_theme +
  theme(axis.text.x = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank())

# grid of 2017 and 2018 soil respiration summary plots
Rs_piura_plot <- ggarrange(Rs_piura17_plot, Rs_piura18_plot,common.legend = TRUE, legend = "top")

# Global soil respiration analysis  ----------------------------------------------------------------------

# Subsetting soil respiration local data for summer and winter
Rs_piura_summer <- Rs_piura %>% 
  filter(Season == "Summer") %>% 
  select(Rs = Soil_resp,
         Temp = Soil_temp,
         Class = Year) %>% 
  mutate(Class = as.factor(Class)) %>% 
  mutate(Class = fct_recode(Class, "Piura 2017 summer" = "2017", "Piura 2018 summer" = "2018")) %>% 
  na.omit()

Rs_piura_winter <- Rs_piura %>%
  filter(Season == "Winter") %>% # subsetting summer season data only
  select(Rs = Soil_resp,
         Temp = Soil_temp,
         Class = Year) %>% 
  mutate(Class = as.factor(Class)) %>% 
  mutate(Class = fct_recode(Class, "Piura 2017 winter" = "2017", "Piura 2018 winter" = "2018")) %>% 
  na.omit()

# Subsetting global data for summer and winter
Rs_glob_summer <- Rs_glob %>%
  select(Rs = Rs_summer,
         Temp = MAT) %>%
  mutate(Class = factor("Global summer")) %>% 
  na.omit()

Rs_glob_winter <- Rs_glob %>%
  select(Rs = Rs_winter,
         Temp = MAT) %>%
  mutate(Class = factor("Global winter")) %>% 
  na.omit()

# Combining 
Rs_piura_glob_summer <- rbind(Rs_glob_summer, Rs_piura_summer)
Rs_piura_glob_winter <- rbind(Rs_glob_winter, Rs_piura_winter)

# Plotting --------------------------------------------------------------
# Piura + Global summer plot
Rs_summer_plot <- ggplot(data = Rs_piura_glob_summer, aes(x = Temp, y = Rs)) +
  geom_point(aes(color = Class), size = 2) +
  scale_colour_manual(values = c("Global summer" = "#000000", # black
                                 "Piura 2017 summer" = "#0000ff", # blue
                                 "Piura 2018 summer" = "#ff0000"), # red
                      labels = c("Global summer" = "Global",
                                 "Piura 2017 summer" = "2017",
                                 "Piura 2018 summer" = "2018")) +
  scale_y_continuous(limits = c(0,8)) +
  labs(x = "Temperature (C°)",
       y = expression(paste("Rs (", ~mu~"molC",O[2] ~ m^-2 ~ s^-1,")")),
       color = "") +
  annotate(geom = "text", x = 40, y = 7, label = "Summer", size = 10) +
  custom_theme +
  theme(axis.text = element_text(size = 24),
        axis.title.y = element_blank(), # No numbers on y axis
        legend.position = "bottom")

# Piura + Global winter plot
Rs_winter_plot <- ggplot(data = Rs_piura_glob_winter, aes(x = Temp, y = Rs)) +
  geom_point(aes(color = Class), size = 2) +
  scale_colour_manual(values = c("Global winter" = "#000000",
                                 "Piura 2017 winter" = "#0000ff",
                                 "Piura 2018 winter" = "#ff0000"),
                      labels = c("Global winter" = "Global",
                                 "Piura 2017 winter" = "2017",
                                 "Piura 2018 winter" = "2018")) +
  scale_y_continuous(limits = c(0,8)) +
  labs(x = "Temperature (C°)",
       y = expression(paste("Rs (", ~mu~"molC",O[2] ~ m^-2 ~ s^-1,")")),
       color = "") +
  annotate(geom = "text", x = 40, y = 7, label = "Winter", size = 10) +
  custom_theme +
  theme(axis.title.x = element_text(size = 24),
        axis.title.y = element_blank(), # No y axis title
        axis.text.y = element_blank(), # No numbers on y axis
        legend.position = "bottom")

Rs_piura_global_plot <- ggarrange(Rs_summer_plot, Rs_winter_plot, 
                                  align = "v", common.legend = TRUE, hjust = c(-1.5,-1.5),
                                  font.label = list(size = 22, face = "bold"), 
                                  labels = c("A)","B)"), legend = "bottom")


# setwd("/Users/RStudio/Documents/Dropbox/SoilResp/Outputs/Outputs2/")
tiff("Global_winter.tiff", width = 12, height = 4.5, units = 'in', res = 300, compression = 'lzw')
annotate_figure(arrange1,
                left = text_grob(expression(paste(Rs~group("(",mu~molCO[2]~m^-2~s^-1,")"))), 
                                 size = 24, rot = 90))
dev.off()

# Soil respiration with Soil Humidity analysis -----------------------------------------------------
# Summary tables of soil respiration by sample points
Rs_piura17_SP <- Rs_piura %>%
  filter(Year == "2017") %>%
  group_by(Sample_point) %>%
  summarise(N = n(), 
            Rs_mean = mean(Soil_resp, na.rm = TRUE),
            sd = sd(Soil_resp, na.rm = T)) %>% 
  mutate(se = sd/sqrt(N))

Rs_piura18_SP <- Rs_piura %>%
  filter(Year == "2018") %>%
  group_by(Sample_point) %>%
  summarise(N = n(), 
            Rs_mean = mean(Soil_resp, na.rm = TRUE),
            sd = sd(Soil_resp, na.rm = T)) %>% 
  mutate(se = sd/sqrt(N))

# Soil humidity by sample points
Soil_Hum17 <- Soil_Hum %>% 
  select(Sample_point = Punto,
         Sample_number = Muestra,
         Year,
         SoilHum = AguaenSuelo) %>% 
  filter(Year == "2017") %>% 
  mutate(Year = as.factor(Year))

Soil_Hum18 <- Soil_Hum %>% 
  select(Sample_point = Punto,
         Sample_number = Muestra,
         Year,
         SoilHum = AguaenSuelo) %>% 
  filter(Year == "2018") %>% 
  mutate(Year = as.factor(Year))

# Merging soil respiration and soil humidity per year
Rs_SH_17 <- merge(Rs_piura17_SP, Soil_Hum17)
Rs_SH_18 <- merge(Rs_piura18_SP, Soil_Hum18)

# Correlation with canopy area ------------------------------------------------------------------------------------
Rs_piura_summer2 <- Rs_piura %>% 
  select(Year,
         Coverage,
         Rs = Soil_resp,
         Soil_temp) %>% 
  filter(Year == "2017", Coverage == "Under canopy") %>% 
  mutate(Year = as.factor(Year),
         ID = rep(1:20, times = 10),
         CanopyArea = rep(Das_dat$CanopyArea, times = 10))

ggplot(data = Rs_piura_summer2, aes(x = CanopyArea, y = Rs)) +
  geom_point(aes(color = Year)) +
  geom_smooth(aes(x = CanopyArea, y = Rs), method = lm, se = FALSE) +
  labs(x = expression(paste("Canopy area (",m^2,")")),
       y = expression(paste("Soil respiration rate (",~mu~molCO[2]~m^-2~s^-1,")"))) +
  custom_theme +
  theme(legend.position = "none")

cor.test(Rs_piura_summer2, )






















