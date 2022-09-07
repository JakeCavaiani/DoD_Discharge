### Read me ###
# the purpose of this script is to generate rating curves from discrete observed discharge measurements 

# Important NOTES:
# 1) Discrete discharge measurements are found using two methods: Salt Slug Dilution that and a wading rod measurement

# 2) This data is read in from DoD Project->2020 AK sensors->Discharge-> QSummary

# Step 1: import discrete discharge measurements summary file which is site, date, time, method, VolSlugml	Batch, and MeasuredQ_Ls 
# Step 2: Generate rating curves for both rating curves from PT data from 01_PT_data script

# Load packages #
library(tidyverse)
library(lubridate)
library(data.table)
library(rio)
library(ggplot2)
library(scales)
library(psych)
library(here)
library(googledrive)
library(readxl)
library(cowplot)
library(zoo)
library(readr)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(ggpmisc)




dir.create(here("Rating_curve"))
dir.create(here("Rating_curve", "Plots"))
dir.create(here("Rating_curve", "Plots", "FRCH"))
dir.create(here("Rating_curve", "Plots", "MOOS"))
dir.create(here("Rating_curve", "Plots", "POKE"))
dir.create(here("Rating_curve", "Plots", "STRT"))
dir.create(here("Rating_curve", "Plots", "VAUL"))

frch_pt_2015 <- read.csv("PT_data/2015/frch.pt.2015.csv")


##################### 2015 ##################################################################
# Import data from google drive #
discharge.2015 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQC9Bk0nS-Cx4Ec8MyLHd2xNuSv8JTobR8SSV_ODQHAvp4cUK8k3z9EmOs/pub?output=csv"
QSummary <- read.csv(url(discharge.2015))
QSummary$date <- mdy(QSummary$date)
QSummary$DateTime <- as.POSIXct(paste(QSummary$date, QSummary$time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")


# ALL Sites #
ggplot(QSummary) +
  geom_point(aes(x=date, y=Q..L.s., color=site), size=3) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("ALL SITES")

# Filter French #
QSummary.FR <- QSummary %>% filter(site =="French")


### Rating curve for FRCH PT1 ###
frch.stream$site <- "French" # Add a column identifier 

French1comb <- full_join(frch.stream, QSummary.FR) # Join PT data with Discharge
French1.lm <- lm(French1comb$Q..L.s. ~ French1comb$AbsolutePressure) # linear model with discharge and water level

frch.formula <- y ~ x

frc.1 <- ggplot(aes(x = AbsolutePressure, y = Q..L.s.), data = French1comb) +
  geom_point(aes(color = Q..L.s.), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_classic() +
  ggtitle("French1 all measured Q") 

frc.1

# Filter Moose #
QSummary.MO <- QSummary %>% filter(site =="Moose")

# Rating curve for MOOS PT1 # 
moos.stream$site <- "Moose"

Moose1comb <- full_join(moos.stream, QSummary.MO)
MOOS1.lm <- lm(Moose1comb$Q..L.s. ~ Moose1comb$AbsolutePressure)

moos.formula <- y ~ x

mrc.1 <- ggplot(aes(x = AbsolutePressure, y = Q..L.s.), data = Moose1comb) +
  geom_point(aes(color = Q..L.s.), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_classic() +
  ggtitle("Moose1 all measured Q") 

mrc.1



#################################### 2018 ###################################################
Qsummary.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT0R955lmRu0iaZFc-CxoVhwApmJuWaiHCYxhqICnY4oFH1sDI-VhRESmLFeuss01SYz0krWRktJ3oF/pub?output=csv" 
Qsummary <- read.csv(url(Qsummary.url))

Qsummary$Date <- mdy(Qsummary$Date)
Qsummary$DateTime <- as.POSIXct(paste(Qsummary$Date, Qsummary$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
Qsummary$DateTime <- lubridate::round_date(Qsummary$DateTime, "15 minutes")

# Moose 1 PT 
moose1comb <- full_join(Moose1, Qsummary.MO) %>% 
  filter(WaterLevelmeters > 166)
ggplot(aes(x=WaterLevelmeters, y=MeasuredQ_Ls), data=moose1comb) +
  geom_point(aes( color=Method), size=3) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(limits = c(166.2,167)) +
  theme_classic() +
  ggtitle("Moose1 all measured Q") 
Moose1.lm <- lm(moose1comb$MeasuredQ_Ls ~ moose1comb$WaterLevelmeters)
summary(Moose1.lm)

# Moose 2 PT 
Qsummary.MO <- Qsummary  %>% 
  filter(Site == "Moose") 
moose2comb <- full_join(Moose2, Qsummary.MO) %>% 
  filter(WaterLevelmeters > 166)
ggplot(aes(x=WaterLevelmeters, y=MeasuredQ_Ls), data=moose2comb) +
  geom_point(aes( color=Method), size=3) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(limits = c(166.2,167)) +
  theme_classic() +
  ggtitle("Moose2 all measured Q") 
Moose2.lm <- lm(moose2comb$MeasuredQ_Ls ~ moose2comb$WaterLevelmeters)

# FRCH PT 1 # 
Qsummary.FR <- Qsummary  %>% #filter out measured Q at just french
  filter(Site == "French") 

French1comb <- full_join(French1, Qsummary.FR) %>% 
  filter(WaterLevelmeters > 184)
ggplot(aes(x=WaterLevelmeters, y=MeasuredQ_Ls), data=French1comb) +
  geom_point(aes( color=Method), size=3) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(limits = c(184.2,184.9)) +
  theme_classic() +
  ggtitle("French1 all measured Q") 
French1.lm <- lm(French1comb$MeasuredQ_Ls ~ French1comb$WaterLevelmeters)
summary(French1.lm)

# FRCH  PT 2 # 
French2comb <- full_join(French2, Qsummary.FR) %>% 
  filter(WaterLevelmeters > 185)
ggplot(aes(x=WaterLevelmeters, y=MeasuredQ_Ls), data=French2comb) +
  geom_point(aes(color = Method), size=3) +
  geom_smooth(method = "lm", se=FALSE) +
  scale_x_continuous(limits = c(185.4,186.3)) +
  theme_classic() +
  ggtitle("French2 all measured Q") 
French2.lm <- lm(French2comb$MeasuredQ_Ls ~ French2comb$WaterLevelmeters)
summary(French2.lm)
########################################## 2019 #################################################
### Observed Discharge ###
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRUMy2yDlF5WQRDGgbuNHeVNp7diusfPJuKgikGY2ZQ8ewbG4Tyxm5TeN0shtDkxMmeL9M0AzhaL8l7/pub?output=csv"
QSummary.2019 <- read.csv(url(myurl))

QSummary.2019$Time[QSummary.2019$Time == ""] <- NA
QSummary.2019$Q_Ls[QSummary.2019$Q_Ls == ""] <- NA

### Format Time ###
QSummary.2019$Date <- mdy(QSummary.2019$Date)
QSummary.2019$DateTime <- as.POSIXct(paste(QSummary.2019$Date, QSummary.2019$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
QSummary.2019$DateTime <- lubridate::round_date(QSummary.2019$DateTime, "15 minutes")

### Rating curve for FRCH PT1 ###
QSummary.FR.2019 <- QSummary.2019 %>% filter(Site =="French") %>% drop_na(Q_Ls)
QSummary.FR.2019$Site <- "FRCH"
frch.stream.one.2019$Site <- "FRCH"

French1comb.2019 <- full_join(frch.stream.one.2019, QSummary.FR.2019) 
French1.lm.2019 <- lm(French1comb.2019$Q_Ls ~ French1comb.2019$AbsPTDepth)
summary(French1.lm.2019)  # Worked

frch.formula <- y ~ x

ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = French1comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0.55, 0.70) +
  ylim(0, 300) +
  theme_classic() +
  ggtitle("French1 all measured Q")

French1comb.2019$pred.french1.Q <- coef(French1.lm.2019)[2] * French1comb.2019$AbsPTDepth+ coef(French1.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.french1.Q), data=French1comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("French") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  ylim(0, 5000) +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))

### Rating curve for FRCH PT2 ###
frch.stream.two.2019$Site<- "FRCH"

French2comb.2019 <- full_join(frch.stream.two.2019, QSummary.FR.2019) 
French2.lm.2019 <- lm(French2comb.2019$Q_Ls ~ French2comb.2019$AbsPTDepth)
summary(French2.lm.2019) # worked


ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = French2comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0.45, 0.5) +
  ylim(0, 300) +
  theme_classic() +
  ggtitle("French2 all measured Q")

# VAUL #
### Rating curve for VAUL PT1 ###
QSummary.VA.2019 <- QSummary.2019 %>% filter(Site =="Vault") %>% drop_na(Q_Ls)
vaul.stream.one.2019$Site <- "VAUL"
QSummary.VA.2019$Site <- "VAUL"

Vaultcomb.2019 <- full_join(vaul.stream.one.2019, QSummary.VA.2019)

Vault.lm.2019<- lm(Vaultcomb.2019$Q_Ls ~ 0 + Vaultcomb.2019$AbsPTDepth)
summary(Vault.lm.2019)

vaul.formula <- y ~ x

ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = Vaultcomb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  xlim(0.4, 0.65) +
  theme_classic() +
  ggtitle("Vault all measured Q")  # I think this worked

### Rating curve for POKE PT1 ###
QSummary.PO.2019 <- QSummary.2019 %>% filter(Site =="Poker") %>% drop_na(Q_Ls)
QSummary.PO.2019$Site <- "POKE"
poke.stream.one.2019$Site <- "POKE"

Poker1comb.2019 <- full_join(poke.stream.one.2019, QSummary.PO.2019)
Poker1.lm.2019 <- lm(Poker1comb.2019$Q_Ls ~ Poker1comb.2019$AbsPTDepth)
summary(Poker1.lm.2019) 

poke.formula <- y ~ x


ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = Poker1comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 0.5) +
  ylim(0, 750) +
  theme_classic() +
  ggtitle("Poker1 all measured Q")

Poker1comb.2019$pred.poke1.Q <- coef(Poker1.lm.2019)[2] * Poker1comb.2019$AbsPTDepth+ coef(Poker1.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.poke1.Q), data=Poker1comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  ylim(0, 1500) +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))


### Rating Curve for POKE PT2 ### 
poke.stream.two.2019$Site <- "POKE"

Poker2comb.2019 <- full_join(poke.stream.two.2019, QSummary.PO.2019)
Poker2.lm.2019 <- lm(Poker2comb.2019$Q_Ls ~ Poker2comb.2019$AbsPTDepth)
summary(Poker2.lm.2019)

ggplot(aes(x= AbsPTDepth, y = Q_Ls), data = Poker2comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0.3,0.6) +
  theme_classic() +
  ggtitle("Poker2 all measured Q") 


### STRT ###
### Rating curve for STRT PT1 ###
QSummary.ST.2019 <- QSummary.2019 %>% filter(Site =="Stuart") %>% drop_na(Q_Ls)
QSummary.ST.2019$Site<- "STRT"
strt.stream.one.2019$Site<- "STRT"

Stuart1comb.2019 <- full_join(strt.stream.one.2019, QSummary.ST.2019)
Stuart1.lm.2019 <- lm(Stuart1comb.2019$Q_Ls ~ Stuart1comb.2019$AbsPTDepth)
summary(Stuart1.lm.2019) 


strt.formula <- y ~ x


ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = Stuart1comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 1) +
  ylim(0, 750) +
  theme_classic() +
  ggtitle("Stuart1 all measured Q")



### Rating Curve for STRT PT2 ### 
strt.stream.two.2019$Site<- "STRT"

Stuart2comb.2019 <- full_join(strt.stream.two.2019, QSummary.ST.2019)
Stuart2.lm.2019 <- lm(Stuart2comb.2019$Q_Ls ~ Stuart2comb.2019$AbsPTDepth)
summary(Stuart2.lm.2019) 


strt.formula <- y ~ x


ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = Stuart2comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 1) +
  ylim(0, 750) +
  theme_classic() +
  ggtitle("Stuart2 all measured Q")


### MOOS ###
QSummary.MO.2019 <- QSummary.2019 %>% filter(Site =="Moose") %>% drop_na(Q_Ls)
QSummary.MO.2019$Site <- "MOOS"
moos.stream.one.2019$Site <- "MOOS"

Moose1comb.2019 <- full_join(moos.stream.one.2019, QSummary.MO.2019) 

Moose1.lm.2019 <- lm(Moose1comb.2019$Q_Ls ~ Moose1comb.2019$AbsPTDepth)
summary(Moose1.lm.2019) # I think this worked

moos.formula <- y ~ x

ggplot(aes(x = AbsPTDepth, y = Q_Ls), data = Moose1comb.2019) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0.9, 1.8) + 
  theme_classic() +
  ggtitle("Moose1 all measured Q")  # I think this worked


################################## 2020 ###################################################################
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRUMy2yDlF5WQRDGgbuNHeVNp7diusfPJuKgikGY2ZQ8ewbG4Tyxm5TeN0shtDkxMmeL9M0AzhaL8l7/pub?output=csv"
QSummary.2019 <- read.csv(url(myurl))

QSummary.2019$Time[QSummary.2019$Time == ""] <- NA
QSummary.2019$Q_Ls[QSummary.2019$Q_Ls == ""] <- NA

### Format Time ###
QSummary.2019$Date <- mdy(QSummary.2019$Date)
QSummary.2019$DateTime <- as.POSIXct(paste(QSummary.2019$Date, QSummary.2019$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
QSummary.2019$DateTime <- lubridate::round_date(QSummary.2019$DateTime, "15 minutes")

# Import data from google drive #
discharge.2020 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTPrFKu3yyEDEDkxPVJW2vIWznwmSUcwuNlHInDmrD4EjOQYAkHmtnWJXRT1toDa74ptmHj4O1My3xw/pub?output=csv"
QSummary.2020 <- read.csv(url(discharge.2020))
QSummary.2020 <-  subset(QSummary.2020, select = -c(X2019, Notes, Average, X, Observations, X.1, X2020, average.as.of.8.29., X.2, observations.as.of.8.29.)) # Cleaning columns that are not important to the dataset
QSummary.2020$date <- mdy(QSummary.2020$Date)
QSummary.2020$DateTime <- as.POSIXct(paste(QSummary.2020$date, QSummary.2020$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

### ALL Sites ###
ggplot(QSummary.2020) +
  geom_point(aes(x=Date, y=MeasuredQ_Ls, color=Site, shape=Method), size=3) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("ALL SITES")

# Filter French #
QSummary.FR.2020 <- QSummary.2020 %>% filter(Site =="FRCH")

### Rating curve for FRCH PT1 ###
frch.stream.one.2020$Site <- "FRCH"

French1comb.2020 <- full_join(frch.stream.one.2020, QSummary.FR.2020) # Join PT data with Discharge
French1.lm.2020 <- lm(French1comb.2020$MeasuredQ_Ls ~ French1comb.2020$WaterLevel) # linear model with discharge and water level


frch.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = French1comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(184,185.5) +
  theme_classic() +
  ggtitle("French1 all measured Q") 



### Rating curve for FRCH PT2 ### 
frch.stream.two.2020$Site <- "FRCH"

French2comb.2020 <- full_join(frch.stream.two.2020, QSummary.FR.2020)
French2.lm.2020 <- lm(French2comb.2020$MeasuredQ_Ls ~ French2comb.2020$WaterLevel)


ggplot(aes(x= WaterLevel, y = MeasuredQ_Ls), data = French2comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(184, 185.5) + 
  theme_classic() +
  ggtitle("French2 all measured Q") 



### Filter Moose ###
QSummary.MO.2020 <- QSummary.2020 %>% filter(Site =="MOOS")

moos.stream.one.2020.final$Site <- "MOOS"

Moose1comb.2020 <- full_join(moos.stream.one.2020.final, QSummary.MO.2020)
MOOS1.lm.2020 <- lm(Moose1comb.2020$MeasuredQ_Ls ~ Moose1comb.2020$WaterLevel)

moos.formula <- y ~ x
ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Moose1comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(165.75,166.5) +
  ylim(600, 1500) +
  theme_classic() +
  ggtitle("Moose1 all measured Q") 



### Rating curve for MOOS PT2 ### 

moos.stream.two.2020.final$Site <- "MOOS"

Moose2comb.2020 <- full_join(moos.stream.two.2020.final, QSummary.MO.2020)
MOOS2.lm.2020 <- lm(Moose2comb.2020$MeasuredQ_Ls ~ Moose2comb.2020$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Moose2comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(165.75,166.75) +
  ylim(600, 1500) +
  theme_classic() +
  ggtitle("Moose2 all measured Q") 


### Filter Poker ###
QSummary.PO.2020 <- QSummary.2020 %>% filter(Site =="POKE")

### Rating curve for POKE PT1 ###
poke.stream.one.2020$Site <- "POKE"

Poke1comb.2020 <- full_join(poke.stream.one.2020, QSummary.PO.2020)
POKE1.lm.2020 <- lm(Poke1comb.2020$MeasuredQ_Ls ~ Poke1comb.2020$WaterLevel)

poke.formula <- y ~ x


ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Poke1comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = poke.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(215.9, 216.5) + 
  ylim(200, 2000) + 
  theme_classic() +
  ggtitle("Poke1 all measured Q") 



### Rating curve for POKE PT2 ###

poke.stream.two.2020$Site <- "POKE"

Poke2comb.2020 <- full_join(poke.stream.two.2020, QSummary.PO.2020)
POKE2.lm.2020 <- lm(Poke2comb.2020$MeasuredQ_Ls ~ Poke2comb.2020$WaterLevel)


ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Poke2comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(215.8, 216.5) + 
  ylim(200, 2000) +
  theme_classic() +
  ggtitle("Poker2 all measured Q")  


### Filter Stuart ###
QSummary.ST.2020 <- QSummary.2020 %>% filter(Site =="STRT")

### Rating curve for STRT PT1 ### 

strt.stream.one.2020$Site <- "STRT"

Strt1comb.2020 <- full_join(strt.stream.one.2020, QSummary.ST.2020)
STRT1.lm.2020 <- lm(Strt1comb.2020$MeasuredQ_Ls ~ Strt1comb.2020$WaterLevel)

strt.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Strt1comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = strt.formula) +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(248.4, 248.7) + 
  ylim(200, 3000) + 
  theme_classic() +
  ggtitle("Strt1 all measured Q")  # I think this worked


### Rating curve for STRT PT2 ###

strt.stream.two.2020$Site <- "STRT"

Strt2comb.2020 <- full_join(strt.stream.two.2020, QSummary.ST.2020)
STRT2.lm.2020 <- lm(Strt2comb.2020$MeasuredQ_Ls ~ Strt2comb.2020$WaterLevel)

strt.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Strt2comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = strt.formula) +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(248.5, 248.7) + 
  ylim(200, 2000) + 
  theme_classic() +
  ggtitle("Strt2 all measured Q") 


### Filter Vault ### 
QSummary.VA.2020 <- QSummary.2020 %>% filter(Site =="VAUL") %>% filter(MeasuredQ_Ls < 2000)

### Rating curve for VAUL PT2 ###
vaul.stream.2020$Site <- "VAUL"

Vaul2comb.2020 <- full_join(vaul.stream.2020, QSummary.VA.2020)
VAUL2.lm.2020 <- lm(Vaul2comb.2020$MeasuredQ_Ls ~ Vaul2comb.2020$WaterLevel)

vaul.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Vaul2comb.2020) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(197.5, 198.5) + 
  ylim(0, 1500) +
  theme_classic() +
  ggtitle("Vault2 all measured Q")  

########################################### 2021 ##############################################################
QSummary.2021 <- read_csv("~/Desktop/Q_Summary_2021.csv")


QSummary.2021$Time[QSummary.2021$Time == ""] <- NA
QSummary.2021$MeasuredQ_Ls[QSummary.2021$MeasuredQ_Ls == " "] <- NA
### Format Time ###
QSummary.2021$Date <- mdy(QSummary.2021$Date)
QSummary.2021$DateTime <- as.POSIXct(paste(QSummary.2021$Date, QSummary.2021$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
QSummary.2021$DateTime <- lubridate::round_date(QSummary.2021$DateTime, "15 minutes")

### ALL Sites ###
ggplot(QSummary.2021) +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls, color=Site, shape=Method), size=3) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("ALL SITES")

ggplot(QSummary.2021, aes(y=MeasuredQ_Ls, x=DateTime, col = Site, shape = Method)) +
  geom_point(alpha=1) +
  facet_wrap(~Site) + 
  ylab("Discharge (L/s)") +
  xlab("Date") +
  theme_light()

### Filter Poker ###
QSummary.PO.2021 <- QSummary.2021 %>% filter(Site =="POKE")

### Rating curve for POKE PT1 ###
poke.stream.one.2021$Site <- "POKE"

Poke1comb.2021 <- full_join(poke.stream.one.2021, QSummary.PO.2021)
POKE1.lm.2021 <- lm(Poke1comb.2021$MeasuredQ_Ls ~ Poke1comb.2021$WaterLevel)


poke.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Poke1comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = poke.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(216, 216.4) +
  ylim(0,1500) +
  theme_light() +
  ggtitle("Poke1 all measured Q") 

ysi.pt1 <- Poke1comb.2021[which(Poke1comb.2021$Method == "YSI"), ]
rod.pt1 <- Poke1comb.2021[which(Poke1comb.2021$Method == "Wading rod"), ]

Poke1comb.2021.1 <- Poke1comb.2021[-c(818,4720,28901,8734), ]# removing measurements that dont seem good

POKE1.lm.2021.1 <- lm(Poke1comb.2021.1$MeasuredQ_Ls ~ Poke1comb.2021.1$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Poke1comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = poke.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(216, 216.4) +
  ylim(0,1500) +
  theme_light() +
  ggtitle("Poke1 all measured Q") 

### Rating curve for POKE PT2 ###

poke.stream.two.2021$Site <- "POKE"

Poke2comb.2021 <- full_join(poke.stream.two.2021, QSummary.PO.2021)
POKE2.lm.2021 <- lm(Poke2comb.2021$MeasuredQ_Ls ~ Poke2comb.2021$WaterLevel)


ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Poke2comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(216, 216.5) + 
  ylim(200, 1500) +
  theme_light() +
  ggtitle("Poker2 all measured Q")

ysi.pt2 <- Poke2comb.2021[which(Poke2comb.2021$Method == "YSI"), ]
rod.pt2 <- Poke2comb.2021[which(Poke2comb.2021$Method == "Wading rod"), ]

Poke2comb.2021.1 <- Poke2comb.2021[-c(1359,5261,29443), ]# removing measurements that dont seem good

POKE2.lm.2021.1 <- lm(Poke2comb.2021.1$MeasuredQ_Ls ~ Poke2comb.2021.1$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Poke2comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = poke.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(216, 216.4) +
  ylim(0,1500) +
  theme_light() +
  ggtitle("Poke2 all measured Q") 


### Filter STRT ###
QSummary.ST.2021 <- QSummary.2021 %>% filter(Site =="STRT")

### Rating curve for STRT PT1 ###
strt.stream.one.2021$Site <- "STRT"

Strt1comb.2021 <- full_join(strt.stream.one.2021, QSummary.ST.2021)
STRT1.lm.2021 <- lm(Strt1comb.2021$MeasuredQ_Ls ~ Strt1comb.2021$WaterLevel)

strt.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Strt1comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = strt.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(250, 251) + 
  ylim(200, 5000) + 
  theme_classic() +
  ggtitle("Strt1 all measured Q") 

### Rating curve for STRT PT2 ###

strt.stream.two.2021$Site <- "STRT"

Strt2comb.2021 <- full_join(strt.stream.two.2021, QSummary.ST.2021)
STRT2.lm.2021 <- lm(Strt2comb.2021$MeasuredQ_Ls ~ Strt2comb.2021$WaterLevel)


ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Strt2comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(249.5, 250.15) + 
  ylim(200, 3000) +
  theme_classic() +
  ggtitle("Stuart2 all measured Q")

ysi.pt2.strt <- Strt2comb.2021[which(Strt2comb.2021$Method == "YSI"), ]


Strt2comb.2021.1 <- Strt2comb.2021[-c(1453,2123,2791,17456,27437,30069), ]# removing measurements that dont seem good

STRT2.lm.2021.1 <- lm(Strt2comb.2021.1$MeasuredQ_Ls ~ Strt2comb.2021.1$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Strt2comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(249.5, 250.15) + 
  ylim(200, 3000) +
  theme_classic() +
  ggtitle("Stuart2 all measured Q")


### Filter VAUL ###
QSummary.VA.2021 <- QSummary.2021 %>% filter(Site =="VAUL")

### Rating curve for VAUL PT1 ###
vaul.stream.one.2021$Site <- "VAUL"

Vaul1comb.2021 <- full_join(vaul.stream.one.2021, QSummary.VA.2021)
VAUL1.lm.2021 <- lm(Vaul1comb.2021$MeasuredQ_Ls ~ Vaul1comb.2021$WaterLevel)

vaul.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Vaul1comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = vaul.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(197.5, 198.5) + 
  ylim(0, 1250) +
  theme_classic() +
  ggtitle("Vaul1 all measured Q") 

### Filter FRCH ###
QSummary.FR.2021 <- QSummary.2021 %>% filter(Site =="FRCH")

### Rating curve for FRCH PT1 ###
frch.stream.one.2021$Site <- "FRCH"

Frch1comb.2021 <- full_join(frch.stream.one.2021, QSummary.FR.2021)
FRCH1.lm.2021 <- lm(Frch1comb.2021$MeasuredQ_Ls ~ Frch1comb.2021$WaterLevel)

frch.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Frch1comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = frch.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(184, 185) + 
  theme_light() +
  ggtitle("Frch1 all measured Q") 

ysi.pt1.frch <- Frch1comb.2021[which(Frch1comb.2021$Method == "YSI"), ]

Frch1comb.2021.1 <- Frch1comb.2021[-c(21394,25408,29203), ]

FRCH1.lm.2021.1 <- lm(Frch1comb.2021.1$MeasuredQ_Ls ~ Frch1comb.2021.1$WaterLevel)


ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Frch1comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = frch.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(184, 185) + 
  theme_classic() +
  ggtitle("Frch1 all measured Q") 

rod.pt1.frch <- Frch1comb.2021[which(Frch1comb.2021$Method == "Wading rod"), ]

Frch1comb.2021.2 <- Frch1comb.2021[-c(21394,13030, 17623), ]

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Frch1comb.2021.2) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = frch.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(184, 185) + 
  theme_light() +
  ggtitle("Frch1 all measured Q") 



### Rating curve for FRCH PT2 ###

frch.stream.two.2021$Site <- "FRCH"

Frch2comb.2021 <- full_join(frch.stream.two.2021, QSummary.FR.2021)
FRCH2.lm.2021 <- lm(Frch2comb.2021$MeasuredQ_Ls ~ Frch2comb.2021$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Frch2comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_light() +
  ggtitle("Frch2 all measured Q")

ysi.pt2.frch <- Frch2comb.2021[which(Frch2comb.2021$Method == "YSI"), ]
rod.pt2.frch <- Frch2comb.2021[which(Frch2comb.2021$Method == "Wading rod"), ]

Frch2comb.2021.1 <- Frch2comb.2021[-c(21392,21410), ]

FRCH2.lm.2021.1 <- lm(Frch2comb.2021.1$MeasuredQ_Ls ~ Frch2comb.2021.1$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Frch2comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_light() +
  ggtitle("Frch2 all measured Q")

### Filter MOOS ###
QSummary.MO.2021 <- QSummary.2021 %>% filter(Site =="MOOS")

### Rating curve for MOOS PT1 ###
moos.stream.one.2021$Site <- "MOOS"

Moos1comb.2021 <- full_join(moos.stream.one.2021, QSummary.MO.2021)
MOOS1.lm.2021 <- lm(Moos1comb.2021$MeasuredQ_Ls ~ Moos1comb.2021$WaterLevel)

moos.formula <- y ~ x

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Moos1comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = moos.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(165.8, 166.1) +
  theme_classic() +
  ggtitle("Moos1 all measured Q") 

ysi.pt1.moos <- Moos1comb.2021[which(Moos1comb.2021$Method == "YSI"), ]

Moos1comb.2021.1 <- Moos1comb.2021[-c(9008,13055, 17648), ]

MOOS1.lm.2021.1 <- lm(Moos1comb.2021.1$MeasuredQ_Ls ~ Moos1comb.2021.1$WaterLevel)


ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Moos1comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = moos.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(165.8, 166.1) +
  theme_classic() +
  ggtitle("Moos1 all measured Q") 

### Rating curve for MOOS PT2 ###

moos.stream.two.2021$Site <- "MOOS"

Moos2comb.2021 <- full_join(moos.stream.two.2021, QSummary.MO.2021)
MOOS2.lm.2021 <- lm(Moos2comb.2021$MeasuredQ_Ls ~ Moos2comb.2021$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Moos2comb.2021) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(165.8, 166) +
  theme_classic() +
  ggtitle("Moos2 all measured Q")

ysi.pt2.moos <- Moos2comb.2021[which(Moos2comb.2021$Method == "YSI"), ]

Moos2comb.2021.1 <- Moos2comb.2021[-c(9008,13055,17648), ]

MOOS2.lm.2021.1 <- lm(Moos2comb.2021.1$MeasuredQ_Ls ~ Moos2comb.2021.1$WaterLevel)

ggplot(aes(x = WaterLevel, y = MeasuredQ_Ls), data = Moos2comb.2021.1) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = moos.formula) +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(165.8, 166.1) +
  theme_classic() +
  ggtitle("Moos2 all measured Q") 















########################################## 2022 ####
# Import data from google drive #
discharge.2022 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQR6HHHDpnxc6DmNHfdNLR9-dgDHR5Imt0Ve4_t2DzIF18_8D3O2da5zcWQzJUSoFQfaetPZDeXZ610/pub?gid=0&single=true&output=csv"
QSummary.2022 <- read.csv(url(discharge.2022))

### Format Time ###
QSummary.2022$Date <- mdy(QSummary.2022$Date)
QSummary.2022$DateTime <- as.POSIXct(paste(QSummary.2022$Date, QSummary.2022$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
QSummary.2022$DateTime <- lubridate::round_date(QSummary.2022$DateTime, "15 minutes")

### ALL Sites ###
ggplot(QSummary.2022) +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls, color=Site, shape=Method), size=3) +
  theme_classic() +
  scale_color_brewer(palette = "Set1") +
  ggtitle("ALL SITES") 


#### This is a rating curve for midway through the year 
  # we are going to take water pressure - ATM pressure and plot that versus measured Q
    # for each site 

### MOOS #### 
# Load in water pressure data 
# urls
moos.atmo.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7QMNWjVX1R43qflIW7lVqkj7wRHouhgQhWK76kfR8zK-UZg9bFSm92ccPy8T0luhFHKwsLNQQjA56/pub?output=csv"
moos.stream.one.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRo04g32Yr3DsZbXQiw6VTPfgWuWq5csh1VhxfMy3IT3U6OkJOGvTlofjXwFfugiaRVJ34pPnOUZ3te/pub?output=csv"
moos.stream.two.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRBpyHQqVDqrUTjTvYXM4RhAEoeG0lRV7fB2mHHGa55Xa__nHrjKCRX_GAIyoIatgWfOwNlmjN-hzzb/pub?output=csv"

# load in data
moos.atmo.2022 <- read.csv(url(moos.atmo.2022.url), skip = 1) 
moos.stream.one.2022 <- read.csv(url(moos.stream.one.2022.url), skip = 1) 
moos.stream.two.2022 <- read.csv(url(moos.stream.two.2022.url), skip = 1) 

# cleaning df to be able to interpret and merge
moos.atmo.2022 <- moos.atmo.2022[, -c(5:11)] # removing columns that aren't date/abs pressure and temp
names(moos.atmo.2022) <- c("Site", "DateTimeGMT", "AirPressure", "TempC")

moos.stream.one.2022 <- moos.stream.one.2022[, -c(5:8)] # removing columns that aren't date/abs pressure and temp
names(moos.stream.one.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

moos.stream.two.2022 <- moos.stream.two.2022[, -c(5:12)] # removing columns that arent date/abs pressure and temp
names(moos.stream.two.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

moos.final.pressure.2022 <- right_join(moos.stream.one.2022, moos.stream.two.2022, by = c("DateTimeGMT"))
moos.final.pressure.2022$MeanPressure <- rowMeans(moos.final.pressure.2022[,c(3,6)], na.rm = TRUE)

ggplot(moos.atmo.2022, aes(x = DateTimeGMT, y = AirPressure)) +
  geom_point()

# join the two atmospheric and water pressure together
MOOS.2022 <- left_join(moos.atmo.2022, moos.final.pressure.2022, by = "DateTimeGMT")

MOOS.2022$DateTime <- mdy_hms(MOOS.2022$DateTimeGMT, tz = "GMT")
attributes(MOOS.2022$DateTime)$tzone <- 'America/Anchorage'

# Water pressure - atmospheric pressure 
MOOS.2022$difference <- MOOS.2022$MeanPressure - MOOS.2022$AirPressure
MOOS.2022 <- MOOS.2022[,-c(2:10)] # only want Site/WaterPressure/DateTime/Difference
MOOS.2022$Site <- "MOOS"

# Checking closeness between two PTs
moos.stream.two.2022 <- moos.stream.two.2022[1:nrow(moos.stream.one.2022),]
moos.stream.one.2022$Site <- "STRT1" #add column identifier
moos.stream.two.2022$Site <- "STRT2"
moos.pt.2022 <- bind_rows(moos.stream.one.2022, moos.stream.two.2022)

plot(x = moos.stream.one.2022$WaterPressure, y = moos.stream.two.2022$WaterPressure, main = "Stuart PT comparison",
     xlab = "Moose1PT", 
     ylab = "Moose2PT")
abline(1,1)

### Filter MOOS ###
QSummary.MO.2022 <- QSummary.2022 %>% filter(Site =="MOOS")

Moose1comb.2022 <- full_join(MOOS.2022, QSummary.MO.2022)
MOOS1.lm.2022 <- lm(Moose1comb.2022$MeasuredQ_Ls ~ Moose1comb.2022$difference)


moos.formula <- y ~ x

ggplot(aes(x = difference, y = MeasuredQ_Ls), data = Moose1comb.2022) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = moos.formula) +
  stat_poly_eq(formula = moos.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  #xlim(216, 216.4) +
  #ylim(0,1500) +
  theme_light() +
  ggtitle("Moos1 all measured Q")

### POKE ####
poke.stream.one.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSIx8D5P_93-MADOTtYZ75oAXQp_KJWk7es6sG9x_ytpNSXX7dSkGEUds5UvPUwpAucNyih3jCmzwRM/pub?output=csv"
poke.stream.two.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR-wJjf3rO8eC7bMpQZgwqFLHFQpf5qCInyWtUF1PqaCB8Z_EoM1cTHRUmWypjftREIx1rP0V6zYJxL/pub?output=csv"

# load in url
poke.stream.one.2022 <- read.csv(url(poke.stream.one.2022.url), skip = 1) 
poke.stream.two.2022 <- read.csv(url(poke.stream.two.2022.url), skip = 1) 

# cleaning df to be able to interpret and merge
poke.stream.one.2022 <- poke.stream.one.2022[, -c(5:13)] # removing columns that arent date/abs pressure and temp
names(poke.stream.one.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

poke.stream.two.2022 <- poke.stream.two.2022[, -c(5:9)] # removing columns that arent date/abs pressure and temp
names(poke.stream.two.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

# merge to one 
poke.final.pressure.2022 <- left_join(poke.stream.one.2022, poke.stream.two.2022, by = c("DateTimeGMT"))
poke.final.pressure.2022$MeanPressure <- rowMeans(poke.final.pressure.2022[,c(3,6)], na.rm = TRUE)

# join the two atmospheric and water pressure together
POKE.2022 <- right_join(moos.atmo.2022, poke.final.pressure.2022, by = "DateTimeGMT")

POKE.2022$DateTime <- mdy_hms(POKE.2022$DateTimeGMT, tz = "GMT")
attributes(POKE.2022$DateTime)$tzone <- 'America/Anchorage'

# Water pressure - atmospheric pressure 
POKE.2022$difference <- POKE.2022$MeanPressure - POKE.2022$AirPressure
POKE.2022 <- POKE.2022[,-c(2:10)] # only want Site/WaterPressure/DateTime/Difference
POKE.2022$Site <- "POKE"

### Filter POKE ###
QSummary.PO.2022 <- QSummary.2022 %>% filter(Site =="POKE")

Poker1comb.2022 <- full_join(POKE.2022, QSummary.PO.2022)
POKE1.lm.2022 <- lm(Poker1comb.2022$MeasuredQ_Ls ~ Poker1comb.2022$difference)
#POKE2.lm.2022 <- lm(Poker1comb.2022$MeasuredQ_Ls ~ Poker1comb.2022$WaterPressure)


poke.formula <- y ~ x

ggplot(aes(x = difference, y = MeasuredQ_Ls), data = Poker1comb.2022) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = poke.formula) +
  stat_poly_eq(formula = poke.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  #xlim(216, 216.4) +
  #ylim(0,1500) +
  theme_light() +
  ggtitle("Poker1 all measured Q")


### STRT ####
strt.stream.one.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQZ42rKNca5fKJ-aXyrAf7ZkdmfHoHidtKdmNT7jD7BLJVNoRzp6xnVzg98F-_XGNjRN5ZXlzFDjWKg/pub?output=csv"
strt.stream.two.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSXPDD-aCfqJ8R8mF5W0aWQ_wnFQMGc_ioP5qtgadW8rsjzfKCouALYcC3VCC65U6uUApPffsX8pPo0/pub?output=csv"

# load in data 
strt.stream.one.2022 <- read.csv(url(strt.stream.one.2022.url), skip = 1) 
strt.stream.two.2022 <- read.csv(url(strt.stream.two.2022.url), skip = 1) 


# clean for merging purposes
strt.stream.one.2022 <- strt.stream.one.2022[, -c(5:8)] # removing columns that arent date/abs pressure and temp
names(strt.stream.one.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

strt.stream.two.2022 <- strt.stream.two.2022[, -c(5:8)] # removing columns that arent date/abs pressure and temp
names(strt.stream.two.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

# merge to one 
strt.final.pressure.2022 <- left_join(strt.stream.one.2022, strt.stream.two.2022, by = c("DateTimeGMT"))
strt.final.pressure.2022$MeanPressure <- rowMeans(strt.final.pressure.2022[,c(3,6)], na.rm = TRUE)


# join the two atmospheric and water pressure together
STRT.2022 <- left_join(moos.atmo.2022, strt.final.pressure.2022, by = "DateTimeGMT")

STRT.2022$DateTime <- mdy_hms(STRT.2022$DateTimeGMT, tz = "GMT")
attributes(STRT.2022$DateTime)$tzone <- 'America/Anchorage'

# Water pressure - atmospheric pressure 
STRT.2022$difference <- STRT.2022$MeanPressure - STRT.2022$AirPressure
STRT.2022 <- STRT.2022[,-c(2:10)] # only want Site/WaterPressure/DateTime/Difference
STRT.2022$Site <- "STRT"

# Checking closeness between two PTs
strt.stream.two.2022 <- strt.stream.two.2022[1:nrow(strt.stream.one.2022),]
strt.stream.one.2022$Site <- "STRT1" #add column identifier
strt.stream.two.2022$Site <- "STRT2"
strt.pt.2022 <- bind_rows(strt.stream.one.2022, strt.stream.two.2022)

plot(x = strt.stream.one.2022$WaterPressure, y = strt.stream.two.2022$WaterPressure, main = "Stuart PT comparison",
     xlab = "Stuart1PT", 
     ylab = "Stuart2PT")
abline(1,1)

ggplot(strt.pt.2022, aes(x = DateTimeGMT, y = WaterPressure)) +
  geom_point() +
  facet_wrap(~Site)


### Filter STRT ###
QSummary.ST.2022 <- QSummary.2022 %>% filter(Site =="STRT")

Stuart1comb.2022 <- full_join(STRT.2022, QSummary.ST.2022)
STRT1.lm.2022 <- lm(Stuart1comb.2022$MeasuredQ_Ls ~ Stuart1comb.2022$difference)
#POKE2.lm.2022 <- lm(Poker1comb.2022$MeasuredQ_Ls ~ Poker1comb.2022$WaterPressure)


strt.formula <- y ~ x

ggplot(aes(x = difference, y = MeasuredQ_Ls), data = Stuart1comb.2022) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = strt.formula) +
  stat_poly_eq(formula = strt.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  #xlim(216, 216.4) +
  #ylim(0,1500) +
  theme_light() +
  ggtitle("Stuart1 all measured Q")


### FRCH ####
frch.stream.one.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTGeOE-b-tvYIqLruE4YTUM_D9VXU3aveSkAAa0RK0HCpmlDezPsQzo4DLVpir-dGUyObDxZnf2TfaK/pub?output=csv"
frch.stream.two.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSW0-Yfk7-7IpXqVOiytfc25Pff3HF-_QlSoTyNWpEkrQdF5iYgDj0LZGLcKk6d8XWH1tMvqzHf3lX9/pub?output=csv"

# load in url
frch.stream.one.2022 <- read.csv(url(frch.stream.one.2022.url), skip = 1) 
frch.stream.two.2022 <- read.csv(url(frch.stream.two.2022.url), skip = 1) 

# clean for merging purposes
frch.stream.one.2022 <- frch.stream.one.2022[, -c(5:13)] # removing columns that arent date/abs pressure and temp
names(frch.stream.one.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

frch.stream.two.2022 <- frch.stream.two.2022[, -c(5:17)] # removing columns that arent date/abs pressure and temp
names(frch.stream.two.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")

# merge to one 
frch.final.pressure.2022 <- left_join(frch.stream.one.2022, frch.stream.two.2022, by = c("DateTimeGMT"))
frch.final.pressure.2022$MeanPressure <- rowMeans(frch.final.pressure.2022[,c(3,6)], na.rm = TRUE)


# join the two atmospheric and water pressure together
FRCH.2022 <- right_join(moos.atmo.2022, frch.final.pressure.2022, by = "DateTimeGMT")

FRCH.2022$DateTime <- mdy_hms(FRCH.2022$DateTimeGMT, tz = "GMT")
attributes(FRCH.2022$DateTime)$tzone <- 'America/Anchorage'

# Water pressure - atmospheric pressure 
FRCH.2022$difference <- FRCH.2022$MeanPressure - FRCH.2022$AirPressure
FRCH.2022 <- FRCH.2022[,-c(2:10)] # only want Site/WaterPressure/DateTime/Difference
FRCH.2022$Site <- "FRCH"

# Checking closeness between two PTs
frch.stream.two.2022 <- frch.stream.two.2022[1:nrow(frch.stream.one.2022),]
frch.stream.one.2022$Site <- "STRT1" #add column identifier
frch.stream.two.2022$Site <- "STRT2"
frch.pt.2022 <- bind_rows(frch.stream.one.2022, frch.stream.two.2022)

plot(x = frch.stream.one.2022$WaterPressure, y = frch.stream.two.2022$WaterPressure, main = "French PT comparison",
     xlab = "French1PT", 
     ylab = "French2PT")
abline(1,1)


### Filter FRCH ###
QSummary.FR.2022 <- QSummary.2022 %>% filter(Site =="FRCH")

French1comb.2022 <- full_join(FRCH.2022, QSummary.FR.2022)
FRCH1.lm.2022 <- lm(French1comb.2022$MeasuredQ_Ls ~ French1comb.2022$difference)
#POKE2.lm.2022 <- lm(Poker1comb.2022$MeasuredQ_Ls ~ Poker1comb.2022$WaterPressure)


frch.formula <- y ~ x

ggplot(aes(x = difference, y = MeasuredQ_Ls), data = French1comb.2022) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = frch.formula) +
  stat_poly_eq(formula = frch.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  #xlim(216, 216.4) +
  #ylim(0,1500) +
  theme_light() +
  ggtitle("French1 all measured Q")

### VAUL ####
vaul.stream.one.2022.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ-ztCTFptHNaTaoxpXL3xN5g58Jkdoywv0J3qAR45t65rtubHbsw7mpFt2WGn3_hhbbxr7_QY7jWhP/pub?output=csv"

# load in url
vaul.stream.one.2022 <- read.csv(url(vaul.stream.one.2022.url), skip = 1) 

# clean for merging purposes
vaul.stream.one.2022 <- vaul.stream.one.2022[, -c(5:13)] # removing columns that arent date/abs pressure and temp
names(vaul.stream.one.2022) <- c("Site", "DateTimeGMT", "WaterPressure", "TempC")


# # merge to one 
# frch.final.pressure.2022 <- left_join(frch.stream.one.2022, frch.stream.two.2022, by = c("DateTimeGMT"))
# frch.final.pressure.2022$MeanPressure <- rowMeans(frch.final.pressure.2022[,c(3,6)], na.rm = TRUE)


# join the two atmospheric and water pressure together
VAUL.2022 <- left_join(moos.atmo.2022, vaul.stream.one.2022, by = "DateTimeGMT")

VAUL.2022$DateTime <- mdy_hms(VAUL.2022$DateTimeGMT, tz = "GMT")
attributes(VAUL.2022$DateTime)$tzone <- 'America/Anchorage'

# Water pressure - atmospheric pressure 
VAUL.2022$difference <- VAUL.2022$WaterPressure - VAUL.2022$AirPressure
VAUL.2022 <- VAUL.2022[,-c(2:5,7)] # only want Site/WaterPressure/DateTime/Difference
VAUL.2022$Site <- "VAUL"


### Filter VAUL ###
QSummary.VA.2022 <- QSummary.2022 %>% filter(Site =="VAUL")

Vault1comb.2022 <- full_join(VAUL.2022, QSummary.VA.2022)
VAUL1.lm.2022 <- lm(Vault1comb.2022$MeasuredQ_Ls ~ Vault1comb.2022$difference)
#POKE2.lm.2022 <- lm(Poker1comb.2022$MeasuredQ_Ls ~ Poker1comb.2022$WaterPressure)


vaul.formula <- y ~ x

ggplot(aes(x = difference, y = MeasuredQ_Ls), data = Vault1comb.2022) +
  geom_point(aes(color = Method), size = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = vaul.formula) +
  stat_poly_eq(formula = vaul.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  #xlim(216, 216.4) +
  #ylim(0,1500) +
  theme_light() +
  ggtitle("Vault1 all measured Q")




