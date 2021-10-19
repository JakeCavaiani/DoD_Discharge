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
library(here)



dir.create(here("Rating_curve"))
dir.create(here("Rating_curve", "Plots"))
dir.create(here("Rating_curve", "Plots", "FRCH"))
dir.create(here("Rating_curve", "Plots", "MOOS"))
dir.create(here("Rating_curve", "Plots", "POKE"))
dir.create(here("Rating_curve", "Plots", "STRT"))
dir.create(here("Rating_curve", "Plots", "VAUL"))


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















