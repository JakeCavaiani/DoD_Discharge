### Read me ###
# the purpose of this script is to export DoD water level data and process it into continuous discharge (Q) once we calculate rating curves
# you can collapse into years #

# Important NOTES:
# 1) Water level data is obtained from HOBO pressure transducers (PTs) that were installed in PVC pipes in streams, with the PT sitting on top of a rebar piece at bottom of pipe. Raw pressure from PTs were processed in HOBOware to correct for atmospheric pressure to get water depth. Atmospheric pressure was obtained from a PT installed at each site in a tree. HOWEVER, water depth is absolute PT depth, NOT actual water depth. To get actual water depth, we need a reference water depth from a time point in the water depth time series that is an accurate measure of the depth of the pt from the water surface. We do not have these (depth measurements from flow meter measurements were not done sufficiently close to PT locations to use). We must therefore rely on the rating curve to convert absolute water depth to continuous Q.

# 2) Date/time from HOBOware data is GMT-8, which is Alaska Daylight Time, which is the correct timezone for this project in summer (AK is in GMT-9 March-November when daylight savings is not observed). Data/time therefore needs to be formatted but not timezone-converted.

# 3) 2019 data is read in from 019 PT data is from DoD->2019 ak sensors->2019 sensor data->Q -> Pressure Transducer Data->Depth->'site'
# 2020 data is read in from DoD Project->2020 AK sensors->Discharge->Pressure Transducer->Water Level

# Step 1: import raw data files which is site, datetime, absolute pressure and water level
# Step 2: Clean errant points within the data that could be due to installation/decommission or gaps in data
# Step 3: Write final output of cleaned site, datetime, absolute pressure and water level

### load packages ### 
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
library(neonUtilities)
library(raster)



dir.create(here("PT_data"))
dir.create(here("PT_data", "2019"))
dir.create(here("PT_data", "2019", "FRCH"))
dir.create(here("PT_data", "2019", "VAUL"))
dir.create(here("PT_data", "2019", "POKE"))
dir.create(here("PT_data", "2019", "STRT"))
dir.create(here("PT_data", "2019", "MOOS"))
dir.create(here("PT_data", "2019", "plots"))

################################# 2015 #######################################################################
# Import Raw Data #

moos.stream.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpuXf_81xoWO_N3uJY8qT_ZY-a9CfjFsl-UavJ8vXjBkfiR3cT9YcXo8xXncjN0Yph_6tHEY9iMJVM/pub?output=csv"

frch.stream.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVR3fDBQNFLI3kvdH2jgM_ktS2S4MuAnBnMct6LC3_Ph2xAwBWQP7XHkMvDDF1oiSkB58NrANhu601/pub?output=csv"



# Load Data#
moos.stream <- read.csv(url(moos.stream.url), skip = 24)

frch.stream <- read.csv(url(frch.stream.url), skip = 1)

moos.stream <- moos.stream[,-c(5:7)] # Remove Na columns


# Rename columns #
names(moos.stream) <- c("Date", "Time", "AbsolutePressure", "WaterLevel")

names(frch.stream) <- c("Date", "Time", "AbsolutePressure", "WaterLevel")

# Convert time and put in AK time #
moos.stream$Date <- mdy(moos.stream$Date)
moos.stream$DateTime <- as.POSIXct(paste(moos.stream$Date, moos.stream$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

frch.stream$Date <- mdy(frch.stream$Date)
frch.stream$DateTime <- as.POSIXct(paste(frch.stream$Date, frch.stream$Time), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

# Filtering out data #
### MOOS ###
moos.stream <- moos.stream %>% subset(moos.stream$DateTime < "2015-09-20 18:30:00") # PT was taken out on the 21st 
plot(x = moos.stream$DateTime, y = moos.stream$WaterLevel) # Plot check

### FRCH ###
frch.stream <- frch.stream %>%  subset(frch.stream$DateTime < "2015-09-20 18:30:00") # PT was taken out on the 21st 

frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:10"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:15"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:20"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:25"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:30"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:35"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:40"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:45"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:50"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 12:55"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:00"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:05"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:10"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:15"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:20"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:25"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:30"] <- NA
frch.stream$DateTime[frch.stream$DateTime == "2015-08-10 13:35"] <- NA

plot(x = frch.stream$DateTime, y = frch.stream$WaterLevel) # plot check


getwd()
### Write CSV ###
write.csv(moos.stream,"PT_data/2015/moos.pt.2015.csv", row.names = FALSE)
write.csv(frch.stream,"PT_data/2015/frch.pt.2015.csv", row.names = FALSE)

############################################## 2018 ###########################################################
# Import Data #
French1.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQeBCz7Rq59AjhTnjeJH_H9ot8gtiujyv9-W7KOAKYacPFizjOp4KqxGbhMWmsT756KEGzBYQjeRpgz/pub?output=csv"
French2.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRCm-lCZI01xF1g4ytFCMECZJoUhjjEd11_cQTLDGzyv_4GHeLVEwX5alAVyO8hiDLzJOwWVYQui7E_/pub?output=csv"
Moose1.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTAzlkTb2-hfKPTXP1ZWp1vYCqK1irV4Gy2UMcjF7HSDdUq1WZCRNBfJs07VDrB7dGeHgIalOzrsKPG/pub?output=csv"
Moose2.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTGobrtIaYMa8-jDTYvO2s9OBsnQyksLPLRXPycD0pLi3_Asd786dK0XGnYowobM0p7NLZM5qpxmk9S/pub?output=csv"

# read in data #
French1 <- read.csv(url(French1.url))
French2 <- read.csv(url(French2.url))
Moose1 <- read.csv(url(Moose1.url))
Moose2 <- read.csv(url(Moose2.url))

# Convert time and put in AK time 
French1$DateTime <- mdy_hm(French1$DateTimeGMT, tz="GMT") #convert date format.
attributes(French1$DateTime)$tzone <-'America/Anchorage'
French2$DateTime <- mdy_hm(French2$DateTimeGMT, tz="GMT") #convert date format.
attributes(French2$DateTime)$tzone <-'America/Anchorage'
Moose1$DateTime <- mdy_hm(Moose1$DateTimeGMT, tz="GMT") #convert date format.
attributes(Moose1$DateTime)$tzone <-'America/Anchorage'
Moose2$DateTime <- mdy_hm(Moose2$DateTimeGMT, tz="GMT") #convert date format.
attributes(Moose2$DateTime)$tzone <-'America/Anchorage'

# format data, combine all french, combine all moose, combine french and moose
French2 <- French2[1:nrow(French1),] # French 2 has a few extra rows on it from when stopping logger. Stripping them.
French1$name <- "French1" #add column identifier
French2$name <- "French2"
allfrench <- bind_rows(French1, French2) # combine 1&2 into one dataframe French
Moose1$name <- "Moose1" #add column indentifier
Moose2$name <- "Moose2"
allmoose <- bind_rows(Moose1, Moose2) # combine 1&2 into one dataframe Moose
frenchmoose <- bind_rows(allfrench, allmoose) # combine all french and all moose

# check closeness between two HOBOs at each station
plot(x= French1$WaterLevelmeters[100:nrow(French1)], y=French2$WaterLevelmeters[100:nrow(French2)], 
     main = "French PT comparison", xlab = "French1 PT", ylab = "French2 PT")

plot(x= Moose1$WaterLevelmeters[100:nrow(Moose1)-15], y=Moose2$WaterLevelmeters[100:nrow(Moose2)-15], 
     main = "Moose PT comparison", xlab = "Moose1 PT", ylab = "Moose2 PT")

# French Plots
allfrench %>% 
  filter(WaterLevelmeters > 184) %>% 
  ggplot() + 
  geom_point(aes(x=DateTime, y=WaterLevelmeters, color=name)) +
  theme_classic() +
  ggtitle("French") +
  scale_color_brewer(palette = "Paired")

# Moose Plots
allmoose %>% 
  filter(WaterLevelmeters > 166) %>% 
  ggplot() + 
  geom_point(aes(x=DateTime, y=WaterLevelmeters, color=name)) +
  theme_classic() +
  ggtitle("Moose") +
  scale_color_brewer(palette = "Paired")


############################################## 2019 #########################################################
### Import Data ###
frch.stream.2019.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vStPTLYv7j4tC3hGRHQ3rW8oCTx9_I0bekcLBjCl4jKQT8GLImI_hXp9qq6UsmdVAaPd7vr4r3BsLZJ/pub?output=csv"
frch.stream.2019.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQmHJ-1aQ_8UWAPq-v_us3gz-JTv7vsdVVKNnnMUloJwJNK7TTgvU8kLeUOCbIYU_mHz8v1k1CFz2Vl/pub?output=csv"
vaul.stream.2019.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSo7CfzMCKXdPmdVZ2c8tJQ-_NzKkje0QWYIseiLxH82hJeJZZ1wtiuL6ZleDoEaPPJMzuWdqB3NaAQ/pub?output=csv"
poke.stream.2019.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR5CEMDu-NFH49FcPmbf_QglRqVaEV-0xgcGJWz3kWuuGP8pwI-OhXtSZCwN4uwBlOq0CuuQ9tMYLXX/pub?output=csv"
poke.stream.2019.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTInrTbsXhYZ0Hcn0YZIBF7LWimNdO0V1e_06hKNaIriwxszvphODlUDfRnT_5_Xgi63k2WFW4q8EKm/pub?output=csv"
strt.stream.2019.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTgVqJmbVgtXVDGQL_SiQqHphyUBuXG-w0bCk8mLn-IksFrqg3PMvRveGizqHM9lhq_rhqozKseAD_7/pub?output=csv"
strt.stream.2019.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRNwL17hN8tMuyzDCDKCTeXGjQ1eN7j881D0-pyi46PhTK7LwoqQ_jrwZWQrSypd3icId9KFpqsuatj/pub?output=csv"
moos.stream.2019.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQMF0fbfNRwgU-9DfsCJ7LpPd_LoAL01gcSdHzNIFXJ5sSnXwH8Vvsqyj_nZAeWVcXGDWhyJP2TbjZq/pub?output=csv"


### read in data ###
frch.stream.one.2019 <- read.csv(url(frch.stream.2019.url))
frch.stream.two.2019 <- read.csv(url(frch.stream.2019.url.two))
vaul.stream.one.2019 <- read.csv(url(vaul.stream.2019.url))
poke.stream.one.2019 <- read.csv(url(poke.stream.2019.url))
poke.stream.two.2019 <- read.csv(url(poke.stream.2019.url.two))
strt.stream.one.2019 <- read.csv(url(strt.stream.2019.url)) # only goes to august
strt.stream.two.2019 <- read.csv(url(strt.stream.2019.url.two)) # goes all the way to october.
moos.stream.one.2019 <- read.csv(url(moos.stream.2019.url))

# Rename column headers # 
names(frch.stream.one.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(frch.stream.two.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(vaul.stream.one.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(poke.stream.one.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(poke.stream.two.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(strt.stream.one.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(strt.stream.two.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")
names(moos.stream.one.2019) <- c("Site", "DateTimeGMT", "Temp", "AbsPTDepth")

# Input NA for missing time #
frch.stream.one.2019$DateTimeGMT[frch.stream.one.2019$DateTimeGMT == ""] <- NA
frch.stream.two.2019$DateTimeGMT[frch.stream.two.2019$DateTimeGMT == ""] <- NA

vaul.stream.one.2019$DateTimeGMT[vaul.stream.one.2019$DateTimeGMT == ""] <- NA

poke.stream.one.2019$DateTimeGMT[poke.stream.one.2019$DateTimeGMT == ""] <- NA
poke.stream.two.2019$DateTimeGMT[poke.stream.two.2019$DateTimeGMT == ""] <- NA

strt.stream.one.2019$DateTimeGMT[strt.stream.one.2019$DateTimeGMT == ""] <- NA
strt.stream.two.2019$DateTimeGMT[strt.stream.two.2019$DateTimeGMT == ""] <- NA

moos.stream.one.2019$DateTimeGMT[moos.stream.one.2019$DateTimeGMT == ""] <- NA


# Convert time and put in AK time #
frch.stream.one.2019$DateTime <- as.POSIXct(paste(frch.stream.one.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
frch.stream.one.2019$DateTime <- lubridate::round_date(frch.stream.one.2019$DateTime, "15 minutes")

frch.stream.two.2019$DateTime <- as.POSIXct(paste(frch.stream.two.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

vaul.stream.one.2019$DateTime <- as.POSIXct(paste(vaul.stream.one.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

poke.stream.one.2019$DateTime <- as.POSIXct(paste(poke.stream.one.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.stream.one.2019$DateTime <- lubridate::round_date(poke.stream.one.2019$DateTime, "15 minutes")

poke.stream.two.2019$DateTime <- as.POSIXct(paste(poke.stream.two.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

strt.stream.one.2019$DateTime <- as.POSIXct(paste(strt.stream.one.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.stream.two.2019$DateTime <- as.POSIXct(paste(strt.stream.two.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

moos.stream.one.2019$DateTime <- as.POSIXct(paste(moos.stream.one.2019$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

# filter out take out of water points #
frch.stream.one.2019 <- frch.stream.one.2019 %>% subset(frch.stream.one.2019$DateTime > "2019-04-29" & frch.stream.one.2019$DateTime < "2019-10-10") # removed on 10/10
frch.stream.two.2019 <- frch.stream.two.2019 %>% subset(frch.stream.two.2019$DateTime > "2019-04-29" & frch.stream.two.2019$DateTime < "2019-10-10") # removed on 10/10

vaul.stream.one.2019 <- vaul.stream.one.2019 %>% subset(vaul.stream.one.2019$DateTime > "2019-06-07" & vaul.stream.one.2019$DateTime < "2019-10-18") # removed on 10/18

poke.stream.one.2019 <- poke.stream.one.2019 %>% subset(poke.stream.one.2019$DateTime > "2019-05-10" & poke.stream.one.2019$DateTime < "2019-10-14") # removed on 10/18 but erroneous values starting after the 14th....maybe ice...maybe beaver dam?
poke.stream.two.2019 <- poke.stream.two.2019 %>% subset(poke.stream.two.2019$DateTime > "2019-05-14" & poke.stream.two.2019$DateTime < "2019-10-18") # removed on 10/18

strt.stream.one.2019 <- strt.stream.one.2019 %>% subset(strt.stream.one.2019$DateTime > "2019-05-21" & strt.stream.one.2019$DateTime < "2019-10-14") 
strt.stream.two.2019 <- strt.stream.two.2019 %>% subset(strt.stream.two.2019$DateTime > "2019-05-21" & strt.stream.two.2019$DateTime < "2019-10-13") # removed on 10/16 but erroneous values starting after the 14th....maybe ice...maybe beaver dam?

### combine data sets ### 
frch.stream.two.2019 <- frch.stream.two.2019[1:nrow(frch.stream.one.2019),]
frch.stream.one.2019$Site <- "FRCH1" #add column identifier
frch.stream.two.2019$Site <- "FRCH2"
frch.pt.2019 <- bind_rows(frch.stream.one.2019, frch.stream.two.2019)

vaul.pt.2019 <- vaul.stream.one.2019
vaul.pt.2019$Site <- "VAUL1"

poke.stream.two.2019 <- poke.stream.two.2019[1:nrow(poke.stream.one.2019),]
poke.stream.one.2019$Site <- "POKE1" #add column identifier
poke.stream.two.2019$Site <- "POKE2"
poke.pt.2019 <- bind_rows(poke.stream.one.2019, poke.stream.two.2019)

#strt.stream.one.2019 <- strt.stream.one.2019[1:nrow(strt.stream.two.2019),]
#strt.stream.one.2019$Site <- "STRT1" #add column identifier
#strt.stream.two.2019$Site <- "STRT2"
#strt.pt.2019 <- bind_rows(strt.stream.one.2019, strt.stream.two.2019)


moos.pt.2019 <- moos.stream.one.2019
moos.pt.2019$Site <- "MOOS1"

# Checking closeness between two PT #
plot(x = frch.stream.one.2019$AbsPTDepth, y = frch.stream.two.2019$AbsPTDepth, main = "French PT comparison",
     xlab = "French1 PT", 
     ylab = "French2 PT")
abline(1,1) 

plot(x = vaul.stream.one.2019$DateTime, y = vaul.stream.one.2019$AbsPTDepth, main = "Vault PT comparison",
     xlab = "DateTime", 
     ylab = "VaultPT")

plot(x = poke.stream.one.2019$AbsPTDepth, y = poke.stream.two.2019$AbsPTDepth, main = "Poker PT comparison", 
     xlab = "Poker1PT", 
     ylab = "Poker2PT")
abline(1,1)

plot(x = poke.stream.one.2019$DateTime, y = poke.stream.one.2019$AbsPTDepth, main = "Vault PT comparison",
     xlab = "DateTime", 
     ylab = "Poke PT")
plot(x = poke.stream.two.2019$DateTime, y = poke.stream.two.2019$AbsPTDepth, main = "Vault PT comparison",
     xlab = "DateTime", 
     ylab = "Poke PT")

plot(x = strt.stream.one.2019$AbsPTDepth, y = strt.stream.two.2019$AbsPTDepth, main = "Stuart PT comparison",
     xlab = "Stuart1 PT", 
     ylab = "Stuart2 PT")
abline(1,1)

plot(x = moos.stream.one.2019$DateTime, y = moos.stream.one.2019$AbsPTDepth, main = "Vault PT comparison",
     xlab = "DateTime", 
     ylab = "Moose PT")

### Water Level Plots ###
ggplot(frch.pt.2019) + 
  geom_line(aes(x = DateTime , y= AbsPTDepth, color = Site), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("French PT comparison") + 
  scale_color_brewer(palette = "Paired")


ggplot(vaul.pt.2019) + 
  geom_line(aes(x = vaul.pt.2019$DateTime , y= vaul.pt.2019$AbsPTDepth), size=1.25) +
  xlab("Date") +
  ylab("Depth to Sensor") +
  theme_classic() +
  ggtitle("Vault PT comparison") 


ggplot(poke.pt.2019) + 
  geom_line(aes(x = DateTime , y= AbsPTDepth, color = Site), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("Poker PT comparison") + 
  scale_color_brewer(palette = "Paired")


ggplot(strt.stream.two.2019) + 
  geom_line(aes(x =DateTime , y= AbsPTDepth), size=1.25) +
  xlab("Date") +
  ylab("Depth to Sensor") +
  theme_classic() +
  ggtitle("Stuart PT comparison") + 
  scale_color_brewer(palette = "Paired")

ggplot(moos.pt.2019) + 
  geom_line(aes(x = DateTime , y= AbsPTDepth), size=1.25) +
  xlab("Date") +
  ylab("Depth to Sensor") +
  theme_classic() +
  ggtitle("Moose PT comparison") 

all.pt.2019 <- rbind(frch.pt.2019, vaul.pt.2019, poke.pt.2019, strt.pt.2019, moos.pt.2019)
### Write CSV ###
write.csv(frch.pt.2019, "~/Documents/DoD_Discharge/PT_data/2019/FRCH/frch.pt.2019.csv")
write.csv(vaul.pt.2019, "~/Documents/DoD_Discharge/PT_data/2019/VAUL/vaul.pt.2019.csv")
write.csv(poke.pt.2019, "~/Documents/DoD_Discharge/PT_data/2019/POKE/poke.pt.2019.csv")
write.csv(strt.pt.2019, "~/Documents/DoD_Discharge/PT_data/2019/STRT/strt.pt.2019.csv")
write.csv(moos.pt.2019, "~/Documents/DoD_Discharge/PT_data/2019/MOOS/moos.pt.2019.csv")
write.csv(all.pt.2019, "~/Documents/DoD_Discharge/PT_data/2019/all.pt.2019.csv")
############################################### 2020 #############################################################
### Import Raw Data ### 
# VAUL only has one because data for PT1 was very erroneous 
moos.stream.2020.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS_IoyPoUK6tDeBoPsTibXES49eORxi6SUzt5Df3iSmMmIXncaIAQWlsmzKwjecJelrcKDL3rryyKjS/pub?output=csv"
moos.stream.2020.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRb88DUzb4g2wJjmw0wfBatqS6Hg90epELO1sMlnQfzuI0t7aTWuqXYIY18r-T8mt0koTH4gc041PxL/pub?output=csv"
frch.stream.2020.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQQ0F4a3e4MTwKTChJmS3GTwC_ENETsM_CoJNq6awdnXzXnl1nQKizm63JjFg3WUbXZusXBLt_DNcMX/pub?output=csv"
frch.stream.2020.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRtJhzv4dunVMRCmolhShz29XDicFk2wFafUUC4ZDWaOs7z-5Q--vEJWCcYJsVEikjXYql6fc3TZV63/pub?output=csv"
strt.stream.2020.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSE57c7ol5XBElx6nIOm4fl4UFB5qgffo49nSw2RuJ_kIZAC43vLusqtjQy5h2h3bNikmJwDvIBEO7X/pub?output=csv"
strt.stream.2020.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSPRQxqqKl_N36uEzbMC1_T5v8k67aUyJhhMYZvsmwJKafU2NmL1NtbmOW8BvtqWqxSTY3ndYnjzy_f/pub?output=csv"
vaul.stream.2020.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVA4AxNNaYJY6hJCD7c7Y4jloR68x1Zols2Grg7xiKx-gVQlqh5yb3e3L5XkFUXyRn0GnD1nRi_XXJ/pub?output=csv"
poke.stream.2020.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQwtaiz8D9c4QelUMsM5BwCMu4lu_Eqlo6IwTC0s-MXE-a075hQ4xgEvMovGrrMuv9_qX7cCYJvISrN/pub?output=csv"
poke.stream.2020.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQuyeltwK25QUepwlUKTSL9SAFCwgTtxs6YscILt_npSKjFV1mY5DVrs-RxGivvpg7ZCKfGuej0DOJO/pub?output=csv"

# Load Data#
moos.stream.one.2020 <- read.csv(url(moos.stream.2020.url), skip = 491) # started in lab it looks like 
moos.stream.two.2020 <- read.csv(url(moos.stream.2020.url.two), skip = 1)

frch.stream.one.2020 <- read.csv(url(frch.stream.2020.url), skip = 1)
frch.stream.two.2020 <- read.csv(url(frch.stream.2020.url.two), skip = 4)


strt.stream.one.2020 <- read.csv(url(strt.stream.2020.url), skip = 1)
strt.stream.two.2020 <- read.csv(url(strt.stream.2020.url.two), skip = 1)

vaul.stream.2020 <- read.csv(url(vaul.stream.2020.url), skip = 1)

poke.stream.one.2020 <- read.csv(url(poke.stream.2020.url), skip = 1)
poke.stream.two.2020 <- read.csv(url(poke.stream.2020.url.two), skip = 1)

# Rename columns #
names(moos.stream.one.2020) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")
names(moos.stream.two.2020) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

names(frch.stream.one.2020) <- c("Site", "DateTimeGMT", "Absolute Pressure", "WaterLevel")
names(frch.stream.two.2020) <- c("Site", "DateTimeGMT", "Absolute Pressure", "WaterLevel")

names(strt.stream.one.2020) <- c("Site", "DateTimeGMT", "Absolute Pressure", "WaterLevel")
names(strt.stream.two.2020) <- c("Site", "DateTimeGMT", "Absolute Pressure", "WaterLevel")

names(vaul.stream.2020) <- c("Site", "DateTimeGMT", "Absolute Pressure", "WaterLevel")

names(poke.stream.one.2020) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")
names(poke.stream.two.2020) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

# Input NA for missing time #
moos.stream.one.2020$DateTimeGMT[moos.stream.one.2020$DateTimeGMT == ""] <- NA
moos.stream.two.2020$DateTimeGMT[moos.stream.two.2020$DateTimeGMT == ""] <- NA

frch.stream.one.2020$DateTimeGMT[frch.stream.one.2020$DateTimeGMT == ""] <- NA
frch.stream.two.2020$DateTimeGMT[frch.stream.two.2020$DateTimeGMT == ""] <- NA

strt.stream.one.2020$DateTimeGMT[strt.stream.one.2020$DateTimeGMT == ""] <- NA
strt.stream.two.2020$DateTimeGMT[strt.stream.two.2020$DateTimeGMT == ""] <- NA

vaul.stream.2020$DateTimeGMT[vaul.stream.2020$DateTimeGMT == ""] <- NA

poke.stream.one.2020$DateTimeGMT[poke.stream.one.2020$DateTimeGMT == ""] <- NA
poke.stream.two.2020$DateTimeGMT[poke.stream.two.2020$DateTimeGMT == ""] <- NA


# Convert time and put in AK time #
moos.stream.one.2020$DateTime <- mdy_hms(moos.stream.one.2020$DateTimeGMT, tz = "GMT")
attributes(moos.stream.one.2020$DateTime)$tzone <- 'America/Anchorage'
moos.stream.two.2020$DateTime <- mdy_hms(moos.stream.two.2020$DateTimeGMT, tz = "GMT")
attributes(moos.stream.two.2020$DateTime)$tzone <- 'America/Anchorage'

frch.stream.one.2020$DateTime <- mdy_hms(frch.stream.one.2020$DateTimeGMT, tz = "GMT")
attributes(frch.stream.one.2020$DateTime)$tzone <- 'America/Anchorage'
frch.stream.two.2020$DateTime <- mdy_hms(frch.stream.two.2020$DateTimeGMT, tz = "GMT")
attributes(frch.stream.two.2020$DateTime)$tzone <- 'America/Anchorage'

strt.stream.one.2020$DateTime <- mdy_hms(strt.stream.one.2020$DateTimeGMT, tz = "GMT")
attributes(strt.stream.one.2020$DateTime)$tzone <- 'America/Anchorage'
strt.stream.two.2020$DateTime <- mdy_hms(strt.stream.two.2020$DateTimeGMT, tz = "GMT")
attributes(strt.stream.two.2020$DateTime)$tzone <- 'America/Anchorage'

vaul.stream.2020$DateTime <- mdy_hms(vaul.stream.2020$DateTimeGMT, tz = "GMT")
attributes(vaul.stream.2020$DateTime)$tzone <- 'America/Anchorage'

poke.stream.one.2020$DateTime <- mdy_hms(poke.stream.one.2020$DateTimeGMT, tz = "GMT")
attributes(poke.stream.one.2020$DateTime)$tzone <- 'America/Anchorage'
poke.stream.two.2020$DateTime <- mdy_hms(poke.stream.two.2020$DateTimeGMT, tz = "GMT")
attributes(poke.stream.two.2020$DateTime)$tzone <- 'America/Anchorage'

### Filtering out data ###
### MOOS ###
moos.stream.one.2020 <- moos.stream.one.2020 %>% subset(moos.stream.one.2020$DateTime < "2020-10-14") # cleaning data that is below 165.5 because those are errant and then before 10/14 because thats when we took the PTs out
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 05:15"] <- NA
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 05:30"] <- NA
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 05:45"] <- NA
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 06:00"] <- NA
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 06:15"] <- NA
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 06:30"] <- NA
moos.stream.one.2020$DateTime[moos.stream.one.2020$DateTime == "2020-08-17 06:45"] <- NA

plot(moos.stream.one.2020$WaterLevel ~ moos.stream.one.2020$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-27 04:45:00","2020-06-30 06:45:00"), tz="America/Anchorage"))
moos.stream.one.2020[c(1115:1499), 4] <- NA # Setting NA to noisy part of the data set from the 27th to 30th

plot(x = moos.stream.one.2020$DateTime, y = moos.stream.one.2020$WaterLevel) # Plot check

moos.stream.two.2020 <- moos.stream.two.2020 %>% subset(moos.stream.two.2020$DateTime < "2020-10-14")

plot(x = moos.stream.two.2020$DateTime, y = moos.stream.two.2020$WaterLevel) #plot check

### FRCH ###
frch.stream.one.2020 <- frch.stream.one.2020  %>%  subset(frch.stream.one.2020$DateTime < "2020-10-14") #cleaning data that is before the 14th (Site was taken down the 15th)
frch.stream.one.2020$DateTime[frch.stream.one.2020$DateTime == "2020-06-11 4:45"] <- NA
frch.stream.one.2020$DateTime[frch.stream.one.2020$DateTime == "2020-06-11 5:00"] <- NA
frch.stream.one.2020$DateTime[frch.stream.one.2020$DateTime == "2020-06-11 5:15"] <- NA
frch.stream.one.2020$DateTime[frch.stream.one.2020$DateTime == "2020-06-28 21:15"] <- NA
frch.stream.one.2020$DateTime[frch.stream.one.2020$DateTime == "2020-06-29 14:45"] <- NA

plot(x = frch.stream.one.2020$DateTime, y = frch.stream.one.2020$WaterLevel) # plot check
frch.stream.one.2020[c(1039:3822), 4] <- NA # Setting NA to noisy part of the data set
frch.stream.two.2020 <- frch.stream.two.2020 %>%  subset(frch.stream.two.2020$DateTime < "2020-10-14") #cleaning data that is before the 14th (Site was taken down the 15th)

plot(x = frch.stream.two.2020$DateTime, y = frch.stream.two.2020$WaterLevel) # plot check

### STRT ###
strt.stream.one.2020 <- strt.stream.one.2020 %>% subset(strt.stream.one.2020$DateTime < "2020-10-8") # cleaning data that is before October 8th due to ice in channel (Site was taken down Oct 13th)

plot(x = strt.stream.one.2020$DateTime, y = strt.stream.one.2020$WaterLevel) # plot check

strt.stream.two.2020 <- strt.stream.two.2020 %>% subset(strt.stream.two.2020$DateTime < "2020-10-08") # cleaning data that is before October 8th due to ice in channel (Site was taken down Oct 13th)

strt.stream.two.2020$DateTime[strt.stream.two.2020$DateTime == "2020-06-24 8:45"] <- NA

plot(x = strt.stream.two.2020$DateTime, y = strt.stream.two.2020$WaterLevel) # plot check

### VAUL ###
vaul.stream.2020 <- vaul.stream.2020 %>% subset(vaul.stream.2020$DateTime < "2020-10-05") # Cleaning data that is before October 5th due to ice in channel (Site was taken down Oct 14th)

plot(x = vaul.stream.2020$DateTime, y = vaul.stream.2020$WaterLevel) # plot check

### POKE ### 
poke.stream.one.2020 <- poke.stream.one.2020 %>% subset(poke.stream.one.2020$DateTime < "2020-10-10") # Cleaning data that is before October 10th due to ice (Site was taken down Oct 14th)

plot(x = poke.stream.one.2020$DateTime, y = poke.stream.one.2020$WaterLevel) # plot check

poke.stream.two.2020 <- poke.stream.two.2020 %>% subset(poke.stream.two.2020$DateTime < "2020-10-10") # Cleaning data that is before October 10th due to ice (Site was taken down Oct 14th)

plot(x = poke.stream.two.2020$DateTime, y = poke.stream.two.2020$WaterLevel) # plot check

# Combine the two PT into one #
moos.stream.two.2020 <- moos.stream.two.2020[1:nrow(moos.stream.one.2020),]
moos.stream.one.2020$Site <- "MOOS1" #add column identifier
moos.stream.two.2020$Site <- "MOOS2"
moos.pt.2020 <- bind_rows(moos.stream.one.2020, moos.stream.two.2020)

frch.stream.two.2020 <- frch.stream.two.2020[1:nrow(frch.stream.one.2020),]
frch.stream.one.2020$Site <- "FRCH1" #add column identifier
frch.stream.two.2020$Site <- "FRCH2"
frch.pt.2020 <- bind_rows(frch.stream.one.2020, frch.stream.two.2020)

strt.stream.two.2020 <- strt.stream.two.2020[1:nrow(strt.stream.one.2020),]
strt.stream.one.2020$Site <- "STRT1" #add column identifier
strt.stream.two.2020$Site <- "STRT2"
strt.pt.2020 <- bind_rows(strt.stream.one.2020, strt.stream.two.2020)

vaul.stream.2020$Site <- "VAUL" #add column identifier
vaul.pt.2020 <- vaul.stream.2020
names(vaul.pt.2020) <- c("Site", "DateTimeGMT", "AbsolutePressure", "MeanWL", "DateTime")

poke.stream.two.2020 <- poke.stream.two.2020[1:nrow(poke.stream.one.2020),]
poke.stream.one.2020$Site <- "POKE1" #add column identifier
poke.stream.two.2020$Site <- "POKE2"
poke.pt.2020 <- bind_rows(poke.stream.one.2020, poke.stream.two.2020)

# Checking closeness between two PT #
plot(x = moos.stream.one.2020$WaterLevel, y = moos.stream.two.2020$WaterLevel, main = "Moose PT comparison",
     xlab = "Moose1PT", 
     ylab = "Moose2PT")
abline(1,1)


plot(x = frch.stream.one.2020$WaterLevel, y = frch.stream.two.2020$WaterLevel, main = "French PT comparison",
     xlab = "French1 PT", 
     ylab = "French2 PT")
abline(1,1)

plot(x = strt.stream.one.2020$WaterLevel, y = strt.stream.two.2020$WaterLevel, main = "Stuart PT comparison",
     xlab = "Stuart1 PT", 
     ylab = "Stuart2 PT")
abline(1,1)

plot(x = poke.stream.one.2020$WaterLevel, y = poke.stream.two.2020$WaterLevel, main = "Poker PT comparison",
     xlab = "Poker1 PT", 
     ylab = "Poker2 PT")
abline(1,1)

# Moos dirty #
ggplot(moos.pt.2020) + 
  geom_line(aes(x = DateTime , y= WaterLevel, color = Site), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("Moose PT comparison") + 
  scale_color_brewer(palette = "Paired")

## MOOS FINAL ##
moos.pt.2020[6031,4] # Last measurement before cleaning on 8/17 @ 04:45
moos.pt.2020[6039,4] # First measurement after cleaning on 8/17 @7:00
moos.pt.2020[6031,4] - moos.pt.2020[6039,4] #0.116

moos.stream.one.before <- moos.stream.one.2020[-c(6032:11578), ] # clipping off after cleaning
moos.stream.one.after <- moos.stream.one.2020[-c(1:6031),] # clipping off before cleaning
moos.stream.one.after$difference <- moos.stream.one.after[, 4] + 0.116
names(moos.stream.one.after) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WLB4", "DateTime", "WaterLevel")
moos.stream.one.2020.final <- full_join(moos.stream.one.before, moos.stream.one.after)

plot(moos.stream.two.2020$WaterLevel ~ moos.stream.two.2020$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-08-17 04:45:00","2020-08-17 06:45:00"), tz="America/Anchorage"))
moos.stream.two.2020[6030, 4] # Last measurement before cleaning
moos.stream.two.2020[6038, 4] # first measurement after cleaning
moos.stream.two.2020[6030, 4] - moos.stream.two.2020[6038, 4] #0.19 is the difference 

moos.stream.two.before <- moos.stream.two.2020[-c(6031:11578), ]
moos.stream.two.after <- moos.stream.two.2020[-c(1:6030), ]
moos.stream.two.after$difference <- moos.stream.two.after[, 4] + 0.19
names(moos.stream.two.after) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WLB4", "DateTime", "WaterLevel")
moos.stream.two.2020.final <- full_join(moos.stream.two.before, moos.stream.two.after)

moos.pt.2020 <- full_join(moos.stream.one.2020.final, moos.stream.two.2020.final)
moos.pt.2020 <- full_join(moos.stream.one.2020.final, moos.stream.two.2020.final, by = c("DateTime"))
moos.pt.2020 <- moos.pt.2020[, -c(2,3,6,8,9,11)] # cleaning unnecessary columns
names(moos.pt.2020) <- c("Site", "WL1", "DateTime", "Site2", "WL2") #rename
moos.pt.2020$MeanWL <- rowMeans(moos.pt.2020[,c(2, 5)], na.rm = TRUE) # mean of both WL

moos.pt.2020 <- moos.pt.2020 %>% subset(moos.pt.2020$DateTime < "2020-10-13") # Cleaning data that is before October 13th due to removal 

moos.pt.2020 <- moos.pt.2020[-c(5999:6100), ] # clipping off after cleaning

# Final STRT #
strt.pt.2020 <- full_join(strt.stream.one.2020, strt.stream.two.2020, by = c("DateTime"))
strt.pt.2020$MeanWL <- rowMeans(strt.pt.2020[,c(4, 9)], na.rm = TRUE) # mean of both WL

# Final FRCH #
frch.pt.2020 <- full_join(frch.stream.one.2020, frch.stream.two.2020)

frch.pt.2020 <- full_join(frch.stream.one.2020, frch.stream.two.2020, by = c("DateTime"))
frch.pt.2020 <- frch.pt.2020[, -c(2, 3, 7, 8)]
names(frch.pt.2020) <- c("Site", "WL1", "DateTime", "Site2", "WL2") #rename
frch.pt.2020$MeanWL <- rowMeans(frch.pt.2020[,c(2, 5)], na.rm = TRUE) # mean of both WL

# Final POKE #
poke.pt.2020 <- full_join(poke.stream.one.2020, poke.stream.two.2020, by = c("DateTime"))
poke.pt.2020$MeanWL <- rowMeans(poke.pt.2020[,c(4, 9)], na.rm = TRUE) # mean of both WL

poke.pt.2020[9799, 10] - poke.pt.2020[9800, 10] #0.058 is the difference 

poke.pt.before <- poke.pt.2020[-c(9800:12268), ] # before
poke.pt.after <- poke.pt.2020[-c(1:9798), ] # after
poke.pt.after$MeanWL <- poke.pt.after[, 10] + 0.058
#names(moos.stream.two.after) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WLB4", "DateTime", "WaterLevel")
poke.pt.2020 <- full_join(poke.pt.before, poke.pt.after)

poke.pt.2020$MeanWL[poke.pt.2020$DateTime == "2020-09-14 06:30"] <- NA


plot(poke.pt.2020$MeanWL ~ poke.pt.2020$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-09-14 06:15:00","2020-09-14 6:45:00"), tz="America/Anchorage"))

# Moos Clean #
ggplot(moos.pt.2020) + 
  geom_line(aes(x = DateTime , y= MeanWL), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("Moose PT comparison") + 
  scale_color_brewer(palette = "Paired")


ggplot(frch.pt.2020) + 
  geom_line(aes(x = DateTime , y= MeanWL), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("French Water Level") + 
  scale_color_brewer(palette = "Paired")
 

ggplot(strt.pt.2020) + 
  geom_line(aes(x = DateTime , y= MeanWL), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("Stuart PT comparison") + 
  scale_color_brewer(palette = "Paired")
 

ggplot(vaul.pt.2020) + 
  geom_line(aes(x = DateTime , y= MeanWL), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("Vault PT comparison") + 
  scale_color_brewer(palette = "Paired")


ggplot(poke.pt.2020) + 
  geom_line(aes(x = DateTime , y= MeanWL), size=1.25) +
  xlab("Date") +
  ylab("Water Level") +
  theme_classic() +
  ggtitle("Poker average WL") + 
  scale_color_brewer(palette = "Paired")
 

setwd(here())
# check: should be at DoD_Discharge
getwd()

# remove unnecessary columns to merge #
moos.pt.2020 <- moos.pt.2020[,-c(2,4,5)]
frch.pt.2020 <- frch.pt.2020[,-c(2,4,5,6)]
strt.pt.2020 <- strt.pt.2020[,-c(2,3,4,6,7,8,9)]
names(strt.pt.2020) <- c("Site", "DateTime", "MeanWL")
vaul.pt.2020 <- vaul.pt.2020[,-c(2,3)]
poke.pt.2020 <- poke.pt.2020[,-c(2,3,4,6,7,8,9)]
names(poke.pt.2020) <- c("Site", "DateTime", "MeanWL")
all.pt.2020 <- rbind(moos.pt.2020, frch.pt.2020, strt.pt.2020, vaul.pt.2020, poke.pt.2020)
### Write CSV ###
write.csv(moos.pt.2020,"~/Documents/DoD_Discharge/PT_data/2020/MOOS/moos.pt.2020.csv", row.names = FALSE)
write.csv(frch.pt.2020,"~/Documents/DoD_Discharge/PT_data/2020/FRCH/frch.pt.2020.csv", row.names = FALSE)
write.csv(strt.pt.2020,"~/Documents/DoD_Discharge/PT_data/2020/STRT/strt.pt.2020.csv", row.names = FALSE)
write.csv(vaul.pt.2020,"~/Documents/DoD_Discharge/PT_data/2020/VAUL/vaul.pt.2020.csv", row.names = FALSE)
write.csv(poke.pt.2020,"~/Documents/DoD_Discharge/PT_data/2020/POKE/poke.pt.2020.csv", row.names = FALSE)
write.csv(all.pt.2020,"~/Documents/DoD_Discharge/PT_data/2020/all.pt.2020.csv", row.names = FALSE)






############################################### 2021 ################################################
### Import Raw Data ### 
# VAUL only has one because data for PT1 was very erroneous 
poke.stream.2021.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRuPTbgwOrFhhVETrN4HMpVoHrNVwLSecr0acVH7i8ePtxme0PxX1tR_SQ7Mqlg3iiCOHUFw80NFfA5/pub?output=csv"
poke.stream.2021.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTgmuOVxwxSfNzLsb76OmrUQhYzl6prnjP17ubO4XV7x0T0bMpUX7jX5itel6oPe3HDCORnoYD25IgU/pub?output=csv"
strt.stream.2021.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTBHVco3KO6uDX5ixIteIKLgnLUTe1GIGYK-8WBM2eXn1VWvthOjFIGvmXyVOq3l2vnxiBQQaDzbqE1/pub?output=csv"
strt.stream.2021.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTqHls7RlLhnawrL43INl8xLeRLigkYcLhNaUtpHBCN91YmE0rCpNJqBiwvJKp9d0rDapG_UGid43fC/pub?output=csv"
vaul.stream.2021.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSUpdW2ARqdQmnNzpRIbIyGD24DhBSwL5CHFzAG8bwhOsttnyU2nehzfJ0gG8BZHX2VbSc3W1NikCIH/pub?output=csv"
frch.stream.2021.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQi6_PAev36hNhtcXdBBQk3pyJqBoQEKpV8tSvtZgz_DPdqXSg93-d_FDomNSH_lkNhb7fJJVloxl1g/pub?output=csv"
frch.stream.2021.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0NaSrjYnUkQC42v448LFY0EZEr98R6a2gH0FpPlMBwpfEDY80rSzbDOP3OfpB-SI4QQBCOMgoQxd2/pub?output=csv"
moos.stream.2021.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSVsPDDkXNKBU9Ux2qnvtWl-HS0hgXM2cww9_1l2Xz0Vc9C1_KA2Ss56FuS1fq8mESdgqq2Pl5Nvw6o/pub?output=csv"
moos.stream.2021.url.two <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRCnhugeQ8EmP9P22kirbLQhPgDwFoPyMmZ4SR05jSHgleJBcUQYjNb3K2w6hGtdu4W-XJixdl8fk6-/pub?output=csv"

# Load Data#
poke.stream.one.2021 <- read.csv(url(poke.stream.2021.url), skip = 1) 
poke.stream.two.2021 <- read.csv(url(poke.stream.2021.url.two), skip = 1)
strt.stream.one.2021 <- read.csv(url(strt.stream.2021.url), skip = 1) # Deployed the 7th of May 
strt.stream.two.2021 <- read.csv(url(strt.stream.2021.url.two), skip = 1) # Deployed the 19th of May
vaul.stream.one.2021 <- read.csv(url(vaul.stream.2021.url), skip = 1)
frch.stream.one.2021 <- read.csv(url(frch.stream.2021.url), skip = 1) # Deployed the 7th of May 
frch.stream.two.2021 <- read.csv(url(frch.stream.2021.url.two), skip = 1) # Deployed the 19th of May
moos.stream.one.2021 <- read.csv(url(moos.stream.2021.url), skip = 1) # Deployed the 7th of May 
moos.stream.two.2021 <- read.csv(url(moos.stream.2021.url.two), skip = 1) # Deployed the 19th of May

# Erase columns that are unneeded
poke.stream.one.2021 <- poke.stream.one.2021[,-c(4,5)] # Dont need temperature and barometric pressure 
poke.stream.two.2021 <- poke.stream.two.2021[,-c(4,5)] # Dont need temperature and barometric pressure 

strt.stream.one.2021 <- strt.stream.one.2021[,-c(4,5)] # Dont need temperature and barometric pressure 
strt.stream.two.2021 <- strt.stream.two.2021[,-c(4,5)] # Dont need temperature and barometric pressure 

vaul.stream.one.2021 <- vaul.stream.one.2021[,-c(4,5)] # Dont need temperature and barometric pressure 

frch.stream.one.2021 <- frch.stream.one.2021[,-c(4,5)] # Dont need temperature and barometric pressure 
frch.stream.two.2021 <- frch.stream.two.2021[,-c(4,5)] # Dont need temperature and barometric pressure 

moos.stream.one.2021 <- moos.stream.one.2021[,-c(4,5)] # Dont need temperature and barometric pressure 
moos.stream.two.2021 <- moos.stream.two.2021[,-c(4,5)] # Dont need temperature and barometric pressure 

# Rename columns 
names(poke.stream.one.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")
names(poke.stream.two.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

names(strt.stream.one.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")
names(strt.stream.two.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

names(vaul.stream.one.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

names(frch.stream.one.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")
names(frch.stream.two.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

names(moos.stream.one.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")
names(moos.stream.two.2021) <- c("Site", "DateTimeGMT", "AbsolutePressure", "WaterLevel")

# Input NAs for time 
poke.stream.one.2021$DateTimeGMT[poke.stream.one.2021$DateTimeGMT == ""] <- NA
poke.stream.two.2021$DateTimeGMT[poke.stream.two.2021$DateTimeGMT == ""] <- NA

strt.stream.one.2021$DateTimeGMT[strt.stream.one.2021$DateTimeGMT == ""] <- NA
strt.stream.two.2021$DateTimeGMT[strt.stream.two.2021$DateTimeGMT == ""] <- NA

vaul.stream.one.2021$DateTimeGMT[vaul.stream.one.2021$DateTimeGMT == ""] <- NA

frch.stream.one.2021$DateTimeGMT[frch.stream.one.2021$DateTimeGMT == ""] <- NA
frch.stream.two.2021$DateTimeGMT[frch.stream.two.2021$DateTimeGMT == ""] <- NA

moos.stream.one.2021$DateTimeGMT[moos.stream.one.2021$DateTimeGMT == ""] <- NA
moos.stream.two.2021$DateTimeGMT[moos.stream.two.2021$DateTimeGMT == ""] <- NA

# Convert to AK time 
poke.stream.one.2021$DateTime <- mdy_hms(poke.stream.one.2021$DateTimeGMT, tz = "GMT")
attributes(poke.stream.one.2021$DateTime)$tzone <- 'America/Anchorage'
poke.stream.two.2021$DateTime <- mdy_hms(poke.stream.two.2021$DateTimeGMT, tz = "GMT")
attributes(poke.stream.two.2021$DateTime)$tzone <- 'America/Anchorage'

strt.stream.one.2021$DateTime <- mdy_hms(strt.stream.one.2021$DateTimeGMT, tz = "GMT")
attributes(strt.stream.one.2021$DateTime)$tzone <- 'America/Anchorage'
strt.stream.two.2021$DateTime <- mdy_hms(strt.stream.two.2021$DateTimeGMT, tz = "GMT")
attributes(strt.stream.two.2021$DateTime)$tzone <- 'America/Anchorage'

vaul.stream.one.2021$DateTime <- mdy_hms(vaul.stream.one.2021$DateTimeGMT, tz = "GMT")
attributes(vaul.stream.one.2021$DateTime)$tzone <- 'America/Anchorage'

frch.stream.one.2021$DateTime <- mdy_hms(frch.stream.one.2021$DateTimeGMT, tz = "GMT")
attributes(frch.stream.one.2021$DateTime)$tzone <- 'America/Anchorage'
frch.stream.two.2021$DateTime <- mdy_hms(frch.stream.two.2021$DateTimeGMT, tz = "GMT")
attributes(frch.stream.two.2021$DateTime)$tzone <- 'America/Anchorage'

moos.stream.one.2021$DateTime <- mdy_hms(moos.stream.one.2021$DateTimeGMT, tz = "GMT")
attributes(moos.stream.one.2021$DateTime)$tzone <- 'America/Anchorage'
moos.stream.two.2021$DateTime <- mdy_hms(moos.stream.two.2021$DateTimeGMT, tz = "GMT")
attributes(moos.stream.two.2021$DateTime)$tzone <- 'America/Anchorage'


# filter out take out of water points #
# Poke #
poke.stream.one.2021 <- poke.stream.one.2021 %>% subset(poke.stream.one.2021$DateTime > "2021-05-12" & poke.stream.one.2021$DateTime < "2021-09-29") # Deployed on the 6th of may and removed on the 29th of september 
poke.stream.two.2021 <- poke.stream.two.2021 %>% subset(poke.stream.two.2021$DateTime > "2021-05-06" & poke.stream.two.2021$DateTime < "2021-09-29") # Deployed on the 6th of may and removed on the 29th of september 

ggplot(poke.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel))# plot check 
poke.one.below.216 <- poke.stream.one.2021.2[which(poke.stream.one.2021.2$WaterLevel < "216"), ] # trying to find the erroneous point in the dataframe 

poke.stream.one.2021$DateTime[poke.stream.one.2021$DateTime == "2021-08-10 04:50:00"] <- NA
poke.stream.one.2021$DateTime[poke.stream.one.2021$DateTime == "2021-06-29"] <- NA

ggplot(poke.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) #plot check  # clean for now but definitely have to make a change due to beaver dams

ggplot(poke.stream.two.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel))# plot check

poke.two.below.216 <- poke.stream.two.2021[which(poke.stream.two.2021$WaterLevel < "216"), ] # trying to find the erroneous point in the dataframe 


poke.stream.two.2021$DateTime[poke.stream.two.2021$DateTime == "2021-06-14 04:00:00"] <- NA


ggplot(poke.stream.two.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel))# plot check 

# STRT #
ggplot(strt.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

strt.one.above.251.5 <- strt.stream.one.2021[which(strt.stream.one.2021$WaterLevel > "251.5"), ] # trying to find the erroneous point in the dataframe 
strt.stream.one.2021 <- strt.stream.one.2021[-c(910:923) , ] # removing erroneous points on 5/13 from 20:15-23:30

strt.one.below.250.25 <- strt.stream.one.2021[which(strt.stream.one.2021$WaterLevel < "250.25"), ] # trying to find the erroneous point in the dataframe 
strt.stream.one.2021 <- strt.stream.one.2021[-c(17340:17350) , ] # removing erroneous points on 5/13 from 20:15-23:30

ggplot(strt.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

ggplot(strt.stream.two.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

# Vaul #
ggplot(vaul.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

vaul.one.below.197.75 <- vaul.stream.one.2021[which(vaul.stream.one.2021$WaterLevel < "197.75"), ] # trying to find the erroneous point in the dataframe 
vaul.stream.one.2021 <- vaul.stream.one.2021[-c(4777:4793, 16861, 30756:30757) , ] # removing erroneous points 

ggplot(vaul.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

# FRCH #
ggplot(frch.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

frch.stream.one.2021 <- frch.stream.one.2021 %>% subset(frch.stream.one.2021$DateTime < "2021-09-27") #  removed on the 28th of september and it appears that there was some ice influence on the 28th 

ggplot(frch.stream.two.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check
frch.two.below.183.6 <- frch.stream.two.2021[which(frch.stream.two.2021$WaterLevel < "183.6"), ]
frch.stream.two.2021 <- frch.stream.two.2021[-c(12948, 17535,31360,31361) , ] # removing erroneous points 
frch.stream.two.2021 <- frch.stream.two.2021 %>% subset(frch.stream.two.2021$DateTime < "2021-09-27") #  removed on the 28th of september and it appears that there was some ice influence on the 28th 

ggplot(frch.stream.two.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

# MOOS #
ggplot(moos.stream.one.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check

moos.stream.one.2021 <- moos.stream.one.2021 %>% subset(moos.stream.one.2021$DateTime < "2021-09-27") #  removed on the 28th of september and it appears that there was some ice influence on the 28th 

ggplot(moos.stream.two.2021) +
  geom_point(aes(x = DateTime, y = WaterLevel)) # plot check


# Combine two PTs into one 
poke.stream.two.2021 <- poke.stream.two.2021[1:nrow(poke.stream.one.2021),]
poke.stream.one.2021$Site <- "POKE1" #add column identifier
poke.stream.two.2021$Site <- "POKE2"
poke.pt.2021 <- bind_rows(poke.stream.one.2021, poke.stream.two.2021)

strt.stream.two.2021 <- strt.stream.two.2021[1:nrow(strt.stream.one.2021),]
strt.stream.one.2021$Site <- "STRT1" #add column identifier
strt.stream.two.2021$Site <- "STRT2"
strt.pt.2021 <- bind_rows(strt.stream.one.2021, strt.stream.two.2021)

frch.stream.two.2021 <- frch.stream.two.2021[1:nrow(frch.stream.one.2021),]
frch.stream.one.2021$Site <- "FRCH1" #add column identifier
frch.stream.two.2021$Site <- "FRCH2"
frch.pt.2021 <- bind_rows(frch.stream.one.2021, frch.stream.two.2021)

moos.stream.two.2021 <- moos.stream.two.2021[1:nrow(moos.stream.one.2021),]
moos.stream.one.2021$Site <- "MOOS1" #add column identifier
moos.stream.two.2021$Site <- "MOOS2"
moos.pt.2021 <- bind_rows(moos.stream.one.2021, moos.stream.two.2021)

# Checking closeness between two PT #
plot(x = poke.stream.one.2021$WaterLevel, y = poke.stream.two.2021$WaterLevel, main = "Poker PT comparison",
     xlab = "Poker1PT", 
     ylab = "Poker2PT")
abline(1,1)

plot(x = strt.stream.one.2021$WaterLevel, y = strt.stream.two.2021$WaterLevel, main = "Stuart PT comparison",
     xlab = "Stuart1PT", 
     ylab = "Stuart2PT")
abline(1,1)

plot(x = frch.stream.one.2021$WaterLevel, y = frch.stream.two.2021$WaterLevel, main = "French PT comparison",
     xlab = "French1PT", 
     ylab = "French2PT")
abline(1,1)

plot(x = moos.stream.one.2021$WaterLevel, y = moos.stream.two.2021$WaterLevel, main = "Moose PT comparison",
     xlab = "Moose1PT", 
     ylab = "Moose2PT")
abline(1,1)





