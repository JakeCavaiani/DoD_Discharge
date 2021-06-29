#### READ ME ####
# The purpose of this script is to prepare DoD data for hysteresis analysis and, specifically, the hysteresisMetrics function.
# Step 1: Load in processed SUNA and EXO data from DoD_2020 script within DoD->AK Sensors->2020-> SUNA->Processed & DoD->AK Sensors->2020-> EXO->Processed 
# Step 2: fill gaps in nitrate, fDOM, SpCond, and turbidity data
# Step 3: Define baseflow in each catchment.
# Step 4: Set criteria for storm delineation for each catchment based on some percentage over baseflow.
# Step 5: Delineate storms in each catchment.
# Step 6: IN PYTHON: convert R discharge df to pandas df containing a datetime column named 'valuedatetime', and discharge values in a column 'datavalue'
# Step 7: IN PYTHON: convert R response df(s) to pandas df(s) containing a datetime column named 'valuedatetime', and response values in a column 'datavalue'


library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(imputeTS)
library(TSA)
library(bbmle)
library(zoo)
library(xts)
library(forecast)
library(stats)
library(lattice)
library(nlme)
library(geosphere)
library(car)
library(EcoHydRology)

#### Load from google drive ####
#suna.2020.url <- "https://drive.google.com/drive/u/1/folders/1QGwlgWHBQI_AeLuMGNzD4qWB90UqlykK"

#SUNA_new <- drive_get(as_id(suna.2020.url))
#EXO_new <- drive_get(as_id(exo.2020.url))

#SUNA_glist <- drive_ls(SUNA_new, type = "csv")
#EXO_glist <- drive_ls(EXO_new, type = "csv")

#SUNAfile_list <- list.files(path = "")

#SUNA.ALL <- lapply(SUNA_glist)
#read.csv(SUNA_glist)

### Load from local machine ###
EXO_ALL <- read_csv("~/Documents/DoD_2020/EXO_data/from_internal_harddrive/processed/EXO.ALL.csv")

EXO_ALL$Site <- EXO_ALL$site.ID
EXO_ALL$DateTime <- as.POSIXct(EXO_ALL$DateTime)

ALL <- full_join(EXO_ALL, final_discharge_2020)

SUNA_ALL <- read_csv("~/Documents/DoD_2020/SUNA_data/from_internal_harddrive/processed/SUNA.processed.csv")
SUNA_ALL <- SUNA_ALL[, -(14:269)] # Remove channels
SUNA_ALL$Site <- SUNA_ALL$site.ID
SUNA_ALL$DateTime <- SUNA_ALL$datetimeAK

ALL <- full_join(ALL, SUNA_ALL)


#### subset data by site ####
FRCH <-  subset(ALL, Site == "FRCH")
head(FRCH$DateTime)
STRT = subset(ALL, Site == "STRT")
head(STRT$DateTime)
POKE = subset(ALL, Site == "POKE")
head(POKE$DateTime)
VAUL = subset(ALL, Site == "VAUL")
head(VAUL$DateTime)
MOOS = subset(ALL, Site == "MOOS")
head(MOOS$DateTime)

FRCH_Q = as.data.frame(Q.daily$FRCH)
FRCH_Q$day = as.Date(rownames(Q.daily))
names(FRCH_Q) = c("Discharge_Lsec", "day")

STRT_Q = as.data.frame(Q.daily$STRT)
STRT_Q$day = as.Date(rownames(Q.daily))
names(STRT_Q) = c("Discharge_Lsec", "day")

POKE_Q = as.data.frame(Q.daily$POKE)
POKE_Q$day = as.Date(rownames(Q.daily))
names(POKE_Q) = c("Discharge_Lsec", "day")

VAUL_Q = as.data.frame(Q.daily$VAUL)
VAUL_Q$day = as.Date(rownames(Q.daily))
names(VAUL_Q) = c("Discharge_Lsec", "day")

MOOS_Q = as.data.frame(Q.daily$MOOS)
MOOS_Q$day = as.Date(rownames(Q.daily))
names(MOOS_Q) = c("Discharge_Lsec", "day")


#### data wrangling - fill gaps ####
# fxn #
fillgaps15 = function(df, dat, datquotes, largegap.num){
  ## Document gaps >= largegap.num (1 largegap.num = 15 min) ##
  # (note - the criteria of what constitutes a "large" gap should be reevaluated depending on the trend being characterized)
  is.na.rle <- rle(is.na(dat))
  is.na.rle$values <- is.na.rle$values & is.na.rle$lengths >= (largegap.num)
  biggaps = df[inverse.rle(is.na.rle), ]
  tz(biggaps$DateTime) = "America/Anchorage"
  biggaps = subset(biggaps, select = "DateTime")
  # Make univariate time series, covert to zoo, then to ts #
  ts.xts = subset(df, select = c("DateTime",datquotes))
  ts.xts<-read.zoo(ts.xts, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
  ts.xts<-as.xts(ts.xts)
  # remove leading and trailing NAs #
  ts.xts = na.trim(ts.xts, is.na="any")
  # Apply auto.arima and kalman filter to impute missing values #
  fit2 = auto.arima(ts.xts) 
  kal = KalmanSmooth(ts.xts, fit2$model)
  id.na<-which(is.na(ts.xts))
  for(i in id.na) 
    ts.xts[i]<-fit2$model$Z %*% kal$smooth[i,]
  # revert to dataframe #
  ts.df = as.data.frame((ts.xts))
  ts.df$date_timeAK = as.POSIXct(row.names(ts.df), tz="America/Anchorage")
  names(ts.df) = c("dat_filled", "DateTime")
  # remove large gaps # 
  ts.df$dat_filled[ts.df$date_timeAK %in% as.POSIXct(biggaps$DateTime)] = NA
  # Replace large gaps with linear interpolation #
  ts.df$dat_filled = na.interpolation(ts.df$dat_filled)
  ts.df = subset(ts.df, select = c("dat_filled", "DateTime"))
  return(ts.df)
}


# FRCH # 
FRCH.no3 <- subset(FRCH, select = c("datetimeAK","nitrateuM"))
FRCH.no3 <-  fillgaps15(FRCH.no3, FRCH.no3$nitrateuM, "nitrateuM", 16)
plot(FRCH$nitrateuM ~ FRCH$DateTime)
names(FRCH.no3) = c("DateTime", "nitrate_uM_filled")

FRCH.fDOM = subset(FRCH, select = c("DateTime","fDOM.RFU"))
FRCH.fDOM = fillgaps15(FRCH.fDOM, FRCH.fDOM$fDOM.RFU, "fDOM.RFU", 16)
ggplot(FRCH) +
  geom_point(mapping = aes(x = Date, y = fDOM.RFU)) +
  theme_classic()
names(FRCH.fDOM) = c("fDOM_filled", "DateTime")

FRCH.SpCond = subset(FRCH, select = c("date_timeAK","SpCond_uScm"))
FRCH.SpCond = fillgaps15(FRCH.SpCond, FRCH.SpCond$SpCond_uScm, "SpCond_uScm", 16)
plot(FRCH.SpCond$dat_filled, type="l")
plot(FRCH$SpCond.uScm ~ FRCH$Date)
names(FRCH.SpCond) = c("SpCond_uScm_filled", "date_timeAK")

FRCH.turb = subset(FRCH, select = c("date_timeAK","Turbidity_FNU"))
FRCH.turb = fillgaps15(FRCH.turb, FRCH.turb$Turbidity_FNU, "Turbidity_FNU", 16)
plot(FRCH.turb$dat_filled, type="l")
names(FRCH.turb) = c("Turbidity_FNU_filled", "date_timeAK")

FRCH = left_join(FRCH, FRCH.no3, by="date_timeAK")
FRCH = left_join(FRCH, FRCH.fDOM, by="date_timeAK")
FRCH = left_join(FRCH, FRCH.SpCond, by="date_timeAK")
FRCH = left_join(FRCH, FRCH.turb, by="date_timeAK")
#
### STRT ###
STRT.no3 <- subset(STRT, select = c("DateTime", "nitrateuM"))
STRT.no3 = fillgaps15(STRT.no3, STRT.no3$nitrateuM, "nitrateuM", 16)
plot(STRT.no3$dat_filled, type="l")
names(STRT.no3) = c("nitrate_uM_filled", "DateTime")

STRT.fDOM = subset(STRT, select = c("DateTime","fDOM.RFU"))
STRT.fDOM = fillgaps15(STRT.fDOM, STRT.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(STRT$dat_filled, type="l")
names(STRT.fDOM) = c("fDOM_filled", "DateTime")

### POKE ### 
POKE.no3 <- subset(POKE, select = c("DateTime", "nitrateuM"))
POKE.no3 <- POKE.no3[!is.na(POKE.no3$DateTime), ]
POKE.no3 <-  fillgaps15(POKE.no3, POKE.no3$nitrateuM, "nitrateuM", 16)
plot(POKE.no3$dat_filled, type="l")
names(POKE.no3) = c("nitrate_uM_filled", "DateTime")

POKE.fDOM = subset(POKE, select = c("DateTime","fDOM.RFU"))
POKE.fDOM = fillgaps15(POKE.fDOM, POKE.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(POKE$dat_filled, type="l")
names(POKE.fDOM) = c("fDOM_filled", "DateTime")

### VAUL ### 
VAUL.no3 <- subset(VAUL, select = c("DateTime", "nitrateuM"))
VAUL.no3 <-  fillgaps15(VAUL.no3, VAUL.no3$nitrateuM, "nitrateuM", 16)
plot(VAUL.no3$dat_filled, type="l")
names(POKE.no3) = c("nitrate_uM_filled", "DateTime")

VAUL.fDOM = subset(VAUL, select = c("DateTime","fDOM.RFU"))
VAUL.fDOM = fillgaps15(VAUL.fDOM, VAUL.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(VAUL$dat_filled, type="l")
names(VAUL.fDOM) = c("fDOM_filled", "DateTime")

### MOOS ###
MOOS.no3 <- subset(MOOS, select = c("DateTime", "nitrateuM"))
MOOS.no3 <-  fillgaps15(MOOS.no3, MOOS.no3$nitrateuM, "nitrateuM", 16)
plot(MOOS.no3$dat_filled, type="l")
names(MOOS.no3) = c("nitrate_uM_filled", "DateTime")

MOOS.fDOM = subset(MOOS, select = c("DateTime","fDOM.RFU"))
MOOS.fDOM = fillgaps15(MOOS.fDOM, MOOS.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(MOOS$dat_filled, type="l")
names(MOOS.fDOM) = c("fDOM_filled", "DateTime")

#### Baseflow Separation ####

any(is.na(FRCH_Q$day))
any(is.na(FRCH_Q$Discharge_Lsec))

any(is.na(STRT_Q$day))
any(is.na(STRT_Q$Discharge_Lsec))

any(is.na(POKE_Q$day))
any(is.na(POKE_Q$Discharge_Lsec))

any(is.na(VAUL_Q$day))
any(is.na(VAUL_Q$Discharge_Lsec))

any(is.na(MOOS_Q$day))
any(is.na(MOOS_Q$Discharge_Lsec))


#C3_Q_bf = bf_eckhardt_filter(date = class(as.Date.POSIXct(C3_Q$day)), 
# discharge = C3_Q$Discharge_Lsec_mean, 
# BFImax = 0.9, alpha = .90)
#hydrograph(input=subset(C3_Q, select = c(day, Discharge_Lsec_mean)), streamflow2=C3_Q_bf$baseflow)

#C3_Q = bf_single_term_filter(date = C3_Q$date_timeAK, discharge = C3$Discharge_Lsec, alpha = .925)
#hydrograph(input=subset(C3_Q, select = c(date_timeAK, Discharge_Lsec)), streamflow2=C3_Q$baseflow)

plot(C3$Discharge_Lsec ~ C3$date_timeAK, type="l", xlab="", ylab="Q (L/sec)",
     # xlim = as.POSIXct(c("2017-05-22 00:00:00","2017-09-01 00:00:00"), tz="America/Anchorage"),
     ylim = c(0,180), col="blue")

### examine the recursive digital filter at .9, .925, .95 levels ###
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type = "l", xlab = "", ylab = "Q (L/sec)",
    xlim =  as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
    ylim = c(0, 3000), col="blue")
#
plot(poke.final.discharge$MeanDischarge ~ poke.final.discharge$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(0,3000), col="blue")

plot(vaul.final.discharge$MeanDischarge ~ vaul.final.discharge$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(0,3000), col="blue")

plot(strt.final.discharge$MeanDischarge ~ strt.final.discharge$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(0,3000), col="blue")

plot(moos.final.discharge$MeanDischarge ~ moos.final.discharge$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(0,3000), col="blue")

### Hydrograph Separation ###

install.packages("dpr")
library(dplyr)
#
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(FRCH_Q, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 

#
strt.final.discharge <- strt.final.discharge[-c(10815:10817), ] # clean Nas.


STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(STRT_Q, select = c(day, Discharge_Lsec)), streamflow2=STRT_Q_bf$bt)
#
POKE_Q_bf = BaseflowSeparation(poke.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(POKE_Q, select = c(day, Discharge_Lsec)), streamflow2=POKE_Q_bf$bt) 
#
VAUL_Q_bf = BaseflowSeparation(vaul.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(VAUL_Q, select = c(day, Discharge_Lsec)), streamflow2=VAUL_Q_bf$bt) 

MOOS_Q_bf = BaseflowSeparation(moos.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(MOOS_Q, select = c(day, Discharge_Lsec)), streamflow2=MOOS_Q_bf$bt) 

###.925 ###
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(FRCH_Q, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 
#
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(STRT_Q, select = c(day, Discharge_Lsec)), streamflow2=STRT_Q_bf$bt)
#
POKE_Q_bf = BaseflowSeparation(poke.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(POKE_Q, select = c(day, Discharge_Lsec)), streamflow2=POKE_Q_bf$bt) 
#
VAUL_Q_bf = BaseflowSeparation(vaul.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(VAUL_Q, select = c(day, Discharge_Lsec)), streamflow2=VAUL_Q_bf$bt) 

MOOS_Q_bf = BaseflowSeparation(moos.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(MOOS_Q, select = c(day, Discharge_Lsec)), streamflow2=MOOS_Q_bf$bt) 


### .95 ###
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(FRCH_Q, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 
#
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(STRT_Q, select = c(day, Discharge_Lsec)), streamflow2=STRT_Q_bf$bt)
#
POKE_Q_bf = BaseflowSeparation(poke.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(POKE_Q, select = c(day, Discharge_Lsec)), streamflow2=POKE_Q_bf$bt) 
#
VAUL_Q_bf = BaseflowSeparation(vaul.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(VAUL_Q, select = c(day, Discharge_Lsec)), streamflow2=VAUL_Q_bf$bt) 

MOOS_Q_bf = BaseflowSeparation(moos.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(MOOS_Q, select = c(day, Discharge_Lsec)), streamflow2=MOOS_Q_bf$bt) 


#### Deliniate storms in FRCH ####

# ID storms: Any events where Q reached 2X mean base flow 
# Pick starting points: manually select inflection pt when Q began to rise
# Pick ending points: manually select pt when Q reached pre-storm baseflow OR when another event occurred


FRCH_bfQ_mn = mean(FRCH_Q_bf$bt)
FRCH_bfQ_mn
FRCH_bfQ_mn*2

STRT_bfQ_mn = mean(STRT_Q_bf$bt)
STRT_bfQ_mn
STRT_bfQ_mn*2

VAUL_bfQ_mn = mean(VAUL_Q_bf$bt)
VAUL_bfQ_mn
VAUL_bfQ_mn*2

POKE_bfQ_mn = mean(POKE_Q_bf$bt)
POKE_bfQ_mn
POKE_bfQ_mn*2

MOOS_bfQ_mn = mean(MOOS_Q_bf$bt)
MOOS_bfQ_mn
MOOS_bfQ_mn*2


plot(FRCH_Q$Discharge_Lsec ~ FRCH_Q$day, type="l", xlab="", ylab="Q (L/sec)",ylim = c(0,3000), col="blue", main="FRCH")
lines(FRCH_Q_bf$bt ~ FRCH_Q$day, col="red")
#lines((C3_Q_bf$bt*1.3) ~ C3_Q$day, col="red", lty=2)
#lines((C3_Q_bf$bt*5) ~ C3_Q$day, col="red", lty=2)
abline(h = FRCH_bfQ_mn*2, col="red", lty=2)

plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
lines(FRCH$nitrateuM * 20 ~ FRCH.no3$datetimeAK, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)

### Import precipitation data into the *ALL document ### 
# FRCH rain gauge installed on the 11th of June. 
plot(FRCH.st$inst_rainfall_mm ~ FRCH.st$datetimeAK, type="h",
     xlim = as.POSIXct(c("2019-06-11 0:00:00","2019-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'FRCH precip. (mm)') 
lines(FRCH$nitrateuM ~ FRCH.no3$datetimeAK, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-06-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)


plot(frch.gauge$Precip ~ frch.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-06-11 0:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(400,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'FRCH precip. (mm)') 
lines(FRCH$nitrateuM ~ FRCH.no3$datetimeAK, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)

# STRT rain gauge installed on the 29th of July #
plot(strt.gauge$Precip ~ strt.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-07-29 0:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(200,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'STRT precip. (mm)') 

# VAUL rain gauge installed on the 22th of June #
plot(vaul.gauge$Precip ~ vaul.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-06-22 0:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(300,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'VAUL precip (mm)') 


### Storm 1 ###
# FRCH #
par(mar=c(3,5,3,5), bty = "n")
plot(FRCH$MeanDischarge ~ as.POSIXct(FRCH$datetimeAK, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(0,2000), col="blue", main="C3",
     xlim = as.POSIXct(c("2020-06-12 01:00:00","2020-06-15 01:00:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)
par(mar=c(3,5,3,5), bty = "n")

plot(frch.gauge$Precip ~ frch.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-06-12 01:00:00","2020-06-15 01:00:00"), tz="America/Anchorage"),
     ylim = c(30,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v= as.POSIXct("2017-07-02 18:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2017-07-07 16:00:00", tz="America/Anchorage"), col="purple")

C3_storm1 = C3[C3$date_timeAK > as.POSIXct("2017-07-02 18:30:00", tz="America/Anchorage") &
                 C3$date_timeAK < as.POSIXct("2017-07-07 16:00:00", tz="America/Anchorage"),]
plot(C3_storm1$Discharge_Lsec ~ as.POSIXct(C3_storm1$date_timeAK, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(0,1000), col="blue", main="C3 Storm 1a",
     xlim = as.POSIXct(c("2017-07-01 01:00:00","2017-07-09 01:00:00"), tz="America/Anchorage"))

# STRT # 
plot(STRT$MeanDischarge ~ as.POSIXct(STRT$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(1000,1500), col="blue", main="STRT",
     xlim = as.POSIXct(c("2020-07-29 01:00:00","2020-08-01 01:00:00"), tz="America/Anchorage"))
abline(h=STRT_bfQ_mn*2, col="red", lty=2)
abline(h=STRT_bfQ_mn, col="red")
par(new = T)
par(mar=c(3,5,3,5), bty = "n")

plot(strt.gauge$Precip ~ strt.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-07-29 01:00:00","2020-08-01 23:00:00"), tz="America/Anchorage"),
     ylim = c(30,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v= as.POSIXct("2020-07-29 01:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2020-08-03 16:00:00", tz="America/Anchorage"), col="purple")


# VAUL #
plot(vaul.final.discharge$MeanDischarge ~ vaul.final.discharge$DateTime, type="l", xlab="", ylab="Q (L/sec)",ylim = c(0,1200), col="blue", main="VAUL",
     xlim = as.POSIXct(c("2020-06-22 01:00:00","2020-06-24 23:00:00"), tz="America/Anchorage"))
abline(h=VAUL_bfQ_mn*2, col="red", lty=2)
abline(h=VAUL_bfQ_mn, col="red")
par(new = T)
par(mar=c(3,5,3,5), bty = "n")

plot(vaul.gauge$Precip ~ vaul.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-06-22 01:00:00","2020-06-24 23:00:00"), tz="America/Anchorage"),
     ylim = c(30,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v= as.POSIXct("2020-06-22 01:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2020-06-24 23:00:00", tz="America/Anchorage"), col="purple")







### POKE ### 
plot(POKE_Q$Discharge_Lsec ~ POKE_Q$day, type="l", xlab="", ylab="Q (L/sec)",ylim = c(0,3000), col="blue", main="POKE")
lines(POKE_Q_bf$bt ~ POKE_Q$day, col="red")
#lines((C3_Q_bf$bt*1.3) ~ C3_Q$day, col="red", lty=2)
#lines((C3_Q_bf$bt*5) ~ C3_Q$day, col="red", lty=2)
abline(h = POKE_bfQ_mn*2, col="red", lty=2)

plot(POKE$MeanDischarge ~ POKE$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"))
abline(h=POKE_bfQ_mn*2, col="red", lty=2)
abline(h=POKE_bfQ_mn, col="red")
lines(POKE$nitrateuM ~ POKE.no3$datetimeAK, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)

### Import precipitation data into the *ALL document ### 
# Poke rain gauge installed on the 11th of June. 
plot(poke.gauge$Precip ~ poke.gauge$DateTime, type="h",
     xlim = as.POSIXct(c("2020-06-11 0:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(400,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'POKE precip. (mm)')










