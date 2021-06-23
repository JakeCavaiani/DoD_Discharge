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
suna.2020.url <- "https://drive.google.com/drive/u/1/folders/1QGwlgWHBQI_AeLuMGNzD4qWB90UqlykK"

SUNA_new <- drive_get(as_id(suna.2020.url))
EXO_new <- drive_get(as_id(exo.2020.url))

SUNA_glist <- drive_ls(SUNA_new, type = "csv")
EXO_glist <- drive_ls(EXO_new, type = "csv")

SUNAfile_list <- list.files(path = "")

SUNA.ALL <- lapply(SUNA_glist)
read.csv(SUNA_glist)

### Load from local machine ###
EXO_ALL <- read_csv("~/Documents/DoD_2020/EXO_data/from_internal_harddrive/processed/EXO.ALL.csv")

EXO_ALL$Site <- EXO_ALL$site.ID
EXO_ALL$DateTime <- as.POSIXct(EXO_ALL$DateTime)

ALL <- full_join(EXO_ALL, final_discharge_2020)

SUNA_ALL <- read_csv("~/Documents/DoD_2020/SUNA_data/from_internal_harddrive/processed/SUNA.processed.csv")
SUNA_ALL <- SUNA_ALL[, -(14:269)] # Remove channels
SUNA_ALL$Site <- SUNA_ALL$site.ID
SUNA_ALL$DateTime1 <- as.Date(SUNA_ALL$day_timeUTC, origin = "2020-01-01")
SUNA_ALL$DateTime <- as.Date(c(SUNA_ALL$date_yearday), origin = structure(-2440588, class = "Date"))

ALL <- full_join(ALL, SUNA_ALL)
head(SUNA_ALL$datetimeAK)

as.Date(julian_date, origin="1970-01-01")

as.Date(c(2458010,2458011), origin = structure(-2440588, class = "Date"))

Q <-  subset(ALL, select = c("DateTime","Site","MeanDischarge"))

Q$day = format(as.POSIXct(Q$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
Q$day = as.POSIXct(Q$day, "%Y-%m-%d", tz="America/Anchorage")
Q$DateTime = NULL

Q.daily = with(Q, tapply(MeanDischarge, list(day, Site), mean))
Q.daily = as.data.frame(Q.daily)


frch.final.discharge$day = format(as.POSIXct(frch.final.discharge$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
frch.final.discharge$day = as.POSIXct(frch.final.discharge$day, "%Y-%m-%d", tz="America/Anchorage")
frch.final.discharge$DateTime = NULL

frch.q.daily <- with(frch.final.discharge, tapply(MeanDischarge, list(day, Site), mean))
frch.q.daily <- as.data.frame(frch.q.daily)

FRCH_Q <-  as.data.frame(frch.q.daily)
FRCH_Q$day = as.Date(rownames(frch.q.daily))
names(FRCH_Q) = c("Discharge_Lsec", "day")

strt.final.discharge$day = format(as.POSIXct(strt.final.discharge$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
strt.final.discharge$day = as.POSIXct(strt.final.discharge$day, "%Y-%m-%d", tz="America/Anchorage")
strt.final.discharge$DateTime = NULL

strt.q.daily <- with(strt.final.discharge, tapply(MeanDischarge, list(day, Site), mean))
strt.q.daily <- as.data.frame(strt.q.daily)


STRT_Q <-  as.data.frame(strt.q.daily)
STRT_Q$day = as.Date(rownames(strt.q.daily))
names(STRT_Q) = c("Discharge_Lsec", "day")

vaul.final.discharge$day = format(as.POSIXct(vaul.final.discharge$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
vaul.final.discharge$day = as.POSIXct(vaul.final.discharge$day, "%Y-%m-%d", tz="America/Anchorage")
vaul.final.discharge$DateTime = NULL

vaul.q.daily <- with(vaul.final.discharge, tapply(MeanDischarge, list(day, Site), mean))
vaul.q.daily <- as.data.frame(vaul.q.daily)

VAUL_Q <-  as.data.frame(vaul.q.daily)
VAUL_Q$day = as.Date(rownames(vaul.q.daily))
names(VAUL_Q) = c("Discharge_Lsec", "day")

poke.final.discharge$day = format(as.POSIXct(poke.final.discharge$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
poke.final.discharge$day = as.POSIXct(poke.final.discharge$day, "%Y-%m-%d", tz="America/Anchorage")
poke.final.discharge$DateTime = NULL

poke.q.daily <- with(poke.final.discharge, tapply(MeanDischarge, list(day, Site), mean))
poke.q.daily <- as.data.frame(poke.q.daily)


POKE_Q <-  as.data.frame(poke.q.daily)
POKE_Q$day = as.Date(rownames(poke.q.daily))
names(POKE_Q) = c("Discharge_Lsec", "day")

moos.final.discharge$day = format(as.POSIXct(moos.final.discharge$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
moos.final.discharge$day = as.POSIXct(moos.final.discharge$day, "%Y-%m-%d", tz="America/Anchorage")
moos.final.discharge$DateTime = NULL

moos.q.daily <- with(moos.final.discharge, tapply(MeanDischarge, list(day, Site), mean))
moos.q.daily <- as.data.frame(moos.q.daily)

MOOS_Q <-  as.data.frame(moos.q.daily)
MOOS_Q$day = as.Date(rownames(moos.q.daily))
names(MOOS_Q) = c("Discharge_Lsec", "day")

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




#### ATTEMPT: baseline correct C2 Q data ####
# There are several places in the C2 Q data where it jumps up within 1 timestep, which isn't realistic for a primarily groundwater fed stream. I have left them alone until now because the jumps are small compared to the range of Q in other sites and I haven't had good criteria by which to correct it, but it is really messing up my hysteresis analysis. I will try correcting it here anywhere that Q jumps up within one time step and shifts the baseline up. 

## correction 1 ##
C2.Q = subset(C2, select = c("date_timeAK","Discharge_Lsec"))
plot(C2.Q$Discharge_Lsec ~ as.POSIXct(C2.Q$date_timeAK, tz="America/Anchorage"), type ="b",
     xlim = as.POSIXct(c("2017-06-27 01:00:00","2017-06-28 01:00:00"), tz="America/Anchorage"))

C2.Q$Discharge_Lsec[C2.Q$date_timeAK == as.POSIXct("2017-06-27 10:45:00")] = 22.9603

C2.Q$Discharge_Lsec_corr = 
  ifelse((C2.Q$date_timeAK > as.POSIXct("2017-06-27 10:45:00")), 
         (C2.Q$Discharge_Lsec_corr = C2.Q$Discharge_Lsec - 8.5047),
         (C2.Q$Discharge_Lsec_corr = C2.Q$Discharge_Lsec))

plot(C2.Q$Discharge_Lsec_corr ~ as.POSIXct(C2.Q$date_timeAK, tz="America/Anchorage"), type ="b",
     xlim = as.POSIXct(c("2017-06-27 01:00:00","2017-06-28 01:00:00"), tz="America/Anchorage"))
plot(C2.Q$Discharge_Lsec_corr ~ as.POSIXct(C2.Q$date_timeAK, tz="America/Anchorage"), type ="l")



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
  ts.df$DateTime = as.POSIXct(row.names(ts.df), tz="America/Anchorage")
  names(ts.df) = c("dat_filled", "DateTime")
  # remove large gaps # 
  ts.df$dat_filled[ts.df$DateTime %in% as.POSIXct(biggaps$DateTime)] = NA
  # Replace large gaps with linear interpolation #
  ts.df$dat_filled = na.interpolation(ts.df$dat_filled)
  ts.df = subset(ts.df, select = c("dat_filled", "DateTime"))
  return(ts.df)
}

# FRCH # 
FRCH.no3 = subset(FRCH, select = c("date_timeAK","nitrate_uM"))
C3.no3 = fillgaps15(C3.no3, C3.no3$nitrate_uM, "nitrate_uM", 16)
plot(C3.no3$dat_filled, type="l")
names(C3.no3) = c("nitrate_uM_filled", "date_timeAK")

FRCH.fDOM = subset(FRCH, select = c("DateTime","fDOM.RFU"))
FRCH.fDOM = fillgaps15(FRCH.fDOM, FRCH.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(FRCH$dat_filled, type="l")
names(FRCH.fDOM) = c("fDOM_filled", "DateTime")

# STRT # 
STRT.no3 <- subset(STRT, select = c("DateTime", "nitrateuM"))
STRT.no3 = fillgaps15(STRT.no3, STRT.no3$nitrateuM, "nitrateuM", 16)
plot(STRT.no3$dat_filled, type="l")
names(STRT.no3) = c("nitrate_uM_filled", "DateTime")

STRT.fDOM = subset(STRT, select = c("DateTime","fDOM.RFU"))
STRT.fDOM = fillgaps15(STRT.fDOM, STRT.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(STRT$dat_filled, type="l")
names(STRT.fDOM) = c("fDOM_filled", "DateTime")

# POKE # 
POKE.no3 <- subset(POKE, select = c("DateTime", "nitrateuM"))
POKE.no3 <- POKE.no3[!is.na(POKE.no3$DateTime), ]
POKE.no3 <-  fillgaps15(POKE.no3, POKE.no3$nitrateuM, "nitrateuM", 16)
plot(POKE.no3$dat_filled, type="l")
names(POKE.no3) = c("nitrate_uM_filled", "DateTime")

POKE.fDOM = subset(POKE, select = c("DateTime","fDOM.RFU"))
POKE.fDOM = fillgaps15(POKE.fDOM, POKE.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(POKE$dat_filled, type="l")
names(POKE.fDOM) = c("fDOM_filled", "DateTime")

# VAUL #
VAUL.no3 <- subset(VAUL, select = c("DateTime", "nitrateuM"))
VAUL.no3 <-  fillgaps15(VAUL.no3, VAUL.no3$nitrateuM, "nitrateuM", 16)
plot(VAUL.no3$dat_filled, type="l")
names(POKE.no3) = c("nitrate_uM_filled", "DateTime")

VAUL.fDOM = subset(VAUL, select = c("DateTime","fDOM.RFU"))
VAUL.fDOM = fillgaps15(VAUL.fDOM, VAUL.fDOM$fDOM.RFU, "fDOM.RFU", 16)
plot(VAUL$dat_filled, type="l")
names(VAUL.fDOM) = c("fDOM_filled", "DateTime")

# MOOS #
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

### examine the recursive digital filter at .9, .925, .95 levels ###
par(mfrow=c(4,2))
plot(frch.final.discharge$MeanDischarge~ frch.final.discharge$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2020-06-01 00:00:00","2020-10-15 00:00:00"), tz="America/Anchorage"),
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

#
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(FRCH_Q, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 
#
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3, na.rm = TRUE)
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(STRT_Q, select = c(day, Discharge_Lsec)), streamflow2=STRT_Q_bf$bt)
#
POKE_Q_bf = BaseflowSeparation(poke.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(POKE_Q, select = c(day, Discharge_Lsec)), streamflow2=POKE_Q_bf$bt) 
#
VAUL_Q_bf = BaseflowSeparation(vaul.final.discharge$MeanDischarge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(VAUL_Q, select = c(day, Discharge_Lsec)), streamflow2=VAUL_Q_bf$bt) 

###.925 ###
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(FRCH_Q, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 
#
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3, na.rm = TRUE)
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(STRT_Q, select = c(day, Discharge_Lsec)), streamflow2=STRT_Q_bf$bt)
#
POKE_Q_bf = BaseflowSeparation(poke.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(POKE_Q, select = c(day, Discharge_Lsec)), streamflow2=POKE_Q_bf$bt) 
#
VAUL_Q_bf = BaseflowSeparation(vaul.final.discharge$MeanDischarge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(VAUL_Q, select = c(day, Discharge_Lsec)), streamflow2=VAUL_Q_bf$bt) 


### .95 ###
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(FRCH_Q, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 
#
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3, na.rm = TRUE)
STRT_Q_bf = BaseflowSeparation(strt.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(STRT_Q, select = c(day, Discharge_Lsec)), streamflow2=STRT_Q_bf$bt)
#
POKE_Q_bf = BaseflowSeparation(poke.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(POKE_Q, select = c(day, Discharge_Lsec)), streamflow2=POKE_Q_bf$bt) 
#
VAUL_Q_bf = BaseflowSeparation(vaul.final.discharge$MeanDischarge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(VAUL_Q, select = c(day, Discharge_Lsec)), streamflow2=VAUL_Q_bf$bt) 


#### Deliniate storms in C3 ####

# ID storms: Any events where Q reached 2X mean base flow 
# Pick starting points: manually select inflection pt when Q began to rise
# Pick ending points: manually select pt when Q reached pre-storm baseflow OR when another event occurred


FRCH_bfQ_mn = mean(FRCH_Q_bf$bt)
FRCH_bfQ_mn
FRCH_bfQ_mn*2

plot(FRCH_Q$Discharge_Lsec ~ FRCH_Q$day, type="l", xlab="", ylab="Q (L/sec)",ylim = c(0,3000), col="blue", main="FRCH")
lines(FRCH_Q_bf$bt ~ FRCH_Q$day, col="red")
#lines((C3_Q_bf$bt*1.3) ~ C3_Q$day, col="red", lty=2)
#lines((C3_Q_bf$bt*5) ~ C3_Q$day, col="red", lty=2)
abline(h = FRCH_bfQ_mn*2, col="red", lty=2)




