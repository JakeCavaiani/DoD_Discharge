# This script loads in 2019 and 2020 final chem and discharge data from 6/18-09/30 for flux measurements between STRT and MOOS 

# Load in libraries 
library(readr)
### Load in data from desktop ###
MOOS_final_chem_2020 <- read_csv("Output for analysis/MOOS_final_chem_2020.csv")
FRCH_final_chem_2020 <- read_csv("Output for analysis/FRCH_final_chem_2020.csv")
VAUL_final_chem_2020 <- read_csv("Output for analysis/VAUL_final_chem_2020.csv")
STRT_final_chem_2020 <- read_csv("Output for analysis/STRT_final_chem_2020.csv")


# Calculate Flux # 
# 2020 # 
# NO3-#
MOOS_final_chem_2020$NO3_FLUX <- MOOS_final_chem_2020$MeanDischarge * MOOS_final_chem_2020$nitrateuM
MOOS_median_flux <- median(MOOS_final_chem_2020$NO3_FLUX, na.rm = TRUE)
MOOS_specific_yield <- MOOS_median_flux*14*(1/113)
MOOS_specific_yield #3099.965
MOOS_specific_yield_final <- ((MOOS_specific_yield/1000000000)*(86400))
MOOS_specific_yield_final #0.267837

FRCH_final_chem_2020$NO3_FLUX <- FRCH_final_chem_2020$MeanDischarge * FRCH_final_chem_2020$nitrateuM
FRCH_median_flux <- median(FRCH_final_chem_2020$NO3_FLUX, na.rm = TRUE)
FRCH_specific_yield <- FRCH_median_flux*14*(1/44)
FRCH_specific_yield #6288.251
FRCH_specific_yield_final <- ((FRCH_specific_yield/1000000000)*(86400))
FRCH_specific_yield_final #0.5433049

VAUL_final_chem_2020$NO3_FLUX <- VAUL_final_chem_2020$MeanDischarge * VAUL_final_chem_2020$nitrateuM
VAUL_median_flux <- median(VAUL_final_chem_2020$NO3_FLUX, na.rm = TRUE)
VAUL_specific_yield <- VAUL_median_flux*14*(1/32)
VAUL_specific_yield #2751.455
VAUL_specific_yield_final <- ((VAUL_specific_yield/1000000000)*(86400))
VAUL_specific_yield_final #0.2377257

STRT_final_chem_2020$NO3_FLUX <- STRT_final_chem_2020$MeanDischarge * STRT_final_chem_2020$nitrateuM
STRT_median_flux <- median(STRT_final_chem_2020$NO3_FLUX, na.rm = TRUE)
STRT_specific_yield <- STRT_median_flux*14*(1/125)
STRT_specific_yield #4786.33
STRT_specific_yield_final <- ((STRT_specific_yield/1000000000)*(86400))
STRT_specific_yield_final #0.4135389


# fDOM # 
MOOS_final_chem_2020$fDOM_FLUX <- MOOS_final_chem_2020$MeanDischarge * MOOS_final_chem_2020$fDOM.QSU
MOOS_median_flux_fDOM_2020 <- median(MOOS_final_chem_2020$fDOM_FLUX, na.rm = TRUE)
MOOS_median_flux_fDOM_2020 #115245.7
MOOS_specific_yield_fDOM_2020 <- MOOS_median_flux_fDOM_2020 * (1/113)
MOOS_specific_yield_final_fDOM_2020 <- (MOOS_specific_yield_fDOM_2020*86400)
MOOS_specific_yield_final_fDOM_2020


STRT_final_chem_2020$fDOM_FLUX <- STRT_final_chem_2020$MeanDischarge * STRT_final_chem_2020$fDOM.QSU
STRT_median_flux_fDOM_2020 <- median(STRT_final_chem_2020$fDOM_FLUX, na.rm = TRUE)
STRT_median_flux_fDOM_2020
STRT_specific_yield_fDOM_2020 <- STRT_median_flux_fDOM_2020 * (1/125)
STRT_specific_yield_final_fDOM_2020 <- (STRT_specific_yield_fDOM_2020*86400)
STRT_specific_yield_final_fDOM_2020

STRT_specific_yield_final_fDOM_2020/MOOS_specific_yield_final_fDOM_2020 #0.7252689



STRT_specific_yield_final/STRT_specific_yield_final_2019

MOOS_specific_yield_final/MOOS_specific_yield_final_2019



median(STRT_final_chem_2020$fDOM.QSU, na.rm = TRUE) # 71.41455


median(MOOS_final_chem_2019$nitrateuM, na.rm = TRUE) # 25.22556
median(MOOS_final_chem_2020$nitrateuM, na.rm = TRUE) # 25.17583
median(MOOS_final_chem_2019$fDOM.QSU.mn, na.rm = TRUE) # 113.9
median(MOOS_final_chem_2020$fDOM.QSU, na.rm = TRUE) # 124.0418






################################ FOR TAMARA ########################################################
ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb) +
  geom_line(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = Strt1comb$DateTime, y = Strt1comb$pred.strt1.Q), data = strt.final.discharge, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Stuart1(light) & Stuart2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

strt.ggplot <- ggplot(strt.final.discharge) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = strt.final.discharge, color = "#A6CEE3", size=1.25) +
  theme_classic() +
  ggtitle("Stuart 2020") +
  ylab("") +
  xlab("Date") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))

moos.ggplot <- ggplot(moos.final.discharge) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = moos.final.discharge, color = "#A6CEE3", size=1.25) +
  theme_classic() +
  ggtitle("Moose 2020") +
  ylab("Discharge L/s") +
  xlab("Date") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))
  
moos.2019.q <- read_csv("~/Documents/DoD_2019/Q/Final_Q/MOOS/MOOS.csv")

moos.ggplot.2019 <- ggplot(moos.2019.q) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = moos.2019.q, color = "#A6CEE3", size=1.25) +
  theme_classic() +
  ggtitle("Moose 2019") +
  ylab("Discharge L/s") +
  xlab("") +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))
moos.ggplot.2019

strt.2019.q<- read_csv("~/Documents/DoD_2019/Q/Final_Q/STRT/STRT.csv")
strt.ggplot.2019 <- ggplot(strt.2019.q) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = strt.2019.q, color = "#A6CEE3", size=1.25) +
  theme_classic() +
  ggtitle("Stuart 2019") +
  ylab("") +
  xlab("") +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))
strt.ggplot.2019

plot_grid(moos.ggplot.2019, strt.ggplot.2019,
          moos.ggplot, strt.ggplot, 
          labels = AUTO)




###################### FROM CPCRW ALEX SCRIPT ####################################################
# press Command+Option+O to collapse all sections and get an overview of the workflow! #

#### read me ####
# The purpose of this script is to calculate and plot specific yeild of NO3 and fDOM

#### libraries ####
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(zoo)
library(xts)
library(forecast)
library(gridExtra)

#### NO3 catchment yeild calcs ####

#CPCRW.2017 = read.csv("Stitched_data/CPCRW.2017_may22.00.00.00_sept01.00.00.00.csv", row.names = 1)
#CPCRW.2017$date_timeAK = as.POSIXct(CPCRW.2017$date_timeAK, "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

# https://www.epa.gov/sites/production/files/2016-05/documents/tech_notes_8_dec_2013_load.pdf

## fxn to fill gaps in data ##
fillgaps = function(df, dat, datquotes, largegap.num){
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
  # # Replace large gaps with linear interpolation #
  # ts.df$dat_filled = na.interpolation(ts.df$dat_filled)
  # ### Make daily ###
  # ### add a column for day ###
  # ts.df$day = format(as.POSIXct(ts.df$date_timeAK,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
  # ts.df$day = as.POSIXct(ts.df$day, "%Y-%m-%d", tz="America/Anchorage")
  # ts.df$date_timeAK = NULL
  # daily.df = 
  #   ts.df %>%
  #   group_by(day) %>%
  #   summarize_all(funs(mean), na.rm = TRUE)
  # #daily.df = paste(df, dat, "daily", sep="_")
  return(ts.df)
} 

### C2 ###
C2 = subset(CPCRW.2017, site.ID == "C2")
C2.no3.filled = fillgaps(C2, C2$nitrate_uM_c, "nitrate_uM_c", (24*4))
C2.Q.filled = fillgaps(C2, C2$Discharge_Lsec, "Discharge_Lsec", (24*4))
C2.no3.Q.filled = left_join(C2.no3.filled, C2.Q.filled, by="date_timeAK")
names(C2.no3.Q.filled) = c("nitrate_uM_c", "date_timeAK", "Discharge_Lsec")
C2.no3.Q.filled$nitrate_mgL = C2.no3.Q.filled$nitrate_uM_c*0.014007
C2.no3.Q.filled$nitrate_uM_c = NULL

plot(C2.no3.Q.filled$nitrate_mgL ~ C2.no3.Q.filled$date_timeAK)
plot(C2.no3.Q.filled$Discharge_Lsec ~ C2.no3.Q.filled$date_timeAK)

C2.no3.Q.filled$NO3_load = C2.no3.Q.filled$Discharge_Lsec * C2.no3.Q.filled$nitrate_mgL
C2.no3.Q.filled$NO3_yield_mg.km2.15min = C2.no3.Q.filled$NO3_load/5.2 # C2 is 5.2 km2
C2.no3.Q.filled$site.ID = "C2"

plot(C2.no3.Q.filled$NO3_yield_mg.km2.15min ~ C2.no3.Q.filled$date_timeAK, type="b")

### C3 ###
C3 = subset(CPCRW.2017, site.ID == "C3")
C3.no3.filled = fillgaps(C3, C3$nitrate_uM_c, "nitrate_uM_c", (24*4))
C3.Q.filled = fillgaps(C3, C3$Discharge_Lsec, "Discharge_Lsec", (24*4))
C3.no3.Q.filled = left_join(C3.no3.filled, C3.Q.filled, by="date_timeAK")
names(C3.no3.Q.filled) = c("nitrate_uM_c", "date_timeAK", "Discharge_Lsec")
C3.no3.Q.filled$nitrate_mgL = C3.no3.Q.filled$nitrate_uM_c*0.014007
C3.no3.Q.filled$nitrate_uM_c = NULL

C3.no3.Q.filled$NO3_load = C3.no3.Q.filled$Discharge_Lsec * C3.no3.Q.filled$nitrate_mgL
C3.no3.Q.filled$NO3_yield_mg.km2.15min = C3.no3.Q.filled$NO3_load/5.7 # C3 is 5.7 km2
C3.no3.Q.filled$site.ID = "C3"

plot(C3.no3.Q.filled$NO3_yield_mg.km2.15min ~ C3.no3.Q.filled$date_timeAK)

# DOD Sites #
MOOS$NO3_yield_km2 <- MOOS$nitrateuM/113 #MOOS is 113km^2
FRCH$NO3_yield_km2 <- FRCH$nitrateuM/44 # FRCH is 44km^2
POKE$NO3_yield_km2 <- POKE$nitrateuM/60 # Poke is 60km^2
VAUL$NO3_yield_km2 <- VAUL$nitrateuM/32 # VAUL is 32km^2
STRT$NO3_yield_km2 <- STRT$nitrateuM/125 # STRT is 125 km^2

POKE <- POKE[,-(4)]
POKE <- POKE[,-(5)]
names(POKE) <- c("Site", "DateTime", "MeanDischarge", "fDOM.QSU", "nitrateuM", "NO3_yield_km2")
VAUL <- VAUL[,(-5)]
STRT <- STRT[,(-5)]
no3.yield <- rbind(FRCH, MOOS, POKE, VAUL, STRT)
no3.yield$day <- date(no3.yield$DateTime) # make a day column
no3.daily.means <- aggregate(x=no3.yield$NO3_yield_km2,
                             by=list(no3.yield$Site,no3.yield$day),
                             FUN=mean, na.rm = TRUE)
names(no3.daily.means) <- c("Site", "DateTime", "NO3_yield_km2")

ggplot(no3.daily.means, aes(Site, NO3_yield_km2)) + geom_boxplot(aes(fill=Site))+
  scale_fill_manual(values=c("#0571B0","#CA0020", "yellow", "green", "red"), "Catchment") +
  theme_bw() + ylab("NO3- daily mean yield (mg N/km2/day)") + xlab("") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.position = "none")


#### NO3 catchment yield plot ####

# calc daily yield #
no3.yield.daily = no3.yield
no3.yield.daily$day = format(as.POSIXct(no3.yield.daily$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
no3.yield.daily$day = as.POSIXct(no3.yield.daily$day, "%Y-%m-%d", tz="America/Anchorage")
no3.yield.daily$DateTime = NULL
no3.daily.means <- aggregate(x=no3.yield$NO3_yield_km2,
                             by=list(no3.yield$Site,no3.yield$day),
                             FUN=sum, na.rm = FALSE)


no3.yield.daily$NO3_yield_km2.day = no3.yield.daily$NO3_yield_km2
no3.yield.daily$NO3_yield_km2 = NULL

median(no3.yield.daily$NO3_yield_km2.day[no3.yield.daily$Site=="FRCH" & 
                                              !is.na(no3.yield.daily$NO3_yield_km2.day)]) /
  median(no3.yield.daily$NO3_yield_km2.day[no3.yield.daily$Site=="MOOS"& 
                                                !is.na(no3.yield.daily$NO3_yield_km2.day)])


sum(no3.yield.daily$NO3_yield_km2.day[no3.yield.daily$Site=="FRCH"], na.rm = TRUE)
sum(no3.yield.daily$NO3_yield_km2.day[no3.yield.daily$Site=="MOOS"], na.rm = TRUE)

par(mfrow=c(2,1))
plot(density(no3.yield.daily$NO3_yield_km2.day[no3.yield.daily$Site=="FRCH"]), na.rm = TRUE)

plot(density(no3.yield.daily$NO3_yield_km2.day[no3.yield.daily$Site=="MOOS"]))

no3.yield.daily.density = 
  ggplot() + 
  geom_density(data=no3.yield.daily[no3.yield.daily$site.ID=="C3",], 
               aes(x=NO3_yield_mg.km2.day), fill="#0571B0",alpha=0.5) + 
  geom_density(data=no3.yield.daily[no3.yield.daily$site.ID=="C2",], 
               aes(x=NO3_yield_mg.km2.day), fill="#CA0020", alpha=0.5) +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size =18),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = margin(.5,2,.5,.5, "cm"))

mean(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID=="C3"])
mean(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID=="C2"])

# plot #
no3.yield.daily$site.ID <- factor(no3.yield.daily$site.ID, levels = c('C3', 'C2'))

no3.yield.daily.p = 
  ggplot(no3.yield.daily, aes(site.ID, NO3_yield_mg.km2.day)) + geom_boxplot(aes(fill=site.ID))+
  scale_fill_manual(values=c("#0571B0","#CA0020"), "Catchment") +
  theme_bw() + ylab("Nitrate yield (mg N/km2/day)") + xlab("") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.position = "none")

ggplot(no3.yield.daily, aes(x=day, y=NO3_yield_mg.km2.day)) +geom_line(aes(colour=site.ID))

#### NO3 catchment yeild stat test ####

par(mfrow=c(2,1))
plot(density(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID == "C2"]))
shapiro.test((no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID == "C2"]))
plot(density(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID == "C3"]))
shapiro.test((no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID == "C3"]))
par(mfrow=c(1,1))

acf(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID == "C2"])
acf(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID == "C3"])

### bootstrapped difference of medians ####
library(boot)

medianDiff = function(data, indices) { 
  d <- data[indices,] # allows boot to select sample
  m1 = median(d$NO3_yield_mg.km2.day[d$site.ID == "C2"])
  m2 = median(d$NO3_yield_mg.km2.day[d$site.ID == "C3"])
  m = m1 - m2
  return(m)
}

no3_boot = boot(no3.yield.daily, medianDiff, R = 10000, strata = as.factor(no3.yield.daily$site.ID))
no3_bootCI = boot.ci(no3_boot, type="bca")

no3_bootCI = data.frame(cbind(no3_boot$t0, no3_bootCI[["bca"]][4], no3_bootCI[["bca"]][5]))
names(no3_bootCI) = c("no3", "lower", "upper")

### gls with AR1 ####

library(nlme)

par(mfrow=c(2,1))
plot(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID=="C2"] ~ 
       no3.yield.daily$day[no3.yield.daily$site.ID=="C2"])
plot(no3.yield.daily$NO3_yield_mg.km2.day[no3.yield.daily$site.ID=="C3"] ~ 
       no3.yield.daily$day[no3.yield.daily$site.ID=="C3"])

mod = gls(NO3_yield_mg.km2.day ~ site.ID, 
          na.action=na.omit, 
          data=no3.yield.daily, 
          correlation=corAR1(form=~day|site.ID))

Acf(resid(mod, type="normalized"))
pacf(resid(mod, type="normalized"))
summary(mod)




mod.null = lme(NO3_yield_mg.km2.day~1, random=~1|site.ID, na.action=na.omit, data=no3.yield.daily, correlation = corAR1(form=~day|site.ID))

AICtab(mod.null, mod)

mod = gls(NO3_yield_mg.km2.day~site.ID, na.action=na.omit, data=no3.yield.daily, correlation=corARMA(form=~day|site.ID, c(-0.2,0.2), p=1, q=1))
acf(resid(mod, type="normalized"))



## arima
arima.null = auto.arima(as.ts(no3.yield.daily$NO3_yield_mg.km2.day))

newxreg = as.numeric(no3.yield.daily$site.ID)
arima1 = auto.arima(as.ts(no3.yield.daily$NO3_yield_mg.km2.day), xreg = newxreg)
acf(resid(arima1))
shapiro.test(resid(arima1))
summary(arima1)

library(bbmle)
AICtab(arima.null, arima1)

#### fDOM catchment yeild calcs ####

# https://www.epa.gov/sites/production/files/2016-05/documents/tech_notes_8_dec_2013_load.pdf

## fxn to fill gaps in data ##
fillgaps = function(df, dat, datquotes, largegap.num){
  ## Document gaps >= largegap.num (1 largegap.num = 15 min) ##
  # (note - the criteria of what constitutes a "large" gap should be reevaluated depending on the trend being characterized)
  is.na.rle <- rle(is.na(dat))
  is.na.rle$values <- is.na.rle$values & is.na.rle$lengths >= (largegap.num)
  biggaps = df[inverse.rle(is.na.rle), ]
  tz(biggaps$date_timeAK) = "America/Anchorage"
  biggaps = subset(biggaps, select = "date_timeAK")
  # Make univariate time series, covert to zoo, then to ts #
  ts.xts = subset(df, select = c("date_timeAK",datquotes))
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
  names(ts.df) = c("dat_filled", "date_timeAK")
  # remove large gaps # 
  ts.df$dat_filled[ts.df$date_timeAK %in% as.POSIXct(biggaps$date_timeAK)] = NA
  # # Replace large gaps with linear interpolation #
  # ts.df$dat_filled = na.interpolation(ts.df$dat_filled)
  # ### Make daily ###
  # ### add a column for day ###
  # ts.df$day = format(as.POSIXct(ts.df$date_timeAK,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
  # ts.df$day = as.POSIXct(ts.df$day, "%Y-%m-%d", tz="America/Anchorage")
  # ts.df$date_timeAK = NULL
  # daily.df = 
  #   ts.df %>%
  #   group_by(day) %>%
  #   summarize_all(funs(mean), na.rm = TRUE)
  # #daily.df = paste(df, dat, "daily", sep="_")
  return(ts.df)
} 


# fDOM #
#Clean columns and make all same name # 
MOOS_final_chem_2020 <- MOOS_final_chem_2020[,-c(1,6)]
MOOS_final_chem_2020 <- MOOS_final_chem_2020[,-(7)]
FRCH_final_chem_2020 <- FRCH_final_chem_2020[,-c(1,5)]
#POKE <- POKE[,-6]
#names(POKE) <- c("Site", "DateTime", "MeanDischarge", "fDOM.QSU", "nitrateuM")
VAUL_final_chem_2020 <- VAUL_final_chem_2020[,-c(1,6)]
STRT_final_chem_2020 <- STRT_final_chem_2020[,-c(1,6)]

MOOS_final_chem_2020$fDOM_yield_km2 <- MOOS_final_chem_2020$fDOM.QSU/113 # MOOS is 113km^2
FRCH_final_chem_2020$fDOM_yield_km2 <- FRCH_final_chem_2020$fDOM.QSU/44  # FRCH is 44km^2
#POKE$fDOM_yield_km2 <- POKE$fDOM.QSU/60 # Poke is 60km^2
VAUL_final_chem_2020$fDOM_yield_km2 <- VAUL_final_chem_2020$fDOM.QSU/32 # VAUL is 32km^2
STRT_final_chem_2020$fDOM_yield_km2 <- STRT_final_chem_2020$fDOM.QSU/125 # STRT is 125 km^2




fDOM.yield = rbind(MOOS_final_chem_2020, FRCH_final_chem_2020, VAUL_final_chem_2020, STRT_final_chem_2020)
fDOM.yield$day <- date(fDOM.yield$DateTime)# make a day column
fDOM.daily.means <- aggregate(x=fDOM.yield$fDOM_yield_km2,
                              by=list(fDOM.yield$Site,fDOM.yield$day),
                              FUN=mean, na.rm = TRUE)
names(fDOM.daily.means) <- c("Site", "DateTime", "fDOM_yield_km2")

ggplot(fDOM.daily.means, aes(Site, fDOM_yield_km2)) + geom_boxplot(aes(fill=Site))+
  scale_fill_manual(values=c("#0571B0","#CA0020","green", "red"), "Catchment") +
  theme_bw() + ylab("fDOM daily mean yield (QSU/km2/day)") + xlab("") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.position = "none")+
  ylim(0, 10)



fDOM.yield = rbind(C2.fDOM.Q.filled, C3.fDOM.Q.filled, C4.fDOM.Q.filled, P6.fDOM.Q.filled)

#### fDOM catchment yeild plot ####

# calc daily yield #
fDOM.yield.daily = fDOM.yield
fDOM.yield.daily$day = format(as.POSIXct(fDOM.yield.daily$date_timeAK,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
fDOM.yield.daily$day = as.POSIXct(fDOM.yield.daily$day, "%Y-%m-%d", tz="America/Anchorage")
fDOM.yield.daily$date_timeAK = NULL
fDOM.yield.daily = 
  fDOM.yield.daily %>%
  group_by(day, site.ID) %>%
  summarize_all(funs(sum), na.rm = FALSE)

fDOM.yield.daily$fDOM_yield_mg.km2.day = fDOM.yield.daily$fDOM_yield_mg.km2.15min
fDOM.yield.daily$fDOM_yield_mg.km2.15min = NULL

# plot #
fDOM.yield.daily$site.ID <- factor(fDOM.yield.daily$site.ID, levels = c('C3', 'P6', 'C4', 'C2'))

fDOM.yield.daily.p = 
  ggplot(fDOM.yield.daily, aes(site.ID, (fDOM_yield_mg.km2.day))) + geom_boxplot(aes(fill=site.ID))+
  scale_fill_manual(values=c("#0571B0","#92C5DE","#F4A582", "#CA0020"), "Catchment") +
  theme_bw() + ylab("fDOM yield (QSU/km2/day)") + xlab("") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20))+
  scale_y_log10()

fDOM.yield.daily.density = 
  ggplot() + 
  geom_density(data=fDOM.yield.daily[fDOM.yield.daily$site.ID=="C3",], 
               aes(x=fDOM_yield_mg.km2.day), fill="#0571B0",alpha=0.5) + 
  geom_density(data=fDOM.yield.daily[fDOM.yield.daily$site.ID=="P6",], 
               aes(x=fDOM_yield_mg.km2.day), fill="#92C5DE", alpha=0.5) +
  geom_density(data=fDOM.yield.daily[fDOM.yield.daily$site.ID=="C4",], 
               aes(x=fDOM_yield_mg.km2.day), fill="#F4A582", alpha=0.5) +
  geom_density(data=fDOM.yield.daily[fDOM.yield.daily$site.ID=="C2",], 
               aes(x=fDOM_yield_mg.km2.day), fill="#CA0020", alpha=0.5) +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size =18),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", size = 2),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = margin(.5,2,.5,.5, "cm"))+
  scale_x_log10(breaks= c(2000, 10000, 100000), 
                labels =c("2,000","10,000","100,000"))
fDOM.yield.daily.density

median(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C3" & !is.na(fDOM.yield.daily$fDOM_yield_mg.km2.day)])/
  median(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C2" & !is.na(fDOM.yield.daily$fDOM_yield_mg.km2.day)])

median(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="P6" & !is.na(fDOM.yield.daily$fDOM_yield_mg.km2.day)])/
  median(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C3" & !is.na(fDOM.yield.daily$fDOM_yield_mg.km2.day)])

median(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C2" & !is.na(fDOM.yield.daily$fDOM_yield_mg.km2.day)])/
  median(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C4" & !is.na(fDOM.yield.daily$fDOM_yield_mg.km2.day)])

par(mfrow=c(2,1))
plot(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C2"] ~ fDOM.yield.daily$day[fDOM.yield.daily$site.ID=="C2"], type="b")
plot(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID=="C4"] ~ fDOM.yield.daily$day[fDOM.yield.daily$site.ID=="C4"], type="b")


### 

grid.arrange(no3.yield.daily.p, fDOM.yield.daily.p, nrow=1)

grid.arrange(no3.yield.daily.density, fDOM.yield.daily.density, nrow=1)


#### fDOM catchment yeild stat test ####

par(mfrow=c(2,2))
plot(density(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "C2"]))
shapiro.test((fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "C2"]))
plot(density(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "C4"]))
shapiro.test((fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "C4"]))
plot(density(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "P6"]))
shapiro.test((fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "P6"]))
plot(density(fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "C3"]))
shapiro.test((fDOM.yield.daily$fDOM_yield_mg.km2.day[fDOM.yield.daily$site.ID == "C3"]))
par(mfrow=c(1,1))


