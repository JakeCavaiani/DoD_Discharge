title: "24_CPCRW_2017"
author: "Alex Webster"
date: "6/7/2019"
output: html_document
---
  
# READ ME #**The purpose of this script is to run CPCRW storm data through the hysteresisMetrics function to get a hysteresis index and flushing index for each storm.**
# R setup
{r setup, include=F}
library(reticulate)
Sys.which("python")
use_python("/usr/bin/python")
library(tidyverse)
options(tz="America/Anchorage")
library(ggplot2)
library(gridExtra)
library(boot)


# Python setup
{python}
import pickle
import pandas as pd
from hysteresis_metrics import hysteresisMetrics
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)
quit()


# load data to pandas
{python}
MOOS_storm2_06_28_Q <- read_csv("Storms/MOOS_storm2_06_28_Q.csv", usecols = (1,2))
MOOS_storm2_06_28_NO3 <- read_csv("Storms/MOOS_storm2_06_28_NO3.csv", usecols = (1,2)) 
MOOS_storm2_06_28_fDOM <- read_csv("Storms/MOOS_storm2_06_28_fDOM.csv", usecols = (1,2)) 


#C3_storm1a_Q = pd.read_csv("Stitched_data/storms/C3_storm1a_Q.csv", usecols = (1,2)) 
#C3_storm1a_NO3 = pd.read_csv("Stitched_data/storms/C3_storm1a_NO3.csv", usecols = (1,2)) 
#C3_storm1a_SpCond = pd.read_csv("Stitched_data/storms/C3_storm1a_SpCond.csv", usecols = (1,2)) 
#C3_storm1a_fDOM = pd.read_csv("Stitched_data/storms/C3_storm1a_fDOM.csv", usecols = (1,2)) 
#C3_storm1a_turb = pd.read_csv("Stitched_data/storms/C3_storm1a_turb.csv", usecols = (1,2)) 

#C3_storm1b_Q = pd.read_csv("Stitched_data/storms/C3_storm1b_Q.csv", usecols = (1,2)) 
#C3_storm1b_NO3 = pd.read_csv("Stitched_data/storms/C3_storm1b_NO3.csv", usecols = (1,2)) 
#C3_storm1b_SpCond = pd.read_csv("Stitched_data/storms/C3_storm1b_SpCond.csv", usecols = (1,2)) 
#C3_storm1b_fDOM = pd.read_csv("Stitched_data/storms/C3_storm1b_fDOM.csv", usecols = (1,2)) 
#C3_storm1b_turb = pd.read_csv("Stitched_data/storms/C3_storm1b_turb.csv", usecols = (1,2)) 

#C3_storm2_Q = pd.read_csv("Stitched_data/storms/C3_storm2_Q.csv", usecols = (1,2)) 
#C3_storm2_NO3 = pd.read_csv("Stitched_data/storms/C3_storm2_NO3.csv", usecols = (1,2)) 
#C3_storm2_SpCond = pd.read_csv("Stitched_data/storms/C3_storm2_SpCond.csv", usecols = (1,2)) 
#C3_storm2_fDOM = pd.read_csv("Stitched_data/storms/C3_storm2_fDOM.csv", usecols = (1,2)) 
#C3_storm2_turb = pd.read_csv("Stitched_data/storms/C3_storm2_turb.csv", usecols = (1,2)) 

#C3_storm3_Q = pd.read_csv("Stitched_data/storms/C3_storm3_Q.csv", usecols = (1,2)) 
#C3_storm3_NO3 = pd.read_csv("Stitched_data/storms/C3_storm3_NO3.csv", usecols = (1,2)) 
#C3_storm3_SpCond = pd.read_csv("Stitched_data/storms/C3_storm3_SpCond.csv", usecols = (1,2)) 
#C3_storm3_fDOM = pd.read_csv("Stitched_data/storms/C3_storm3_fDOM.csv", usecols = (1,2)) 
#C3_storm3_turb = pd.read_csv("Stitched_data/storms/C3_storm3_turb.csv", usecols = (1,2)) 

#C3_storm4_Q = pd.read_csv("Stitched_data/storms/C3_storm4_Q.csv", usecols = (1,2)) 
#C3_storm4_NO3 = pd.read_csv("Stitched_data/storms/C3_storm4_NO3.csv", usecols = (1,2)) 
#C3_storm4_SpCond = pd.read_csv("Stitched_data/storms/C3_storm4_SpCond.csv", usecols = (1,2)) 
#C3_storm4_fDOM = pd.read_csv("Stitched_data/storms/C3_storm4_fDOM.csv", usecols = (1,2)) 
#C3_storm4_turb = pd.read_csv("Stitched_data/storms/C3_storm4_turb.csv", usecols = (1,2)) 

#C3_storm5_Q = pd.read_csv("Stitched_data/storms/C3_storm5_Q.csv", usecols = (1,2)) 
#C3_storm5_NO3 = pd.read_csv("Stitched_data/storms/C3_storm5_NO3.csv", usecols = (1,2)) 
#C3_storm5_SpCond = pd.read_csv("Stitched_data/storms/C3_storm5_SpCond.csv", usecols = (1,2)) 
#C3_storm5_fDOM = pd.read_csv("Stitched_data/storms/C3_storm5_fDOM.csv", usecols = (1,2)) 
#C3_storm5_turb = pd.read_csv("Stitched_data/storms/C3_storm5_turb.csv", usecols = (1,2)) 



# C3_storms
{python}

## NO3 ##
MOOS_storm2_06_28
_Q['valuedatetime'] =  pd.to_datetime(MOOS_storm1_06_21_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm1_NO3['valuedatetime'] =  pd.to_datetime(C3_storm1_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm1_NO3 = hysteresisMetrics(C3_storm1_Q, C3_storm1_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm1a_Q['valuedatetime'] =  pd.to_datetime(C3_storm1a_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm1a_NO3['valuedatetime'] =  pd.to_datetime(C3_storm1a_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm1a_NO3 = hysteresisMetrics(C3_storm1a_Q, C3_storm1a_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm1b_Q['valuedatetime'] =  pd.to_datetime(C3_storm1b_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm1b_NO3['valuedatetime'] =  pd.to_datetime(C3_storm1b_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm1b_NO3 = hysteresisMetrics(C3_storm1b_Q, C3_storm1b_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm2_Q['valuedatetime'] =  pd.to_datetime(C3_storm2_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm2_NO3['valuedatetime'] =  pd.to_datetime(C3_storm2_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm2_NO3 = hysteresisMetrics(C3_storm2_Q, C3_storm2_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm3_Q['valuedatetime'] =  pd.to_datetime(C3_storm3_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm3_NO3['valuedatetime'] =  pd.to_datetime(C3_storm3_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm3_NO3 = hysteresisMetrics(C3_storm3_Q, C3_storm3_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm4_Q['valuedatetime'] =  pd.to_datetime(C3_storm4_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm4_NO3['valuedatetime'] =  pd.to_datetime(C3_storm4_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm4_NO3 = hysteresisMetrics(C3_storm4_Q, C3_storm4_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm5_Q['valuedatetime'] =  pd.to_datetime(C3_storm5_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm5_NO3['valuedatetime'] =  pd.to_datetime(C3_storm5_NO3['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm5_NO3 = hysteresisMetrics(C3_storm5_Q, C3_storm5_NO3, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')



## fDOM ##
C3_storm1_Q['valuedatetime'] =  pd.to_datetime(C3_storm1_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm1_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm1_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm1_fDOM = hysteresisMetrics(C3_storm1_Q, C3_storm1_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm1a_Q['valuedatetime'] =  pd.to_datetime(C3_storm1a_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm1a_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm1a_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm1a_fDOM = hysteresisMetrics(C3_storm1a_Q, C3_storm1a_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm1b_Q['valuedatetime'] =  pd.to_datetime(C3_storm1b_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm1b_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm1b_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm1b_fDOM = hysteresisMetrics(C3_storm1b_Q, C3_storm1b_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm2_Q['valuedatetime'] =  pd.to_datetime(C3_storm2_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm2_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm2_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm2_fDOM = hysteresisMetrics(C3_storm2_Q, C3_storm2_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm3_Q['valuedatetime'] =  pd.to_datetime(C3_storm3_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm3_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm3_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm3_fDOM = hysteresisMetrics(C3_storm3_Q, C3_storm3_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm4_Q['valuedatetime'] =  pd.to_datetime(C3_storm4_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm4_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm4_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm4_fDOM = hysteresisMetrics(C3_storm4_Q, C3_storm4_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')

C3_storm5_Q['valuedatetime'] =  pd.to_datetime(C3_storm5_Q['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
C3_storm5_fDOM['valuedatetime'] =  pd.to_datetime(C3_storm5_fDOM['valuedatetime'], format='%Y-%m-%d %H:%M:%S')
timespacing = 15 # 15 minutes between records
hysdict_C3_storm5_fDOM = hysteresisMetrics(C3_storm5_Q, C3_storm5_fDOM, timespacing, timespacing, debug=False, interpall=True, discharge_time_spacing_units='minutes', response_time_spacing_units='minutes', discharge_units='Lsec')


# C3 results
```{r}

C3.hyst.results.list = list(py$hysdict_C3_storm1_NO3,
                            py$hysdict_C3_storm1a_NO3,
                            py$hysdict_C3_storm1b_NO3,
                            py$hysdict_C3_storm2_NO3,
                            py$hysdict_C3_storm3_NO3,
                            py$hysdict_C3_storm4_NO3,
                            py$hysdict_C3_storm5_NO3,
                            py$hysdict_C3_storm1_fDOM,
                            py$hysdict_C3_storm1a_fDOM,
                            py$hysdict_C3_storm1b_fDOM,
                            py$hysdict_C3_storm2_fDOM,
                            py$hysdict_C3_storm3_fDOM,
                            py$hysdict_C3_storm4_fDOM,
                            py$hysdict_C3_storm5_fDOM,
                            py$hysdict_C3_storm1_SpCond,
                            py$hysdict_C3_storm1a_SpCond,
                            py$hysdict_C3_storm1b_SpCond,
                            py$hysdict_C3_storm2_SpCond,
                            py$hysdict_C3_storm3_SpCond,
                            py$hysdict_C3_storm4_SpCond,
                            py$hysdict_C3_storm5_SpCond,
                            py$hysdict_C3_storm1_turb,
                            py$hysdict_C3_storm1a_turb,
                            py$hysdict_C3_storm1b_turb,
                            py$hysdict_C3_storm2_turb,
                            py$hysdict_C3_storm3_turb,
                            py$hysdict_C3_storm4_turb,
                            py$hysdict_C3_storm5_turb)

C3.hyst.results.list.2 = list()
for (i in 1:length(C3.hyst.results.list)){
  C3.hyst.results.list.2[[i]] = as.data.frame(t(as.numeric(c(
    C3.hyst.results.list[[i]][["HI_mean_with_Interp"]],
    C3.hyst.results.list[[i]][["HI_standard_deviation_with_Interp"]],
    C3.hyst.results.list[[i]][["Normalized slope of response"]],
    C3.hyst.results.list[[i]][["interpolated Max width of response"]],
    C3.hyst.results.list[[i]][["Min response"]],
    C3.hyst.results.list[[i]][["Max response"]],
    C3.hyst.results.list[[i]][["Peak Q"]]
  ))))
  names(C3.hyst.results.list.2[[i]]) = c("HI_mean_Interp", "HI_sd_with_Interp", "N.S.", "Max_width_Interp", "Min_response", "Max_response", "Peak_Q")
}

HIs.list = list()
HIs.tests = list()
for (i in 1:length(C3.hyst.results.list)){
  HIs.list[[i]] = unlist(C3.hyst.results.list[[i]][["Hysteresis_Index"]],recursive=FALSE)
  HIs.tests[[i]] = as.data.frame(t(round(as.numeric(c(shapiro.test(HIs.list[[i]])$statistic, shapiro.test(HIs.list[[i]])$p.value,
                                                      t.test(HIs.list[[i]], mu=0)$statistic, t.test(HIs.list[[i]], mu=0)$p.value, 
                                                      t.test(HIs.list[[i]], mu=0)$conf.int[1],t.test(HIs.list[[i]], mu=0)$conf.int[2],
                                                      wilcox.test(HIs.list[[i]], mu=0)$statistic, wilcox.test(HIs.list[[i]], mu=0)$p.value)), 4)))
  names(HIs.tests[[i]]) = c("ShapiroTest.W", "ShapiroTest.p", "t.test.stat", "t.test.p", "t.test.CIlow", "t.test.CIhigh",
                            "wilcox.test.stat", "wilcox.test.p")
}

C3.hyst.results.list.3 =list()
for (i in 1:length(C3.hyst.results.list)){
  C3.hyst.results.list.3[[i]] = cbind(C3.hyst.results.list.2[[i]], HIs.tests[[i]])
}

C3.hyst.results.df = bind_rows(C3.hyst.results.list.3, .id = "column_label")

C3.hyst.results.df$storm.ID = c("C3_storm1_NO3",
                                "C3_storm1a_NO3",
                                "C3_storm1b_NO3",
                                "C3_storm2_NO3",
                                "C3_storm3_NO3",
                                "C3_storm4_NO3",
                                "C3_storm5_NO3",
                                "C3_storm1_fDOM",
                                "C3_storm1a_fDOM",
                                "C3_storm1b_fDOM",
                                "C3_storm2_fDOM",
                                "C3_storm3_fDOM",
                                "C3_storm4_fDOM",
                                "C3_storm5_fDOM",
                                "C3_storm1_SpCond",
                                "C3_storm1a_SpCond",
                                "C3_storm1b_SpCond",
                                "C3_storm2_SpCond",
                                "C3_storm3_SpCond",
                                "C3_storm4_SpCond",
                                "C3_storm5_SpCond",
                                "C3_storm1_turb",
                                "C3_storm1a_turb",
                                "C3_storm1b_turb",
                                "C3_storm2_turb",
                                "C3_storm3_turb",
                                "C3_storm4_turb",
                                "C3_storm5_turb")

C3.hyst.results.df$site.ID = "C3"

write.csv(C3.hyst.results.df, "/Users/alexwebster/Dropbox/Harms Lab/CPCRW 2017/new tidy files/CPCRW_2017/Output from analyses/24_25_CPCRW_2017/C3.hyst.results.csv")

# plot HI by normalized discharge
HIs.Q.list =list()
HIs.df.list = list()
pdf("/Users/alexwebster/Dropbox/Harms Lab/CPCRW 2017/new tidy files/CPCRW_2017/Output from analyses/24_25_CPCRW_2017/C3.hyst.HI_Q.plots.pdf", width = 25, height =15, onefile=FALSE)
par(mfrow=c(4,7))
for (i in 1:length(HIs.list)) {
  HIs.Q.list[[i]] = names(HIs.list[[i]])
  HIs.Q.list[[i]] = (sapply(strsplit(HIs.Q.list[[i]], " "), "[[", 4))
  HIs.Q.list[[i]] = as.numeric(gsub("%", "", HIs.Q.list[[i]]))
  HIs.df.list[[i]] = as.data.frame(cbind(HIs.list[[i]], HIs.Q.list[[i]]))
  HIs.df.list[[i]]= HIs.df.list[[i]][order(HIs.df.list[[i]][["V2"]]),]
  plot(HIs.df.list[[i]][["V1"]] ~ HIs.df.list[[i]][["V2"]], type="l",
       ylab="norm.response", xlab="Q intervals", main= C3.hyst.results.df$storm.ID[i])
  abline(h=0, lty=2)
}
dev.off()

# plot HI by normalized discharge
HIs.Q.list =list()
HIs.df.list = list()
pdf("/Users/alexwebster/Dropbox/Harms Lab/CPCRW 2017/new tidy files/CPCRW_2017/Output from analyses/24_25_CPCRW_2017/C3.hyst.HI_boxplots.pdf", width = 25, height =15, onefile=FALSE)
par(mfrow=c(4,7))
for (i in 1:length(HIs.list)) {
  HIs.Q.list[[i]] = names(HIs.list[[i]])
  HIs.Q.list[[i]] = (sapply(strsplit(HIs.Q.list[[i]], " "), "[[", 4))
  HIs.Q.list[[i]] = as.numeric(gsub("%", "", HIs.Q.list[[i]]))
  HIs.df.list[[i]] = as.data.frame(cbind(HIs.list[[i]], HIs.Q.list[[i]]))
  HIs.df.list[[i]]= HIs.df.list[[i]][order(HIs.df.list[[i]][["V2"]]),]
  boxplot(HIs.df.list[[i]][["V1"]], xlab="HIs", main= C3.hyst.results.df$storm.ID[i])
  abline(h=0, lty=2)
}
dev.off()

## plot HI with bootstrapped 95% CIs around the median ##
HIs.Q.list =list()
HIs.df.list = list()
par(mfrow=c(1,1))
for (i in 1:length(HIs.list)) {
  HIs.Q.list[[i]] = names(HIs.list[[i]])
  HIs.Q.list[[i]] = (sapply(strsplit(HIs.Q.list[[i]], " "), "[[", 4))
  HIs.Q.list[[i]] = as.numeric(gsub("%", "", HIs.Q.list[[i]]))
  HIs.df.list[[i]] = as.data.frame(cbind(HIs.list[[i]], HIs.Q.list[[i]]))
  HIs.df.list[[i]]= HIs.df.list[[i]][order(HIs.df.list[[i]][["V2"]]),]
  names(HIs.df.list[[i]]) = c("HI", "Q_interval")
}

C3.HI.df = bind_rows(HIs.df.list, .id = "column_label")
C3.HI.df$storm.ID = c(rep("C3_storm1_NO3", 50),
                      rep("C3_storm1a_NO3", 50),
                      rep("C3_storm1b_NO3", 50),
                      rep("C3_storm2_NO3",50),
                      rep("C3_storm3_NO3",50),
                      rep("C3_storm4_NO3",50),
                      rep("C3_storm5_NO3",50),
                      rep("C3_storm1_fDOM",50),
                      rep("C3_storm1a_fDOM",50),
                      rep("C3_storm1b_fDOM",50),
                      rep("C3_storm2_fDOM",50),
                      rep("C3_storm3_fDOM",50),
                      rep("C3_storm4_fDOM",50),
                      rep("C3_storm5_fDOM",50),
                      rep("C3_storm1_SpCond",50),
                      rep("C3_storm1a_SpCond",50),
                      rep("C3_storm1b_SpCond",50),
                      rep("C3_storm2_SpCond",50),
                      rep("C3_storm3_SpCond",50),
                      rep("C3_storm4_SpCond",50),
                      rep("C3_storm5_SpCond",50),
                      rep("C3_storm1_turb",50),
                      rep("C3_storm1a_turb",50),
                      rep("C3_storm1b_turb",50),
                      rep("C3_storm2_turb",50),
                      rep("C3_storm3_turb",50),
                      rep("C3_storm4_turb", 50),
                      rep("C3_storm5_turb", 50))
C3.HI.df$storm.ID = as.factor(C3.HI.df$storm.ID)
C3.HI.df = separate(data=C3.HI.df, col=storm.ID, into=c("site.ID","storm.num", "response"), sep = "_", remove = F)

par(mfrow=c(1,1))
median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}
g0 <- ggplot(C3.HI.df, aes(x = storm.num, y = HI, label=storm.num, fill=response))
g1 = g0 + geom_jitter(width = 0.1, fill = "grey", colour = "#0571B0", alpha=0.25, size=3) + 
  theme(axis.text.x = element_text(angle = 0))+  labs(x="") + facet_wrap(~ response, scales = "free_x") +
  theme_bw() +geom_hline(yintercept=0) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
g2 <- g1 + 
  stat_summary(fun.data = median_cl_boot, geom = "errorbar",
               colour = "black", width = 0.2, size=1) + 
  stat_summary(fun.y = median, geom = "point", 
               colour = "black", size = 3)
ggsave("/Users/alexwebster/Dropbox/Harms Lab/CPCRW 2017/new tidy files/CPCRW_2017/Output from analyses/24_25_CPCRW_2017/C3.HI.median.boot.pdf", plot=g2, width = 8, height = 6, units ="in")
write.csv(C3.HI.df, "/Users/alexwebster/Dropbox/Harms Lab/CPCRW 2017/new tidy files/CPCRW_2017/Output from analyses/24_25_CPCRW_2017/C3.HI.df.csv")

```


