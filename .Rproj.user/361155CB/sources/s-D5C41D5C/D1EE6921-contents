### This script reads in and plots Rain Gauge data at STRT FRCH and MOOS sites for 2020 ###

#Step 1: Import raw data from HOBOware 
#Step 2: Plot data
#Step 3: output csv with Site, DateTime, and rainfall


# Import Libraries #
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

### Import Data ###
strt.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT05dgyg07MOSVdaEFFywTuktj7TTY0ukYjp5ZckRTP5BJ9dFaq5fT0MppdTchBJseTlLSzoyuLbWNS/pub?output=csv"
frch.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSXjY8hE7y3xpQs42ce8tTuqxadD-9GSLdlzmlzapcdxZoYwZ0M3GOjrQfKshj5Cw1XlPMICGK2v51L/pub?output=csv"
vaul.url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTd-IH6XcwWpzDQav6eQO5NBzV2XB3XzCwJdxHarSiOniu57EoqegV8vU4eF8bo_8REcr8SxsXaDMpR/pub?output=csv"

strt.gauge <- read.csv(url(strt.url), skip = 1)
frch.gauge <- read.csv(url(frch.url), skip = 1)                       
vaul.gauge <- read.csv(url(vaul.url), skip = 1)


### Rename columns ###
names(strt.gauge) <- c("Site", "DateTimeGMT", "Precip")
names(frch.gauge) <- c("Site", "DateTimeGMT", "Precip")
names(vaul.gauge) <- c("Site", "DateTimeGMT", "Precip")

strt.gauge$Site <- "STRT"
frch.gauge$Site <- "FRCH"
vaul.gauge$Site <- "VAUL"

# Input NA for missing time #
strt.gauge$DateTimeGMT[strt.gauge$DateTimeGMT == ""] <- NA
frch.gauge$DateTimeGMT[frch.gauge$DateTimeGMT == ""] <- NA
vaul.gauge$DateTimeGMT[vaul.gauge$DateTimeGMT == ""] <- NA

# Convert time and put in AK time #
strt.gauge$DateTime <- mdy_hms(strt.gauge$DateTimeGMT, tz = "GMT")
attributes(strt.gauge$DateTime)$tzone <- 'America/Anchorage'

frch.gauge$DateTime <- mdy_hms(frch.gauge$DateTimeGMT, tz = "GMT")
attributes(frch.gauge$DateTime)$tzone <- 'America/Anchorage'

vaul.gauge$DateTime <- mdy_hms(vaul.gauge$DateTimeGMT, tz = "GMT")
attributes(vaul.gauge$DateTime)$tzone <- 'America/Anchorage'


# Plot data #
STRT <- ggplot(strt.gauge) +
  geom_line(aes(x = DateTime, y = Precip)) +
  xlab("Date") +
  ylab("Cumulative Precipitation in mm") +
  ggtitle("Stuart Rain Gauge")

STRT

FRCH <- ggplot(frch.gauge) + 
  geom_line(aes(x = DateTime, y = Precip)) +
  xlab("Date") +
  ylab("Cumulative Precipitation in mm") +
  ggtitle("French Rain Gauge")
FRCH

VAUL <- ggplot(vaul.gauge) +
  geom_line(aes(x = DateTime, y = Precip)) +
  xlab("Date") +
  ylab("Cumulative Precipitation in mm") +
  ggtitle("Vault Rain Gauge")
VAUL


allrain.2020 <- bind_rows(strt.gauge, frch.gauge, vaul.gauge)

allrain.2020$Precip <- allrain$Precip/10

ALL <- ggplot(allrain.2020) +
  geom_line(aes(x = DateTime, y = Precip, color = Site)) +
  xlab("Date") + 
  ylab("Cumulative Precipitation in cm") +
  ggtitle("STRT FRCH VAUL Rain Gauge")
ALL

dir.create("RainGauge")
write.csv(allrain,"RainGauge/allrain_2020.csv", row.names = FALSE)

