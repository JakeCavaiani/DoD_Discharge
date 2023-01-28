### Interpolate missing Q ###
## ~8-day gap most sites 2022
## first half of season 2021 at some sites

## Inputs:
      # Predicted_Q_2022.csv : Q compiled for all sites 2022
      # Q_2022.csv : Q compiled for all sites 2022
    
## Outputs: 

library(here)
library(tidyverse)
library(nlme)
library(broom)

### Data ###
## 2022
Q22 <- read.csv(here("Predicted_Discharge", "2022", "Predicted_Q_2022.csv"))

Q22$DateTimeAK <- as.POSIXct(Q22$DateTimeAK, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
Q22$julian <- sapply(Q22$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))

# remove duplicates
Q22 <- Q22 %>% group_by(Site, DateTimeAK) %>%
  distinct(DateTimeAK, .keep_all = TRUE)
  
## 2021
Q21 <- read.csv(here("Predicted_Discharge", "2021", "Q_2021.csv"))

#Q21fr <- read.csv(here("Predicted_Discharge", "2021", "FRCH", "FRCH.Q.csv"))

#names(Q21fr)[names(Q21fr) == 'Q'] <- 'pred.frch.Q'

## 2019
Q19 <- read.csv(here("Predicted_Discharge", "2019", "Q_2019.csv"))

#####################
### Interpolation ###
#####################
## 2022 ##
# Find sites with missing data
Q22 %>% summarize(across(where(is.numeric), ~sum(is.na(.))))

Q22 %>% ggplot(aes(x = DateTimeAK, y = Q)) +
        geom_point() +
        #xlim(as.POSIXct(c("2022-05-10", "2022-06-10"))) +
        facet_wrap(~Site, scales = "free_y")

# MOOS, POKE, STRT, VAUL missing data
# MOOS: 
  # 2022-07-23 17:00:00, 2022-08-02 16:30:00

# POKE:
  # 2022-07-23 16:55:00, 2022-08-01 15:55:00

# STRT:
  # 2022-07-23 16:55:00, 2022-08-10 22:30:00
  # this can't be correct: Q record beginning again in middle of night

# VAUL:
  # 2022-06-06 10:35, 2022-08-01 10:25

# Align data across sites by datetime
Q22w <- Q22 %>% pivot_wider(names_from = Site, values_from = Q, values_fn = list) %>%
                unnest(cols = everything())

# MOOS
Q22w %>% ggplot(aes(x = FRCH, y = MOOS)) +
  geom_point()
# non-linear relationship

# POKE
Q22w %>% ggplot(aes(x = FRCH, y = POKE)) +
  geom_point()
# non-linear, noisy

# STRT
Q22w %>% ggplot(aes(x = FRCH, y = STRT)) +
  geom_point()
# non-linear, reasonable fit

# VAUL
Q22w %>% ggplot(aes(x = FRCH, y = VAUL)) +
  geom_point()
# no relationship

Q22w %>% ggplot(aes(x = STRT, y = VAUL)) +
  geom_point()
# no relationship during snowmelt

Q22w %>% ggplot(aes(x = POKE, y = VAUL)) +
  geom_point()
# no relationship

Q22w %>% ggplot(aes(x = MOOS, y = VAUL)) +
  geom_point()
# no relationship

## General linear models ##
## MOOS
mod.moos.2 <- Q22w %>%
  gls(MOOS ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin.gls <- Q22w %>% 
  nlme::gls(MOOS ~ FRCH, correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin <- Q22w %>% 
  lm(MOOS ~ FRCH, na.action = na.omit, data = .)

mod.moos.q <- Q22w %>% 
  lm(MOOS ~ FRCH + I(FRCH^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)))

newdat$predgls = predict(mod.moos.2, newdata = newdat)
newdat$predgls.lin = predict(mod.moos.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.moos.lin, newdata = newdat)
newdat$pred.q = predict(mod.moos.q, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = MOOS) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = FRCH, y = MOOS)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red") +
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet")
# try q

# Insert predicted MOOS Q for missing
newdat <- Q22w %>% filter(DateTimeAK >= "2022-07-23 17:00" & DateTimeAK <= "2022-08-02 16:30") %>%
                     select(FRCH)

newdat$MOOS.Q.int <- predict(mod.moos.q, newdata = newdat)
  
Q22w.int <- left_join(Q22w, newdat, by = c("DateTimeAK", "FRCH"))
  
Q22w.int <- Q22w.int %>% mutate(MOOS.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:30:00", MOOS.Q.int, MOOS))

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = MOOS.Q.int)) +
          geom_point(color = "blue") +
          geom_point(aes(x = DateTimeAK, y = MOOS), color = "green")

## POKE ##
Q22w %>% ggplot(aes(x = DateTimeAK, y = POKE)) +
  geom_point() +
  geom_point(aes(y = FRCH), color = "blue")

mod.poke.2 <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  gls(POKE ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin.gls <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  gls(POKE ~ FRCH , correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  lm(POKE ~ FRCH, na.action = na.omit, data = .)

mod.poke.q <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  lm(POKE ~ FRCH + I(FRCH^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)))

newdat$predgls = predict(mod.poke.2, newdata = newdat)
newdat$predgls.lin = predict(mod.poke.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.poke.lin, newdata = newdat)
newdat$pred.q = predict(mod.poke.lin, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = POKE) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = FRCH, y = POKE)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red") +
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet")

# Insert predicted POKE Q for missing
newdat <- Q22w %>% filter(DateTimeAK >= "2022-07-23 17:00" & DateTimeAK <= "2022-08-01 15:45") %>%
  select(FRCH)

newdat$POKE.Q.int <- predict(mod.poke.lin.gls, newdata = newdat)

Q22w.int <- left_join(Q22w.int, newdat, by = c("DateTimeAK", "FRCH"))

Q22w.int <- Q22w.int %>% mutate(POKE.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-01 15:45:00", POKE.Q.int, POKE))

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = POKE.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "green")

# Baseline adjust
Q22w.int <- Q22w.int %>% mutate(POKE.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-01 15:45:00", POKE.Q.int + 17.7098, POKE.Q.int))
     
Q22w.int %>% ggplot(aes(x = DateTimeAK, y = POKE.Q.int)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "green")

# dataframe for drift regression
# dates to start and end drift correction
# observed values before and after drift

PO.dates <- c("2022-07-23 17:00:00", "2022-08-01 15:45:00")
PO.Q <- c(388.7098, 330.4998)

PO.drift <- data.frame(cbind("DateTimeAK" = PO.dates, "poke.Q" = PO.Q))
PO.drift$DateTimeAK <- as.POSIXct(PO.drift$DateTimeAK, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
PO.drift$doy <- sapply(PO.drift$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))
PO.drift <- PO.drift %>% mutate(across(where(is.character), ~as.numeric(.)))

PO.drift$diffQ <- PO.drift$poke.Q - PO.drift$poke.Q[1]

PO.mod.drift <- lm(diffQ ~ doy, data = PO.drift)
PO.Q.sl <- PO.mod.drift$coef[2]

Q22w.int <- Q22w.int %>% 
  mutate(POKE.Q.intb = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-01 15:45:00", POKE.Q.int - ((203.6667 - julian)*PO.Q.sl), POKE.Q.int)) 

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = POKE.Q.intb)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "green")

## STRT
Q22w %>% ggplot(aes(x = DateTimeAK, y = STRT)) +
  geom_point() +
  geom_point(aes(y = FRCH), color = "blue")

mod.strt.2 <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  gls(STRT ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.strt.lin.gls <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  gls(STRT ~ FRCH , correlation = corAR1(), na.action = na.omit, data = .)

mod.strt.lin <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  lm(STRT ~ FRCH, na.action = na.omit, data = .)

mod.strt.q <- Q22w %>% #filter(FRCH < 750 & FRCH > 400) %>%
  lm(STRT ~ FRCH + I(FRCH^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)))

newdat$predgls = predict(mod.strt.2, newdata = newdat)
newdat$predgls.lin = predict(mod.strt.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.strt.lin, newdata = newdat)
newdat$pred.q = predict(mod.strt.q, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = STRT) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = FRCH, y = STRT)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red") +
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "orange")

# Insert predicted STRT Q for missing
newdat <- Q22w %>% filter(DateTimeAK >= "2022-07-23 17:00" & DateTimeAK <= "2022-08-10 22:30") %>%
  select(FRCH)

newdat$STRT.Q.int <- predict(mod.strt.q, newdata = newdat)

Q22w.int <- left_join(Q22w.int, newdat, by = c("DateTimeAK", "FRCH"))

Q22w.int <- Q22w.int %>% mutate(STRT.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-10 22:30:00", STRT.Q.int, STRT))

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = STRT.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = STRT), color = "green")

# Baseline adjust
# dataframe for drift regression
# dates to start and end drift correction
# observed values before and after drift

ST.dates <- c("2022-07-23 17:00:00", "2022-08-10 22:30:00")
ST.Q <- c(972.0982, 806.215)

ST.drift <- data.frame(cbind("DateTimeAK" = ST.dates, "strt.Q" = ST.Q))
ST.drift$DateTimeAK <- as.POSIXct(ST.drift$DateTimeAK, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
ST.drift$doy <- sapply(ST.drift$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))
ST.drift <- ST.drift %>% mutate(across(where(is.character), ~as.numeric(.)))

ST.drift$diffQ <- ST.drift$strt.Q - ST.drift$strt.Q[1]

ST.mod.drift <- lm(diffQ ~ doy, data = ST.drift)
ST.Q.sl <- ST.mod.drift$coef[2]

Q22w.int <- Q22w.int %>% 
  mutate(STRT.Q.intb = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-10 22:30:00", STRT.Q.int - ((221.8958 - julian)*ST.Q.sl), STRT.Q.int)) 

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = STRT.Q.intb)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = STRT), color = "green")

## Clean up
Q22w.int <- Q22w.int %>% select(-c(POKE.Q.int, STRT.Q.int)) %>%
                        rename(STRT.Q.int = STRT.Q.intb) %>%
                        rename(POKE.Q.int = POKE.Q.intb)

## Export
write.csv(Q22w.int, here("Predicted_Discharge", "2022", "Predicted_Q_2022_gapfill.csv"))

## 2021 ##
# Manage dates
Q21$DateTimeAK <- as.POSIXct(Q21$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
Q21 <- Q21 %>% filter(!is.na(DateTimeAK))
Q21$julian <- sapply(Q21$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))

Q21 %>% 
  ggplot(aes(x = DateTimeAK, y = Q)) +
  geom_point() +
  facet_wrap(~Site, scales = "free_y")

## Missing
# FRCH:
# until 2021-06-30 10:45:00

# MOOS: 
# until 2021-06-30 11:45

# POKE:
# begins 2021-05-10 17:00
# 2021-06-23 13:30, 2021-06-29 14:45
# 2021-07-02- 08:15, 2021-07-06 13:15
# 2021-07-10 08:15, 2021-07-13 09:30
# 2021-09-02 01:30, 2021-09-08 14:15

# STRT:
# begins 2021-05-07 17:00
# 2021-06-28 11:15:00, 2021-07-16 12:30
# 2021-07-20 19:15, 2021-07-28 11:30
# 2021-08-02 19:15, 2021-08-03 11:45

# VAUL:
# until 2021-06-30 05:00

Q21w <- Q21 %>% pivot_wider(names_from = Site, values_from = Q, values_fn = list) %>%
  unnest(cols = everything())

# Find best match
Q21w %>% ggplot(aes(x = DateTimeAK, y = POKE)) +
  geom_line(color = "red") +
  geom_line(aes(x = DateTimeAK, y = FRCH), color = "blue") +
  geom_line(aes(x = DateTimeAK, y = MOOS), color = "green") +
  geom_line(aes(x = DateTimeAK, y = STRT), color = "darkblue") +
  geom_line(aes(x = DateTimeAK, y = VAUL), color = "violet") +
  ylim(0, 5000)

### VAUL ###
Q21w %>% ggplot(aes(x = POKE, y = VAUL)) +
  geom_point()
# try POKE

Q21w %>% ggplot(aes(x = STRT, y = VAUL)) +
  geom_point()

## General linear models - VAUL ##
mod.vaul.2 <- Q21w %>% mutate(VAUL = ifelse(VAUL > 250 & POKE < 750, NA, VAUL)) %>%
  gls(VAUL ~ POKE + I(POKE^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.vaul.lin.gls <- Q21w %>% mutate(VAUL = ifelse(VAUL > 250 & POKE < 750, NA, VAUL)) %>%
  nlme::gls(VAUL ~ POKE, correlation = corAR1(), na.action = na.omit, data = .)

mod.vaul.lin <- Q21w %>% mutate(VAUL = ifelse(VAUL > 250 & POKE < 750, NA, VAUL)) %>%
  lm(VAUL ~ POKE, na.action = na.omit, data = .)

mod.vaul.q <- Q21w %>% mutate(VAUL = ifelse(VAUL > 250 & POKE < 750, NA, VAUL)) %>%
  lm(VAUL ~ POKE + I(POKE^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(POKE = runif(100, 150, 1500))

newdat$predgls = predict(mod.vaul.2, newdata = newdat)
newdat$predgls.lin = predict(mod.vaul.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.vaul.lin, newdata = newdat)
newdat$pred.q = predict(mod.vaul.q, newdata = newdat)

ggplot(Q21w, aes(x = POKE, y = VAUL) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = POKE, y = VAUL)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")+
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet") 
# try quad

# Insert predicted VAUL Q for missing
newdat <- Q21w %>% filter(DateTimeAK <= "2021-06-30 05:00:00") %>%
  select(c(DateTimeAK, POKE))

newdat$VAUL.Q.int <- predict(mod.vaul.q, newdata = newdat)

Q21w.int <- left_join(Q21w, newdat, by = c("DateTimeAK", "POKE"))

Q21w.int <- Q21w.int %>% mutate(VAUL.Q.int = ifelse(DateTimeAK <= "2021-06-30 05:00:00", VAUL.Q.int, VAUL))

Q21w.int %>% ggplot(aes(x = DateTimeAK, y = VAUL.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = VAUL), color = "red")

## FRCH ##
Q21w %>% ggplot(aes(x = POKE, y = FRCH)) +
  geom_point()

Q21w %>% ggplot(aes(x = STRT, y = FRCH)) +
  geom_point()

## General linear models - VAUL ##
mod.frch.2 <- Q21w %>% mutate(FRCH = ifelse(FRCH > 600 & STRT < 1500, NA, FRCH)) %>%
  gls(FRCH ~ STRT + I(STRT^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.frch.lin.gls <- Q21w %>% mutate(FRCH = ifelse(FRCH > 600 & STRT < 1500, NA, FRCH)) %>%
  nlme::gls(FRCH ~ STRT, correlation = corAR1(), na.action = na.omit, data = .)

mod.frch.lin <- Q21w %>% mutate(FRCH = ifelse(FRCH > 600 & STRT < 1500, NA, FRCH)) %>%
  lm(FRCH ~ STRT, na.action = na.omit, data = .)

mod.frch.q <- Q21w %>% mutate(FRCH = ifelse(FRCH > 600 & STRT < 1500, NA, FRCH)) %>%
  lm(FRCH ~ STRT + I(STRT^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(STRT = runif(100, 150, 1750))

newdat$predgls = predict(mod.frch.2, newdata = newdat)
newdat$predgls.lin = predict(mod.frch.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.frch.lin, newdata = newdat)
newdat$pred.q = predict(mod.frch.q, newdata = newdat)

ggplot(Q21w, aes(x = STRT, y = FRCH) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = STRT, y = FRCH)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")+
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet") 
# try lin

# Insert predicted VAUL Q for missing
newdat <- Q21w %>% filter(DateTimeAK <= "2021-06-30 10:45:00") %>%
  select(c(DateTimeAK, STRT))

newdat$FRCH.Q.int <- predict(mod.frch.lin, newdata = newdat)

Q21w.int <- left_join(Q21w.int, newdat, by = c("DateTimeAK", "STRT"))

Q21w.int <- Q21w.int %>% mutate(FRCH.Q.int = ifelse(DateTimeAK <= "2021-06-30 10:45:00", FRCH.Q.int, FRCH))

Q21w.int %>% ggplot(aes(x = DateTimeAK, y = FRCH.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = FRCH), color = "red")

## MOOS ##
Q21w %>% ggplot(aes(x = POKE, y = MOOS)) +
  geom_point()

Q21w %>% ggplot(aes(x = STRT, y = MOOS)) +
  geom_point()
# try STRT

## General linear models - MOOS ##
mod.moos.2 <- Q21w %>% 
  gls(MOOS ~ STRT + I(STRT^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin.gls <- Q21w %>% 
  nlme::gls(MOOS ~ STRT, correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin <- Q21w %>% 
  lm(MOOS ~ STRT, na.action = na.omit, data = .)

mod.moos.q <- Q21w %>% 
  lm(MOOS ~ STRT + I(STRT^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(STRT = runif(100, 250, 1750))

newdat$predgls = predict(mod.moos.2, newdata = newdat)
newdat$predgls.lin = predict(mod.moos.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.moos.lin, newdata = newdat)
newdat$pred.q = predict(mod.moos.q, newdata = newdat)

ggplot(Q21w, aes(x = STRT, y = MOOS) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = STRT, y = MOOS)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")+
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet") 
# use lin

# Insert predicted MOOS Q for missing
newdat <- Q21w %>% filter(DateTimeAK <= "2021-06-30 11:45") %>%
  select(c(DateTimeAK, STRT))

newdat$MOOS.Q.int <- predict(mod.moos.q, newdata = newdat)

Q21w.int <- left_join(Q21w.int, newdat, by = c("DateTimeAK", "STRT"))

Q21w.int <- Q21w.int %>% mutate(MOOS.Q.int = ifelse(DateTimeAK <= "2021-06-30 11:45", MOOS.Q.int, MOOS))

Q21w.int %>% ggplot(aes(x = DateTimeAK, y = MOOS.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = MOOS), color = "red")

## STRT ##
Q21w %>% ggplot(aes(x = MOOS, y = STRT)) +
  geom_point()
# use MOOS

Q21w %>% ggplot(aes(x = FRCH, y = STRT)) +
  geom_point()

## General linear models - STRT ##
mod.strt.2 <- Q21w %>% 
  gls(STRT ~ MOOS + I(MOOS^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.strt.lin.gls <- Q21w %>% 
  nlme::gls(STRT ~ MOOS, correlation = corAR1(), na.action = na.omit, data = .)

mod.strt.lin <- Q21w %>% 
  lm(STRT ~ MOOS, na.action = na.omit, data = .)

mod.strt.q <- Q21w %>% 
  lm(STRT ~ MOOS + I(MOOS^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(MOOS = runif(100, 250, 1500))

newdat$predgls = predict(mod.strt.2, newdata = newdat)
newdat$predgls.lin = predict(mod.strt.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.strt.lin, newdata = newdat)
newdat$pred.q = predict(mod.strt.q, newdata = newdat)

ggplot(Q21w, aes(x = MOOS, y = STRT) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = MOOS, y = STRT)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")+
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet") 
# try q

# Insert predicted STRT Q for missing
newdat <- Q21w %>% filter(DateTimeAK >= "2021-06-28 11:15:00" & DateTimeAK <= "2021-07-16 12:30" |
                          DateTimeAK >= "2021-07-20 19:15" & DateTimeAK <= "2021-07-28 11:30" |
                          DateTimeAK >= "2021-08-02 19:15" & DateTimeAK <= "2021-08-03 11:45") %>%
                   select(c(DateTimeAK, MOOS))

newdat$STRT.Q.int <- predict(mod.strt.q, newdata = newdat)

Q21w.int <- left_join(Q21w.int, newdat, by = c("DateTimeAK", "MOOS"))

Q21w.int <- Q21w.int %>% mutate(STRT.Q.int = ifelse(DateTimeAK >= "2021-06-28 11:15:00" & DateTimeAK <= "2021-07-16 12:30" |
                                                    DateTimeAK >= "2021-07-20 19:15" & DateTimeAK <= "2021-07-28 11:30" |
                                                    DateTimeAK >= "2021-08-02 19:15" & DateTimeAK <= "2021-08-03 11:45", STRT.Q.int, STRT))

Q21w.int %>% ggplot(aes(x = DateTimeAK, y = STRT.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = STRT), color = "red")

## POKE ##
Q21w %>% ggplot(aes(x = MOOS, y = POKE)) +
  geom_point()
# try MOOS

Q21w %>% ggplot(aes(x = FRCH, y = POKE)) +
  geom_point()

Q21w %>% ggplot(aes(x = STRT, y = POKE)) +
  geom_point()

## General linear models - POKE ##
mod.poke.2 <- Q21w %>% mutate(POKE = ifelse(POKE < 400 & MOOS > 500, NA, POKE)) %>%
  gls(POKE ~ MOOS + I(MOOS^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin.gls <- Q21w %>% mutate(POKE = ifelse(POKE < 400 & MOOS > 500, NA, POKE)) %>%
  nlme::gls(POKE ~ MOOS, correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin <- Q21w %>% mutate(POKE = ifelse(POKE < 400 & MOOS > 500, NA, POKE)) %>%
  lm(POKE ~ MOOS, na.action = na.omit, data = .)

mod.poke.q <- Q21w %>% mutate(POKE = ifelse(POKE < 400 & MOOS > 500, NA, POKE)) %>%
  lm(POKE ~ MOOS + I(MOOS^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(MOOS = runif(100, 250, 1500))

newdat$predgls = predict(mod.poke.2, newdata = newdat)
newdat$predgls.lin = predict(mod.poke.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.poke.lin, newdata = newdat)
newdat$pred.q = predict(mod.poke.q, newdata = newdat)

ggplot(Q21w, aes(x = MOOS, y = POKE) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = MOOS, y = POKE)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")+
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet") 
# try lin

# Insert predicted POKE Q for missing
newdat <- Q21w %>% filter(DateTimeAK >= "2021-06-23 13:30" & DateTimeAK <= "2021-06-29 14:45" |
                          DateTimeAK >= "2021-07-02- 08:15" & DateTimeAK <= "2021-07-06 13:15" |
                          DateTimeAK >= "2021-07-10 08:15" & DateTimeAK <= "2021-07-13 09:30" |
                          DateTimeAK >= "2021-09-02 01:30" & DateTimeAK <= "2021-09-08 14:15") %>%
                   select(c(DateTimeAK, MOOS))

newdat$POKE.Q.int <- predict(mod.poke.q, newdata = newdat)

Q21w.int <- left_join(Q21w.int, newdat, by = c("DateTimeAK", "MOOS"))

Q21w.int <- Q21w.int %>% mutate(POKE.Q.int = ifelse(DateTimeAK >= "2021-06-23 13:30" & DateTimeAK <= "2021-06-29 14:45" |
                                                      DateTimeAK >= "2021-07-02- 08:15" & DateTimeAK <= "2021-07-06 13:15" |
                                                      DateTimeAK >= "2021-07-10 08:15" & DateTimeAK <= "2021-07-13 09:30" |
                                                      DateTimeAK >= "2021-09-02 01:30" & DateTimeAK <= "2021-09-08 14:15", POKE.Q.int, POKE))

Q21w.int %>% ggplot(aes(x = DateTimeAK, y = POKE.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "red")

#### Remember to export interpolated 2021

## 2019 ##
## Poker
Q19$DateTimeAK <- as.POSIXct(Q19$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
Q19$julian <- sapply(Q19$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))

Q19 %>% 
  ggplot(aes(x = DateTimeAK, y = Q)) +
  geom_point() +
  facet_wrap(~Site, scales = "free_y")

## Missing
# 2019-08-27 00:00:00, 2019-09-08 13:15:00

Q19w <- Q19 %>% pivot_wider(names_from = Site, values_from = Q, values_fn = list) %>%
  unnest(cols = everything())

# Find best match
Q19w %>% ggplot(aes(x = DateTimeAK, y = POKE)) +
          geom_line(color = "red") +
        geom_line(aes(x = DateTimeAK, y = FRCH), color = "blue") +
        geom_line(aes(x = DateTimeAK, y = MOOS), color = "green") +
        geom_line(aes(x = DateTimeAK, y = STRT), color = "darkblue") +
        geom_line(aes(x = DateTimeAK, y = VAUL), color = "violet") +
        ylim(0, 5000)
  
# vs FRCH
Q19w %>% ggplot(aes(x = FRCH, y = POKE)) +
  geom_point()
# no

# vs MOOS
Q19w %>% ggplot(aes(x = MOOS, y = POKE)) +
  geom_point()
# same

# vs STRT
Q19w %>% ggplot(aes(x = STRT, y = POKE)) +
  geom_point()
# same

# vs VAUL
Q19w %>% ggplot(aes(x = VAUL, y = POKE)) +
  geom_point()
# relationship changes in time

## General linear models ##
mod.poke.2 <- Q19w %>% filter(DateTimeAK > "2019-06-15" & DateTimeAK < "2019-09-28") %>%
                       filter(POKE < 1000) %>%
  gls(POKE ~ STRT + I(STRT^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin.gls <- Q19w %>% filter(DateTimeAK > "2019-06-15" & DateTimeAK < "2019-09-28") %>%
                             filter(POKE < 1000) %>%
  nlme::gls(POKE ~ STRT, correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin <- Q19w %>% filter(DateTimeAK > "2019-06-15" & DateTimeAK < "2019-09-28") %>%
                         filter(POKE < 1000) %>%
  lm(POKE ~ STRT, na.action = na.omit, data = .)

mod.poke.q <- Q19w %>% filter(DateTimeAK > "2019-06-15" & DateTimeAK < "2019-09-28") %>%
                       filter(POKE < 1000) %>%
  lm(POKE ~ STRT + I(STRT^2), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(STRT = runif(100, 1200, 6000))

newdat$predgls = predict(mod.poke.2, newdata = newdat)
newdat$predgls.lin = predict(mod.poke.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.poke.lin, newdata = newdat)
newdat$pred.q = predict(mod.poke.q, newdata = newdat)

ggplot(Q19w, aes(x = STRT, y = POKE) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = STRT, y = POKE)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")+
  geom_line(data = newdat, aes(y = pred.q), size = 1, color = "violet") +
  ylim(0, 1000)
# try lm

# Insert predicted POKE Q for missing
newdat <- Q19w %>% filter(DateTimeAK >= "2019-08-27 00:00:00" & DateTimeAK <= "2019-09-08 13:15:00") %>%
  select(c(DateTimeAK, STRT))

newdat$POKE.Q.int <- predict(mod.poke.lin, newdata = newdat)

Q19w.int <- left_join(Q19w, newdat, by = c("DateTimeAK", "STRT"))

Q19w.int <- Q19w.int %>% mutate(POKE.Q.int = ifelse(DateTimeAK >= "2019-08-27 00:00:00" & DateTimeAK <= "2019-09-08 13:15:00", POKE.Q.int, POKE))

Q19w.int %>% ggplot(aes(x = DateTimeAK, y = POKE.Q.int)) +
  geom_point(color = "blue") +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "red")

## Export
write.csv(Q19w.int, here("Predicted_Discharge", "2019", "Predicted_Q_2019_gapfill.csv"))
