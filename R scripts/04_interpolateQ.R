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

## General linear models ##
## MOOS
mod.moos.2 <- Q22w %>%
  gls(MOOS ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin.gls <- Q22w %>% 
  nlme::gls(MOOS ~ FRCH, correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin <- Q22w %>% 
  lm(MOOS ~ FRCH, na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)))

newdat$predgls = predict(mod.moos.2, newdata = newdat)
newdat$predgls.lin = predict(mod.moos.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.moos.lin, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = MOOS) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = FRCH, y = MOOS)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")
# lm is best

# Insert predicted MOOS Q for missing
newdat <- Q22w %>% filter(DateTimeAK >= "2022-07-23 17:00" & DateTimeAK <= "2022-08-02 16:30") %>%
                     select(FRCH)

newdat$MOOS.Q.int <- predict(mod.moos.lin, newdata = newdat)
  
Q22w.int <- left_join(Q22w, newdat, by = c("DateTimeAK", "FRCH"))
  
Q22w.int <- Q22w.int %>% mutate(MOOS.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:30:00", MOOS.Q.int, MOOS))

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = MOOS.Q.int)) +
          geom_point(color = "blue") +
          geom_point(aes(x = DateTimeAK, y = MOOS), color = "green")

## POKE ##
Q22w %>% ggplot(aes(x = DateTimeAK, y = POKE)) +
  geom_point() +
  geom_point(aes(y = FRCH), color = "blue")

mod.poke.2 <- Q22w %>% filter(FRCH < 750 & FRCH > 400) %>%
  gls(POKE ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin.gls <- Q22w %>% filter(FRCH < 750 & FRCH > 400) %>%
  gls(POKE ~ FRCH , correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin <- Q22w %>% filter(FRCH < 750 & FRCH > 400) %>%
  lm(POKE ~ FRCH, na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)))

newdat$predgls = predict(mod.poke.2, newdata = newdat)
newdat$predgls.lin = predict(mod.poke.lin.gls, newdata = newdat)
newdat$pred.lin = predict(mod.poke.lin, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = POKE) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = FRCH, y = POKE)) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1, color = "green") +
  geom_line(data = newdat, aes(y = pred.lin), size = 1, color = "red")

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
                                       
# dataframe for drift regression
# dates to start and end drift correction
# observed values before and after drift

PO.dates <- c("2022-07-23 17:00:00", "2022-08-01 15:45:00")
PO.Q <- c(357.5166, 330.498)

PO.drift <- data.frame(cbind("DateTimeAK" = PO.dates, "poke.Q" = PO.Q))
PO.drift$DateTimeAK <- as.POSIXct(PO.drift$DateTimeAK, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
PO.drift$doy <- sapply(PO.drift$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))
PO.drift <- PO.drift %>% mutate(across(where(is.character), ~as.numeric(.)))

PO.drift$diffQ <- PO.drift$poke.Q - PO.drift$poke.Q[1]

PO.mod.drift <- lm(diffQ ~ doy, data = PO.drift)
PO.Q.sl <- PO.mod.drift$coef[2]

Q22w.int <- Q22w.int %>% 
  mutate(POKE.Q.intb = ifelse(DateTimeAK > "2022-07-23 17:00:00" & DateTimeAK < "2022-08-01 15:45:00", POKE.Q.int - ((212.625 - julian)*PO.Q.sl), POKE.Q.int)) 

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

# Insert predicted POKE Q for missing
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
Q21$julian <- sapply(Q21$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))

Q21 %>% 
  ggplot(aes(x = DateTimeAK, y = Q)) +
  geom_point() +
  facet_wrap(~Site, scales = "free_y")

## Missing
# FRCH:
# 2021-05-04 09:00:00, 2021-06-30 03:00:00

# MOOS: 
# 2021-05-04 09:00:00, 

# POKE:
# 2021-05-04 09:00:00
# shorter gaps throughout

# STRT:
# 2021-06-30 11:25:00, 

# VAUL:
# 2021-05-04 09:00:00

# FRCH
Q21 %>% ggplot(aes(x = log(pred.poke.Q), y = log(pred.frch.Q))) +
  geom_point()

Q21 %>% ggplot(aes(x = log(pred.strt.Q), y = log(pred.frch.Q))) +
  geom_point()
# POKE

# MOOS
Q21 %>% ggplot(aes(x = pred.poke.Q, y = pred.moos.Q)) +
  geom_point()

Q21 %>% ggplot(aes(x = pred.strt.Q, y = pred.moos.Q)) +
  geom_point()
# POKE is better

# STRT
Q21 %>% ggplot(aes(x = pred.poke.Q, y = pred.strt.Q)) +
  geom_point()

Q21 %>% ggplot(aes(x = pred.frch.Q, y = pred.strt.Q)) +
  geom_point()

Q21 %>% ggplot(aes(x = pred.moos.Q, y = pred.strt.Q)) +
  geom_point()
# MOOS?

# VAUL
Q21 %>% ggplot(aes(x = pred.poke.Q, y = pred.vaul.Q)) +
  geom_point()

Q21 %>% ggplot(aes(x = pred.strt.Q, y = pred.vaul.Q)) +
  geom_point()
# POKE?
