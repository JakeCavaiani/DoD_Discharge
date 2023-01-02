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

# Q21d <- read.csv(here("Predicted_Discharge", "2021", "Q.daily.2021.csv"))

Q21fr <- read.csv(here("Predicted_Discharge", "2021", "FRCH", "FRCH.Q.csv"))
Q21mo <- read.csv(here("Predicted_Discharge", "2021", "MOOS", "MOOS.Q.csv"))
Q21po <- read.csv(here("Predicted_Discharge", "2021", "POKE", "POKE.Q.csv"))
Q21st <- read.csv(here("Predicted_Discharge", "2021", "STRT", "STRT.Q.csv"))
Q21va <- read.csv(here("Predicted_Discharge", "2021", "VAUL", "VAUL.Q.csv"))

names(Q21fr)[names(Q21fr) == 'Q'] <- 'pred.frch.Q'
names(Q21mo)[names(Q21mo) == 'Q'] <- 'pred.moos.Q'
names(Q21po)[names(Q21po) == 'Q'] <- 'pred.poke.Q'
names(Q21st)[names(Q21st) == 'Q'] <- 'pred.strt.Q'
names(Q21va)[names(Q21va) == 'Q'] <- 'pred.vaul.Q'

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
  # 2022-07-23 16:55:00, 2022-08-02 16:45:00

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

newdat <- Q22w %>% filter(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:00:00") %>%
                  select(c(FRCH, DateTimeAK)) %>%
                  filter(!is.na(FRCH)) %>%
                  data.frame()

newdat$predgls = predict(mod.moos.2, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = MOOS) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1, color = "blue") +
  geom_point(aes(x = FRCH, y = MOOS))

newdat$predgls.lin = predict(mod.moos.lin, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = MOOS) ) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1) +
  geom_point(aes(x = FRCH, y = MOOS)) +
  geom_smooth(method = "lm", color = "blue")

# Insert predicted MOOS Q for missing
Q22w.int <- left_join(Q22w, newdat, by = c("DateTimeAK", "FRCH"))
  
Q22w.int <- Q22w.int %>% mutate(moos.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:00:00", predgls, MOOS))

Q22w.int %>% ggplot(aes(x = FRCH, y = moos.Q.int)) +
          geom_point()
Q22w.int %>% ggplot(aes(x = DateTimeAK, y = moos.Q.int)) +
          geom_point() +
          geom_point(aes(x = DateTimeAK, y = MOOS), color = "green")

# adjust baseline of interpreted Q
Q22w.int <- Q22w.int %>% mutate(moos.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:00:00", moos.Q.int - 210.65, moos.Q.int))

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = moos.Q.int)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = MOOS), color = "green")

## POKE ##
Q22w %>% ggplot(aes(x = DateTimeAK, y = POKE)) +
  geom_point() +
  geom_point(aes(y = FRCH), color = "blue")
geom_point(aes(x = DateTimeAK, y = pred.poke.Q), color = "green")

mod.poke.2 <- Q22w %>% #filter(pred.frch.Q < 1500) %>%
  gls(POKE ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.poke.lin <- Q22 %>% filter(pred.frch.Q < 750 & pred.frch.Q > 400) %>%
  gls(pred.poke.Q ~ pred.frch.Q , correlation = corAR1(), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)),
                     julian = runif(100, min(Q22w$julian), max(Q22w$julian)))

newdat$predgls.poke = predict(mod.poke.2, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = POKE) ) +
  geom_line(data = newdat, aes(y = predgls.poke), size = 1) +
  geom_point(aes(x = FRCH, y = POKE))

#newdat$predgls.lin.poke = predict(mod.poke.lin, newdata = newdat)

ggplot(Q22w, aes(x = FRCH, y = POKE) ) +
  geom_line(data = newdat, aes(y = predgls.poke), size = 1, color = "blue") +
  #geom_line(data = newdat, aes(y = predgls.lin, color = "blue")) +
  geom_point(aes(x = FRCH, y = POKE))

# Insert predicted POKE Q for missing
newdat <- Q22w %>% filter(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-01 15:15:00") %>%
  select(c(FRCH, DateTimeAK, julian)) %>%
  filter(!is.na(FRCH)) %>%
  data.frame()

newdat$predgls.poke = predict(mod.poke.2, newdata = newdat)
#newdat$predgls.lin.poke = predict(mod.poke.lin, newdata = newdat)

poke.pred <- newdat %>% select(-FRCH, julian)
Q22w.int <- left_join(Q22w.int, poke.pred, by = c("DateTimeAK", "julian"))

Q22w.int <- Q22w.int %>% mutate(poke.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-01 15:15:00", predgls.poke, POKE))

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = poke.Q.int)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "green")

# Baseline adjust
# dataframe for drift regression
# dates to start and end drift correction
# observed values before and after drift
PO.dates <- c("2022-07-23 17:00:00", "2022-08-01 16:00:00")
PO.Q <- c(400.4530, 333.3525)

PO.drift <- data.frame(cbind("DateTimeAK" = PO.dates, "poke.Q" = PO.Q))
PO.drift$DateTimeAK <- as.POSIXct(PO.drift$DateTimeAK, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
PO.drift$doy <- sapply(PO.drift$DateTimeAK, function(x) julian(x, origin = as.POSIXct(paste0(format(x, "%Y"),'-01-01'), tz = 'America/Anchorage')))
PO.drift <- PO.drift %>% mutate(across(where(is.character), ~as.numeric(.)))

PO.drift$diffQ <- PO.drift$poke.Q - PO.drift$poke.Q[1]

PO.mod.drift <- lm(diffQ ~ doy, data = PO.drift)
PO.Q.sl <- PO.mod.drift$coef[2]

Q22w.int <- Q22w.int %>% 
  mutate(poke.Q.int = ifelse(DateTimeAK > "2022-07-23 17:00:00" & DateTimeAK < "2022-08-01 16:00:00", poke.Q.int - ((julian - 276.2500)*PO.Q.sl), poke.Q.int)) 

Q22w.int %>% ggplot(aes(x = DateTimeAK, y = poke.Q.int)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = POKE), color = "green")

### Use linear interpolation for POKE gap. Poor fit to FRCH

## STRT
Q22w %>% ggplot(aes(x = DateTimeAK, y = STRT)) +
  geom_point() +
  geom_point(aes(y = FRCH), color = "blue")

Q22 %>% ggplot(aes(x = pred.frch.Q, y = pred.strt.Q)) +
          geom_point()

mod.strt.2 <- Q22w %>% #filter(FRCH < 1000) %>%
  gls(STRT ~ FRCH + I(FRCH^2), correlation = corAR1(), na.action = na.omit, data = .)

# mod.strt.lin <- Q22 %>% filter(pred.frch.Q < 600 ) %>%
#  gls(pred.strt.Q ~ pred.frch.Q , correlation = corAR1(), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(FRCH = runif(100, min(Q22w$FRCH, na.rm = TRUE), max(Q22w$FRCH, na.rm = TRUE)),
                     julian = runif(100, min(Q22w$julian), max(Q22w$julian)))

newdat$predgls.strt = predict(mod.strt.2, newdata = newdat)

ggplot(Q22, aes(x = FRCH, y = STRT) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1) +
  geom_point(aes(x = FRCH, y = STRT))

#newdat$predgls.lin.strt = predict(mod.strt.lin, newdata = newdat)

#ggplot(Q22, aes(x = pred.frch.Q, y = pred.strt.Q) ) +
#  geom_line(data = newdat, aes(y = predgls.strt), size = 1) +
#  geom_line(data = newdat, aes(y = predgls.lin.strt, color = "blue")) +
#  geom_point(aes(x = pred.frch.Q, y = pred.strt.Q))

# Insert predicted STRT Q for missing
# 2022-07-23 18:00:00, 2022-08-10 22:00:00
newdat <- Q22 %>% filter(DateTimeAK >= "2022-07-23 18:00:00" & DateTimeAK <= "2022-08-10 22:00:00") %>%
  select(c(pred.frch.Q, DateTimeAK, julian)) %>%
  filter(!is.na(pred.frch.Q)) %>%
  data.frame()

newdat$predgls.strt = predict(mod.strt.2, newdata = newdat)
newdat$predgls.lin.strt = predict(mod.strt.lin, newdata = newdat)

strt.pred <- newdat %>% select(-pred.frch.Q, julian)
Q22.pred <- left_join(Q22.pred, strt.pred, by = c("DateTimeAK", "julian"))

Q22.pred <- Q22.pred %>% mutate(strt.Q.int = ifelse(DateTimeAK >= "2022-07-23 18:00:00" & DateTimeAK <= "2022-08-10 22:00:00", predgls.strt, pred.strt.Q))

Q22.pred %>% ggplot(aes(x = DateTimeAK, y = strt.Q.int)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = pred.strt.Q), color = "blue")

Q22.pred %>% ggplot(aes(x = DateTimeAK, y = strt.Q.int)) +
  geom_point() +
  geom_point(aes(x = DateTimeAK, y = pred.strt.Q), color = "blue") +
  scale_x_datetime(limits = as.POSIXct(c("2022-07-01", "2022-08-15"))) +
  ylim(c(500, 1500))
# Possibly raise interpolated piece to match

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
