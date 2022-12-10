### Interpolate missing Q ###
## ~8-day gap most sites 2022
## first half of season 2021 at some sites

## Inputs:
      # X.csv : Q compiled for all sites each year
    
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

## 2021
Q21 <- read.csv(here("Predicted_Discharge", "2021", "Q_2021.csv"))

Q21d <- read.csv(here("Predicted_Discharge", "2021", "Q.daily.2021.csv"))

Q21fr <- read.csv(here("Predicted_Discharge", "2021", "FRCH", "FRCH.Q.csv"))

#####################
### Interpolation ###
#####################
## 2022 ##
# Find sites with missing data
Q22 %>% summarize(across(where(is.numeric), ~sum(is.na(.))))

Q22 %>% pivot_longer(pred.vaul.Q:pred.frch.Q) %>%
        ggplot(aes(x = DateTimeAK, y = value)) +
        geom_point() +
        facet_wrap(~name, scales = "free_y")

# MOOS, POKE, STRT, VAUL missing data
# MOOS: 
  # 2022-07-23 17:00:00, 2022-08-02 16:00:00

# POKE:
  # 2022-07-23 17:00:00, 2022-08-01 16:00:00

# STRT:
  # 2022-07-23 18:00:00, 2022-08-10 22:00:00

# MOOS
Q22 %>% ggplot(aes(x = pred.frch.Q, y = pred.moos.Q)) +
  geom_point()
# potential non-linear relationship > 2000 L/s @ MOOS; linear relationship < 1500 L/s MOOS

Q22 %>% ggplot(aes(x = log(pred.frch.Q), y = log(pred.moos.Q))) +
  geom_point()

# POKE
Q22 %>% ggplot(aes(x = pred.frch.Q, y = pred.poke.Q)) +
  geom_point()
# non-linear- most reliable ~400-600 L/s @ POKE (this part might be linear)

# STRT
Q22 %>% ggplot(aes(x = pred.frch.Q, y = pred.strt.Q)) +
  geom_point()
# non-linear, reasonable fit. Strongest > 1000 L/s STRT

# VAUL
Q22 %>% ggplot(aes(x = pred.frch.Q, y = pred.vaul.Q)) +
  geom_point()
# no relationship

## General linear models ##
mod.moos.2 <- Q22 %>% filter(pred.moos.Q < 2500) %>%
  gls(pred.moos.Q ~ pred.frch.Q + I(pred.frch.Q^2), correlation = corAR1(), na.action = na.omit, data = .)

mod.moos.lin <- Q22 %>% filter(pred.moos.Q < 2000) %>%
  gls(pred.moos.Q ~ pred.frch.Q, correlation = corAR1(), na.action = na.omit, data = .)

# Visualize fits
newdat <- data.frame(pred.frch.Q = runif(100, min(Q22$pred.frch.Q, na.rm = TRUE), max(Q22$pred.frch.Q, na.rm = TRUE)))

newdat <- Q22 %>% filter(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:00:00") %>%
                  select(c(pred.frch.Q, DateTimeAK)) %>%
                  filter(!is.na(pred.frch.Q)) %>%
                  data.frame()

newdat$predgls = predict(mod.moos.2, newdata = newdat)

ggplot(Q22, aes(x = pred.frch.Q, y = pred.moos.Q) ) +
  geom_line(data = newdat, aes(y = predgls), size = 1) +
  geom_point(aes(x = pred.frch.Q, y = pred.moos.Q))

newdat$predgls.lin = predict(mod.moos.lin, newdata = newdat)

ggplot(Q22, aes(x = pred.frch.Q, y = pred.moos.Q) ) +
  geom_line(data = newdat, aes(y = predgls.lin), size = 1) +
  geom_point(aes(x = pred.frch.Q, y = pred.moos.Q)) +
  geom_smooth(method = "lm", color = "blue")

# Insert predicted MOOS Q for missing
Q22 <- left_join(Q22, newdat, by = "DateTimeAK")
  
Q22 <- Q22 %>% mutate(moos.Q.int = ifelse(DateTimeAK >= "2022-07-23 17:00:00" & DateTimeAK <= "2022-08-02 16:00:00", predgls.lin, pred.moos.Q))

Q22 %>% ggplot(aes(x = pred.frch.Q.x, y = moos.Q.int)) +
          geom_point()
Q22 %>% ggplot(aes(x = DateTimeAK, y = moos.Q.int)) +
          geom_point() +
          geom_point(aes(x = DateTimeAK, y = pred.moos.Q, color = "green"))
