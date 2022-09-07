### Read me ###
# the purpose of this script is to generate predicted discharge from rating curves generated from discrete observed discharge measurements 


# Step 1: input rating curves for each site
# Step 2: Generate predicted discharge values 
# Step 3: output final csv with datetime in AK time, predicted discharge of PT1, PT2 and average between the two in L/sec

# Load packages #
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



dir.create(here("Predicted_Discharge"))
dir.create(here("Predicted_Discharge", "2019"))
dir.create(here("Predicted_Discharge", "2019", "FRCH"))
dir.create(here("Predicted_Discharge", "2019", "VAUL"))
dir.create(here("Predicted_Discharge", "2019", "POKE"))
dir.create(here("Predicted_Discharge", "2019", "STRT"))
dir.create(here("Predicted_Discharge", "2019", "MOOS"))
################################################## 2015 ######################################################
### FRCH ###
# PT1 #
French1comb$pred.french1.Q <- coef(French1.lm)[2] * French1comb$AbsolutePressure+ coef(French1.lm)[1]
ggplot(aes(x = DateTime, y = pred.french1.Q), data=French1comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q..L.s.), size=3) +
  theme_classic() +
  ggtitle("French1 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") 


# Final Discharge # 
frch.final.discharge <- data.frame(French1comb$site, French1comb$DateTime, French1comb$pred.french1.Q)

### MOOS ### 
# PT1 #
Moose1comb$pred.moos1.Q <- coef(MOOS1.lm)[2] * Moose1comb$AbsolutePressure+ coef(MOOS1.lm)[1]
ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q..L.s.), size=3) +
  theme_classic() +
  ggtitle("Moose1 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") 


# Final Discharge #
moos.final.discharge <- data.frame(Moose1comb$site, Moose1comb$DateTime, Moose1comb$pred.moos1.Q)



### Rename Columns ###
names(frch.final.discharge) <- c("Site", "DateTime", "MeanDischarge")
names(moos.final.discharge) <- c("Site", "DateTime", "MeanDischarge")

frch.final.discharge$Site <- "FRCH"
moos.final.discharge$Site <- "MOOS"
### Write CSV ### 
write.csv(frch.final.discharge,"~/Documents/DoD_Discharge/Predicted_Discharge/2015/FRCH/FRCH.Q.csv", row.names = FALSE)
write.csv(moos.final.discharge,"~/Documents/DoD_Discharge/Predicted_Discharge/2015/MOOS/MOOS.Q.csv", row.names = FALSE)


moos.final.discharge <- na.omit(moos.final.discharge) # Removed 11 rows
frch.final.discharge <- na.omit(frch.final.discharge) # Removed 31 rows


final_discharge_2015 <- rbind(frch.final.discharge, moos.final.discharge)

write.csv(final_discharge_2015,"~/Documents/DoD_Discharge/Predicted_Discharge/2015/FrMo.Q.csv", row.names = FALSE)

Q_2015 <- final_discharge_2015
Q_2015$day = format(as.POSIXct(Q_2015$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
Q_2015$day = as.POSIXct(Q_2015$day, "%Y-%m-%d", tz="America/Anchorage")
#Q_2015$DateTime = NULL

write.csv(Q_2015, "~/Documents/DoD_Discharge/Predicted_Discharge/2015/Q_2015.csv")

Q.daily.2015 = with(Q_2015, tapply(MeanDischarge, list(day, Site), mean))
Q.daily.2015 = as.data.frame(Q.daily.2015)
write.csv(Q.daily.2015, "~/Documents/DoD_Discharge/Predicted_Discharge/2015/Q.daily.2015.csv")

################################### 2018 ###################################################################
# MOOS 1 #
moose1comb$pred.moose1.Q <- coef(Moose1.lm)[2] * moose1comb$WaterLevelmeters + coef(Moose1.lm)[1]
ggplot(aes(x=DateTime, y=pred.moose1.Q), data=moose1comb) +
  geom_line(color="#A6CEE3") +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls), size=3) +
  theme_classic() +
  ggtitle("Moose1 predicted all measured Q") 
moose1comb <- moose1comb %>% 
  mutate(Q_Ls = pred.moose1.Q)


# MOOS 2 # 
moose2comb$pred.moose2.Q <- coef(Moose2.lm)[2] * moose2comb$WaterLevelmeters + coef(Moose2.lm)[1]
ggplot(aes(x=DateTime, y=pred.moose2.Q), data=moose2comb) +
  geom_line(color="#1F78B4") +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls, shape=Method), size=3) +
  theme_classic() +
  ggtitle("Moose2 predicted all measured Q") 

# Moose 1 & 2 comparison
ggplot(aes(x=DateTime, y=pred.moose1.Q), data=moose1comb) +
  geom_line(aes(x=DateTime, y=pred.moose1.Q), data=moose1comb, color="#A6CEE3") +
  geom_line(aes(x=DateTime, y=pred.moose2.Q), data=moose2comb,color="#1F78B4") +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls), size=3) +
  theme_classic() +
  ggtitle("Moose1(light) & Moose2(dark) predicted all measured Q") 

moose2comb <- moose2comb[-c(1:2), ]
moos.final.discharge.2018 <- data.frame(moose2comb$Site, moose2comb$DateTime, moose1comb$pred.moose1.Q, moose2comb$pred.moose2.Q)
moos.final.discharge.2018$MeanDischarge <- rowMeans(moos.final.discharge.2018[,c(3:4)], na.rm = TRUE)
names(moos.final.discharge.2018) <- c("Site", "DateTime", "PT1Q", "PT2Q", "MeanDischarge")
moos.final.discharge.2018$Site <- "MOOS"



# FRCH PT 1 #
French1comb$pred.french1.Q <- coef(French1.lm)[2] * French1comb$WaterLevelmeters + coef(French1.lm)[1]
ggplot(aes(x=DateTime, y=pred.french1.Q), data=French1comb) +
  geom_point(color="#A6CEE3") +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls, shape=Method), size=3) +
  theme_classic() +
  ggtitle("French1 predicted all measured Q") 
French1comb <- French1comb %>% 
  mutate(Q_Ls = pred.french1.Q)

# FRCH PT 2 # 
French2comb$pred.french2.Q <- coef(French2.lm)[2] * French2comb$WaterLevelmeters + coef(French2.lm)[1]
ggplot(aes(x=DateTime, y=pred.french2.Q), data=French2comb) +
  geom_point(color="#1F78B4") +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls, shape=Method), size=3) +
  theme_classic() +
  ggtitle("French2 predicted all measured Q") 

ggplot(aes(x=DateTime, y=pred.french1.Q), data=French1comb) +
  geom_line(aes(x=DateTime, y=pred.french1.Q), data=French1comb, color="#A6CEE3") +
  geom_line(aes(x=DateTime, y=pred.french2.Q), data=French2comb,color="#1F78B4") +
  geom_point(aes(x=DateTime, y=MeasuredQ_Ls), size=3) +
  theme_classic() +
  ggtitle("French1(light) & French2(dark) predicted all measured Q") 

frch.final.discharge.2018 <- data.frame(French2comb$Site, French2comb$DateTime, French1comb$pred.french1.Q, French2comb$pred.french2.Q)
frch.final.discharge.2018$MeanDischarge <- rowMeans(frch.final.discharge.2018[,c(3:4)], na.rm = TRUE)
names(frch.final.discharge.2018) <- c("Site", "DateTime", "PT1Q", "PT2Q", "MeanDischarge")
frch.final.discharge.2018$Site <- "FRCH"

write.csv(moos.final.discharge.2018, "~/Documents/DoD_Discharge/Predicted_Discharge/2018/MOOS/final_moos_Q.csv", row.names = FALSE)
write.csv(frch.final.discharge.2018, "~/Documents/DoD_Discharge/Predicted_Discharge/2018/FRCH/final_frch_Q.csv", row.names = FALSE)

Q_2018 <- rbind(frch.final.discharge.2018,
                moos.final.discharge.2018)
Q_2018$day = format(as.POSIXct(Q_2018$DateTime, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
Q_2018$day = as.POSIXct(Q_2018$day, "%Y-%m-%d", tz = "America/Anchorage")
write.csv(Q_2018, "~/Documents/DoD_Discharge/Predicted_Discharge/2018/Q_2018.csv", row.names = FALSE)

#Q_2019$DateTime = NULL

Q.daily.2018 = with(Q_2018, tapply(MeanDischarge, list(day, Site), mean))
Q.daily.2018 = as.data.frame(Q.daily.2018)
write.csv(Q.daily.2018, "~/Documents/DoD_Discharge/Predicted_Discharge/2018/Q.daily.2018.csv", row.names = FALSE)

################################################# 2019 ######################################################
# FRCH # 
# Predicted Discharge
French2comb.2019$pred.french2.Q <- coef(French2.lm.2019)[2] * French2comb.2019$AbsPTDepth+ coef(French2.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.french2.Q), data=French2comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("French") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  ylim(0, 5000) +
  scale_x_datetime(limits = as_datetime(c("2019-04-29", "2019-10-10")))

# Final FRCH # 
frch.final.discharge.2019 <- data.frame(French2comb.2019$Site, French2comb.2019$DateTime, French1comb.2019$pred.french1.Q, French2comb.2019$pred.french2.Q)
frch.final.discharge.2019$MeanDischarge <- rowMeans(frch.final.discharge.2019[,c(3:4)], na.rm = TRUE)
names(frch.final.discharge.2019) <- c("Site", "DateTime", "PT1Q", "PT2Q", "MeanDischarge")

ggplot(aes(x = DateTime, y = pred.french1.Q), data = French1comb.2019) +
  geom_line(aes(x = DateTime, y = pred.french1.Q), data = French1comb.2019, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.french2.Q), data = French2comb.2019,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = frch.final.discharge.2019, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = Q_Ls), size=2) +
  theme_classic() +
  ylim(0, 5000) +
  ggtitle("Frecnh1(light) French2(dark) & MeanDischarge (red) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

# Write CSV # 
write.csv(frch.final.discharge.2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/FRCH/final_frch_Q.csv", row.names = FALSE)

# VAUL #
# Predicted Discharge
Vaultcomb.2019$pred.vault.Q <- coef(Vault.lm.2019)[1] * Vaultcomb.2019$AbsPTDepth
ggplot(aes(x = DateTime, y = pred.vault.Q), data = Vaultcomb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vault") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))

# Final VAUL # 
vaul.final.discharge.2019 <- data.frame(Vaultcomb.2019$Site, Vaultcomb.2019$DateTime, Vaultcomb.2019$pred.vault.Q)
names(vaul.final.discharge.2019) <- c("Site", "DateTime", "MeanDischarge")

ggplot(aes(x = DateTime, y = pred.vault.Q), data = Vaultcomb.2019) +
  geom_line(aes(x = DateTime, y = pred.vault.Q), data = Vaultcomb.2019, color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls), size=2) +
  theme_classic() +
  ggtitle("MeanDischarge (blue) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

# Write CSV # 
write.csv(vaul.final.discharge.2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/VAUL/final_vaul_Q.csv", row.names = FALSE)

# POKE #
Poker2comb.2019$pred.poke2.Q <- coef(Poker2.lm.2019)[2] * Poker2comb.2019$AbsPTDepth+ coef(Poker2.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.poke2.Q), data=Poker2comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker2 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") +
  ylim(0, 2000)


# Final POKE # 
Poker1comb.2019[c(10118:11617, 14987:15440), 13] <- NA # Remove beaver dam data
Poker2comb.2019[c(9938:11227, 14631:15060), 13] <- NA # Remove beaver dam data

poke.final.discharge.2019 <- left_join(Poker1comb.2019, Poker2comb.2019, by = c("DateTime"))
poke.final.discharge.2019$MeanDischarge <- rowMeans(poke.final.discharge.2019[,c(13,25)], na.rm = TRUE)
poke.final.discharge.2019 <- poke.final.discharge.2019[,-c(3:12, 14:24)] # cleaning empty columns
names(poke.final.discharge.2019) <- c("Site", "DateTime", "PT1", "PT2", "MeanDischarge")
poke.final.discharge.2019$DateTime <- as.POSIXct(paste(poke.final.discharge.2019$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.final.discharge.2019$DateTime <- lubridate::round_date(poke.final.discharge.2019$DateTime, "15 minutes")
poke.final.discharge.2019 <- poke.final.discharge.2019 %>% subset(poke.final.discharge.2019$DateTime < "2019-10-10") # removed on 10/18

ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poker1comb.2019) +
  geom_line(aes(x = DateTime, y = pred.poke1.Q), data = Poker1comb.2019, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.poke2.Q), data = Poker2comb.2019,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = poke.final.discharge.2019, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = Q_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Poker1(light) Poker2(dark) & MeanDischarge (red) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

# Write CSV # 
write.csv(poke.final.discharge.2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/POKE/final_poke_Q.csv", row.names = FALSE)
ggplot(poke.final.discharge.2019) +
  geom_line(aes(x = DateTime, y = MeanDischarge)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("Poker")

# STRT #
Stuart1comb.2019$pred.stuart1.Q <- coef(Stuart1.lm.2019)[2] * Stuart1comb.2019$AbsPTDepth+ coef(Stuart1.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.stuart1.Q), data=Stuart1comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  ylim(0, 10000) +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))

Stuart2comb.2019$pred.stuart2.Q <- coef(Stuart2.lm.2019)[2] * Stuart2comb.2019$AbsPTDepth+ coef(Stuart2.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.stuart2.Q), data=Stuart2comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  ylim(0, 10000) +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))

# Final STRT # 
strt.final.discharge.2019 <- left_join(Stuart1comb.2019, Stuart2comb.2019, by = c("DateTime"))
strt.final.discharge.2019$MeanDischarge <- rowMeans(strt.final.discharge.2019[,c(13,25)], na.rm = TRUE)
strt.final.discharge.2019 <- strt.final.discharge.2019[,-c(3:12, 14:24)] # cleaning empty columns
strt.final.discharge.2019 <- na.omit(strt.final.discharge.2019) # Remove data points that after removal
names(strt.final.discharge.2019) <- c("Site", "DateTime", "PT1", "PT2", "MeanDischarge")

strt.final.discharge.2019$DateTime <- as.POSIXct(paste(strt.final.discharge.2019$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.final.discharge.2019$DateTime <- lubridate::round_date(strt.final.discharge.2019$DateTime, "15 minutes")
poke.final.discharge.2019 <- poke.final.discharge.2019 %>% subset(poke.final.discharge.2019$DateTime < "2019-10-10") # removed on 10/18

ggplot(aes(x = DateTime, y = pred.stuart2.Q), data = Stuart2comb.2019) +
  geom_line(aes(x = DateTime, y = pred.stuart1.Q), data = Stuart1comb.2019, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.stuart2.Q), data = Stuart2comb.2019,color="red", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = strt.final.discharge.2019, color = "#1F78B4", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = Q_Ls), size=2) +
  theme_classic() +
  ylim(0, 10000) +
  ggtitle("Stuart1(light) Stuart2(dark) & MeanDischarge (red) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")


Stuart2comb.end.date <- Stuart2comb.2019 %>% subset(Stuart2comb.2019$DateTime > "2019-08-23 13:45:00") # the dataset that strt 1 does not have (the rest of the summer)
Stuart2comb.end.date <- Stuart2comb.end.date[,-c(3:12)]
strt.final.discharge.2019 <- strt.final.discharge.2019[,-c(3:4)] # simplify so i can bind rows
names(Stuart2comb.end.date) <- c("Site", "DateTime", "MeanDischarge")
strt.final.discharge.2019 <- rbind(strt.final.discharge.2019, Stuart2comb.end.date)

ggplot(strt.final.discharge.2019) +
  geom_line(aes(x = DateTime, y = MeanDischarge)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("Stuart")
# Write CSV # 
write.csv(strt.final.discharge.2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/STRT/final_strt_Q.csv", row.names = FALSE)

# MOOS #
Moose1comb.2019$pred.moose1.Q <- coef(Moose1.lm.2019)[2] * Moose1comb.2019$AbsPTDepth+ coef(Moose1.lm.2019)[1]
ggplot(aes(x = DateTime, y = pred.moose1.Q), data=Moose1comb.2019) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = Q_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose") +
  scale_shape_discrete(name = "Method", label = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab(" Discharge (L/s)") +
  ylim(0, 4000) +
  scale_x_datetime(limits = as_datetime(c("2019-05-15", "2019-10-10")))

# Final MOOS # 
moos.final.discharge.2019 <- data.frame(Moose1comb.2019$Site, Moose1comb.2019$DateTime, Moose1comb.2019$pred.moose1.Q)
names(moos.final.discharge.2019) <- c("Site", "DateTime", "MeanDischarge")

ggplot(moos.final.discharge.2019) +
  geom_line(aes(x = DateTime, y = MeanDischarge)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("Moose")

# Write CSV # 
write.csv(moos.final.discharge.2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/MOOS/final_moos_Q.csv", row.names = FALSE)
frch.final.discharge.2019 <- frch.final.discharge.2019[, -c(3:4)] # just leaving mean discharge after checking that they make sense 
poke.final.discharge.2019 <- poke.final.discharge.2019[, -c(3:4)]# just leaving mean discharge after checking that they make sense 


Q_2019 <- rbind(frch.final.discharge.2019, vaul.final.discharge.2019,
                poke.final.discharge.2019, strt.final.discharge.2019,
                moos.final.discharge.2019)
Q_2019$day = format(as.POSIXct(Q_2019$DateTime, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
Q_2019$day = as.POSIXct(Q_2019$day, "%Y-%m-%d", tz = "America/Anchorage")
write.csv(Q_2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/Q_2019.csv", row.names = FALSE)

#Q_2019$DateTime = NULL

Q.daily.2019 = with(Q_2019, tapply(MeanDischarge, list(day, Site), mean))
Q.daily.2019 = as.data.frame(Q.daily.2019)
write.csv(Q.daily.2019, "~/Documents/DoD_Discharge/Predicted_Discharge/2019/Q.daily.2019.csv", row.names = FALSE)

###
Q_2020 <- all.discharge.2020
Q_2020$day = format(as.POSIXct(Q_2020$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
Q_2020$day = as.POSIXct(Q_2020$day, "%Y-%m-%d", tz="America/Anchorage")


Q.daily.2020 = with(Q_2020, tapply(MeanDischarge, list(day, Site), mean))
Q.daily.2020 = as.data.frame(Q.daily.2020)
################################### 2020 ##################################################
### FRCH ###
# PT1 #
French1comb.2020$pred.french1.Q <- coef(French1.lm.2020)[2] * French1comb.2020$WaterLevel+ coef(French1.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.french1.Q), data=French1comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  ggtitle("French1 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") 
# PT2 #
French2comb.2020$pred.french2.Q <- coef(French2.lm.2020)[2] * French2comb.2020$WaterLevel+ coef(French2.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.french2.Q), data=French2comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  ggtitle("French") +
  xlab("") +
  ylab("Discharge (L/s)") 

# Final Discharge # 
French1comb.2020$DateTimeGMT <- mdy_hms(French1comb.2020$DateTimeGMT, tz = "GMT")
attributes(French1comb.2020$DateTimeGMT)$tzone <- 'America/Anchorage' # Changing GMT to AK time to prep to remove pred Q for PT1


French1comb.2020[c(1039:2382), 13] <- NA # Setting NA to noisy part of the data set
frch.final.discharge.2020 <- data.frame(French1comb.2020$Site, French2comb.2020$DateTime, French1comb.2020$pred.french1.Q, French2comb.2020$pred.french2.Q)

frch.final.discharge.2020$MeanDischarge <- rowMeans(frch.final.discharge.2020[,c ('French1comb.2020.pred.french1.Q', 'French2comb.2020.pred.french2.Q')], na.rm = TRUE) 

frch.final.discharge.2020 <- frch.final.discharge.2020[,-(3:4)] # Just mean discharge because it looks fine
frch.final.discharge.2020<- frch.final.discharge.2020[-c(1:2), ] # Removing errant points
frch.final.discharge.2020<- frch.final.discharge.2020[-c(1:2), ] # Removing errant points

names(frch.final.discharge.2020) <- c("Site", "DateTime", "MeanDischarge")
### French1 (light blue), French2 (dark blue), and mean (red) with observed Q.
ggplot(aes(x = DateTime, y = pred.french2.Q), data = French2comb.2020) +
  geom_line(aes(x = DateTime, y = pred.french1.Q), data = French1comb.2020, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.french2.Q), data = French2comb.2020,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = frch.final.discharge.2020, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ggtitle("French1(light) & French2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

ggplot(frch.final.discharge.2020) +
  geom_line(aes(x = DateTime, y = MeanDischarge)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("French")

### MOOS ### 
# PT1 #
Moose1comb.2020$pred.moos1.Q <- coef(MOOS1.lm.2020)[2] * Moose1comb.2020$WaterLevel+ coef(MOOS1.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose1 predicted all measured Q") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("Date") +
  ylab("Predicted Discharge") 

# PT2 # 
Moose2comb.2020$pred.moos2.Q <- coef(MOOS2.lm.2020)[2] * Moose2comb.2020$WaterLevel+ coef(MOOS2.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.moos2.Q), data = Moose2comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") 

# Final Discharge # 
Moose1comb.2020[c(540:1594), 13] <- NA # Remove the noisy data

moos.final.discharge.2020 <- full_join(Moose1comb.2020, Moose2comb.2020, by = c("DateTime"))
moos.final.discharge.2020$MeanDischarge <- rowMeans(moos.final.discharge.2020[,c(14,27)], na.rm = TRUE)

moos.final.discharge.2020 <- moos.final.discharge.2020[,-c(2:4, 6:13, 15:26)]
moos.final.discharge.2020 <- moos.final.discharge.2020[,-(3:4)]

### Moose1 (light blue), Moose2 (dark blue), and mean (red) with observed Q.

ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb.2020) +
  geom_line(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb.2020, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.moos2.Q), data = Moose2comb.2020,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = moos.final.discharge.2020, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3500) +
  ggtitle("Moose1(light) & Moose2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

ggplot(moos.final.discharge.2020) +
  geom_line(aes(x = DateTime, y = MeanDischarge)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("French")


### POKE ###
# PT1 #
Poke1comb.2020$pred.poke1.Q <- coef(POKE1.lm.2020)[2] * Poke1comb.2020$WaterLevel+ coef(POKE1.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker1 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") 

# PT2 #
Poke2comb.2020$pred.poke2.Q <- coef(POKE2.lm.2020)[2] * Poke2comb.2020$WaterLevel+ coef(POKE2.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.poke2.Q), data = Poke2comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylim(0, 3500) +
  ylab("Discharge(L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))

# POKE 
poke.final.discharge.2020 <- data.frame(Poke1comb.2020$Site, Poke1comb.2020$DateTime, Poke1comb.2020$pred.poke1.Q, Poke2comb.2020$pred.poke2.Q)
poke.final.discharge.2020$MeanDischarge <- rowMeans(poke.final.discharge.2020[,c('Poke1comb.2020.pred.poke1.Q', 'Poke2comb.2020.pred.poke2.Q')], na.rm = TRUE) 


poke.final.discharge.2020[9800, 3] - poke.final.discharge.2020[9801, 3] #454.4731 is the difference 

poke.final.before <- poke.final.discharge.2020[-c(9799:12269), ] # before
poke.final.after <- poke.final.discharge.2020[-c(1:9800), ] # after
poke.final.after$MeanDischarge <- poke.final.after[, 3] + 454.4731
poke.final.discharge.2020 <- full_join(poke.final.before, poke.final.after)

#poke.pt.2020$MeanWL[poke.pt.2020$DateTime == "2020-09-14 06:30"] <- NA


poke.final.discharge.2019$MeanDischarge <- rowMeans(poke.final.discharge.2019[,c('Poke1comb.pred.poke1.Q', 'Poke2comb.pred.poke2.Q')], na.rm = TRUE) 

poke.final.discharge.2020 <- poke.final.discharge.2020[,-(3:4)]


# Poker1 (light blue) and Poker2 (dark blue) with observed Q.
ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb.2020) +
  geom_line(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb.2020, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.poke2.Q), data = Poke2comb.2020,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = Poke1comb.2020.DateTime, y = MeanDischarge), data = poke.final.discharge.2020, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Poker1(light) & Poker2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

plot(poke.final.discharge.2020$MeanDischarge ~ poke.final.discharge.2020$Poke1comb.2020.DateTime, type="l",
     xlim = as.POSIXct(c("2020-09-14 0:00:00","2020-09-14 10:45:00"), tz="America/Anchorage"))
     

ggplot(poke.final.discharge.2020) +
  geom_line(aes(x = Poke1comb.2020.DateTime, y = MeanDischarge)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("Poker")


### STRT ###
# PT1 #
Strt1comb.2020$pred.strt1.Q <- coef(STRT1.lm.2020)[2] * Strt1comb.2020$WaterLevel+ coef(STRT1.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-09")))

# PT2 #
Strt2comb.2020$pred.strt2.Q <- coef(STRT2.lm.2020)[2] * Strt2comb.2020$WaterLevel+ coef(STRT2.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart2 predicted all measured Q") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("Date") +
  ylab("Predicted Discharge") 

# Final STRT # 
strt.final.discharge.2020 <- data.frame(Strt1comb.2020$Site, Strt1comb.2020$DateTime, Strt1comb.2020$pred.strt1.Q)

strt.final.discharge.2020 <- strt.final.discharge.2020 %>% subset(strt.stream.one.2020$DateTime > "2020-06-16")


### Stuart1 (light blue), Stuart2 (dark blue), and Stuart1 (red) because STRT2 seems bad with observed Q.

ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb.2020) +
  geom_line(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb.2020, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb.2020,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = Strt1comb.2020.DateTime, y = Strt1comb.2020.pred.strt1.Q), data = strt.final.discharge.2020, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Stuart1(light) & Stuart2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

ggplot(strt.final.discharge.2020) +
  geom_line(aes(x = Strt1comb.2020.DateTime, y = Strt1comb.2020.pred.strt1.Q)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("Stuart")


### VAUL ###
# PT2 # 
Vaul2comb.2020$pred.vaul2.Q <- coef(VAUL2.lm.2020)[2] * Vaul2comb.2020$WaterLevel+ coef(VAUL2.lm.2020)[1]
ggplot(aes(x = DateTime, y = pred.vaul2.Q), data = Vaul2comb.2020) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vault") +
  scale_shape_discrete(name = "Mehtod", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))

# Final VAUL #
vaul.final.discharge.2020 <- data.frame(Vaul2comb.2020$Site, Vaul2comb.2020$DateTime, Vaul2comb.2020$pred.vaul2.Q)

vaul.final.discharge.2020 <- vaul.final.discharge.2020 %>%  subset(vaul.stream.2020$DateTime > "2020-06-16")

ggplot(vaul.final.discharge.2020) +
  geom_line(aes(x = Vaul2comb.2020.DateTime, y = Vaul2comb.2020.pred.vaul2.Q)) +
  theme_classic() +
  ylab("Discharge (L/s)") +
  xlab("Date") +
  ggtitle("Vault")


setwd(here())
# check: should be at DoD_Discharge
getwd()

### Rename Columns ###
names(moos.final.discharge.2020) <- c("Site", "DateTime", "MeanDischarge")
poke.final.discharge.2020 <- poke.final.discharge.2020[,-c(3,4)]
names(poke.final.discharge.2020) <- c("Site", "DateTime", "MeanDischarge")
names(strt.final.discharge.2020) <- c("Site", "DateTime", "MeanDischarge")
names(vaul.final.discharge.2020) <- c("Site", "DateTime", "MeanDischarge")


any(is.na(moos.final.discharge.2020))
moos.final.discharge.2020 <- na.omit(moos.final.discharge.2020) # removed 15 rows (11589 to 11574)
any(is.na(frch.final.discharge.2020))
frch.final.discharge.2020 <- na.omit(frch.final.discharge.2020) # removed 3 rows (11979 to 11976)
any(is.na(poke.final.discharge.2020)) # poke is all good 
any(is.na(strt.final.discharge.2020))
strt.final.discharge.2020 <- na.omit(strt.final.discharge.2020) # removed 3 rows (10817to 10814)
any(is.na(vaul.final.discharge.2020)) # vaul is all good

all.discharge.2020 <- rbind(frch.final.discharge.2020, moos.final.discharge.2020, 
                              poke.final.discharge.2020, strt.final.discharge.2020,
                              vaul.final.discharge.2020)

Q_2020 <- all.discharge.2020
Q_2020$day = format(as.POSIXct(Q_2020$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
Q_2020$day = as.POSIXct(Q_2020$day, "%Y-%m-%d", tz="America/Anchorage")


Q.daily.2020 = with(Q_2020, tapply(MeanDischarge, list(day, Site), mean))
Q.daily.2020 = as.data.frame(Q.daily.2020)



# Write CSV ##
write.csv(frch.final.discharge.2020, "~/Documents/DoD_Discharge/Predicted_Discharge/2020/FRCH/final_frch_2020.csv")
write.csv(moos.final.discharge.2020, "~/Documents/DoD_Discharge/Predicted_Discharge/2020/MOOS/final_moos_2020.csv")
write.csv(poke.final.discharge.2020, "~/Documents/DoD_Discharge/Predicted_Discharge/2020/POKE/final_poke_2020.csv")
write.csv(strt.final.discharge.2020, "~/Documents/DoD_Discharge/Predicted_Discharge/2020/STRT/final_strt_2020.csv")
write.csv(vaul.final.discharge.2020, "~/Documents/DoD_Discharge/Predicted_Discharge/2020/VAUL/final_vaul_2020.csv")
write.csv(all.discharge.2020,"~/Documents/DoD_Discharge/Predicted_Discharge/2020/all.discharge.2020.csv", row.names = FALSE)
write.csv(Q_2020,"~/Documents/DoD_Discharge/Predicted_Discharge/2020/Q_2020.csv", row.names = FALSE)
write.csv(Q.daily.2020,"~/Documents/DoD_Discharge/Predicted_Discharge/2020/Q.daily.2020.csv", row.names = FALSE)


################################################# 2021 ###################################################
### POKE ###
# PT1 #
Poke1comb.2021.1$pred.poke1.Q <- coef(POKE1.lm.2021.1)[2] * Poke1comb.2021.1$WaterLevel+ coef(POKE1.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") 

# PT2 #
Poke2comb.2021.1$pred.poke2.Q <- coef(POKE2.lm.2021.1)[2] * Poke2comb.2021.1$WaterLevel+ coef(POKE2.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.poke2.Q), data = Poke2comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylim(0, 3500) +
  ylab("Discharge(L/s)")

poke.final.discharge.2021 <- full_join(Poke1comb.2021.1, Poke2comb.2021.1, by = "DateTime")
poke.final.discharge.2021$MeanDischarge <- rowMeans(poke.final.discharge.2021[,c ("pred.poke1.Q", 'pred.poke2.Q')], na.rm = TRUE) # taking the average of the two PTs
poke.final.discharge.2021 <- poke.final.discharge.2021[,-c(1:4,6:13,15:26)] # remove unnecesary columns
poke.final.discharge.2021 <- poke.final.discharge.2021[-c(30574:31598) , ] # Removing duplicate date values 

# Poker1 (light blue) and Poker2 (dark blue) with observed Q.
ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb.2021.1) +
  geom_line(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb.2021.1, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.poke2.Q), data = Poke2comb.2021.1,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = poke.final.discharge.2021, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Poker1(light) & Poker2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

### STRT ###
# PT1 #
Strt1comb.2021$pred.strt1.Q <- coef(STRT1.lm.2021)[2] * Strt1comb.2021$WaterLevel+ coef(STRT1.lm.2021)[1]
ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb.2021) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") 

# PT2 #
Strt2comb.2021.1$pred.strt2.Q <- coef(STRT2.lm.2021.1)[2] * Strt2comb.2021.1$WaterLevel+ coef(STRT2.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart 2 Predicted Q") +
  scale_shape_discrete(name = "Method", labels = c("ADCP", "Wading Rod", "Salt Dilution")) +
  xlab("") +
  ylim(0, 2000) +
  ylab("Discharge(L/s)")

strt.final.discharge.2021 <- full_join(Strt1comb.2021, Strt2comb.2021, by = "DateTime")
strt.final.discharge.2021$MeanDischarge <- rowMeans(strt.final.discharge.2021[,c ("pred.strt1.Q", 'pred.strt2.Q')], na.rm = TRUE) # taking the average of the two PTs
strt.final.discharge.2021 <- strt.final.discharge.2021[,-c(1:4,6:13,15:26)] # remove unnecesary columns
strt.final.discharge.2021 <- Strt2comb.2021
# Stuart1 (light blue) and Stuart2 (dark blue) with observed Q.
ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb.2021) +
  geom_line(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb.2021, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb.2021,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = strt.final.discharge.2021, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Poker1(light) & Poker2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

### FRCH ###
# PT1 #
Frch1comb.2021.1$pred.frch1.Q <- coef(FRCH1.lm.2021.1)[2] * Frch1comb.2021.1$WaterLevel+ coef(FRCH1.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.frch1.Q), data = Frch1comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("French1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") 

# PT2 #
Frch2comb.2021.1$pred.frch2.Q <- coef(FRCH2.lm.2021.1)[2] * Frch2comb.2021.1$WaterLevel+ coef(FRCH2.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.frch2.Q), data = Frch2comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("French 2 Predicted Q") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge(L/s)")

frch.final.discharge.2021 <- full_join(Frch1comb.2021, Frch2comb.2021, by = "DateTime")
frch.final.discharge.2021$MeanDischarge <- rowMeans(frch.final.discharge.2021[,c ("pred.frch1.Q", 'pred.frch2.Q')], na.rm = TRUE) # taking the average of the two PTs
frch.final.discharge.2021 <- frch.final.discharge.2021[,-c(1:4,6:13,15:26)] # remove unnecesary columns

# French1 (light blue) and French2 (dark blue) with observed Q.
ggplot(aes(x = DateTime, y = pred.frch1.Q), data = Frch1comb.2021) +
  geom_line(aes(x = DateTime, y = pred.frch1.Q), data = Frch1comb.2021, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.frch2.Q), data = Frch2comb.2021,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = frch.final.discharge.2021, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 1500) +
  ggtitle("French1(light) & French2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")
frch.final.discharge.2021 <- Frch2comb.2021.1
### MOOS ###
# PT1 #
Moos1comb.2021.1$pred.moos1.Q <- coef(MOOS1.lm.2021.1)[2] * Moos1comb.2021.1$WaterLevel+ coef(MOOS1.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moos1comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose1 predicted all measured Q") +
  scale_shape_discrete(name = "Method", labels = c("ADCP", "Wading Rod", "Salt Dilution")) +
  xlab("Date") +
  ylab("Discharge (L/s)") 

# PT2 #
Moos2comb.2021.1$pred.moos2.Q <- coef(MOOS2.lm.2021.1)[2] * Moos2comb.2021.1$WaterLevel+ coef(MOOS2.lm.2021.1)[1]
ggplot(aes(x = DateTime, y = pred.moos2.Q), data = Moos2comb.2021.1) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose 2 Predicted Q") +
  scale_shape_discrete(name = "Method", labels = c("ADCP", "Wading Rod", "Salt Dilution")) +
  xlab("") +
  ylab("Discharge(L/s)")

moos.final.discharge.2021 <- full_join(Moos1comb.2021.1, Moos2comb.2021.1, by = "DateTime")
moos.final.discharge.2021$MeanDischarge <- rowMeans(moos.final.discharge.2021[,c("pred.moos1.Q", 'pred.moos2.Q')], na.rm = TRUE) # taking the average of the two PTs
moos.final.discharge.2021 <- moos.final.discharge.2021[,-c(1:4,6:13,15:26)] # remove unnecesary columns

# Moose1 (light blue) and Moose2 (dark blue) with observed Q.
ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moos1comb.2021) +
  geom_line(aes(x = DateTime, y = pred.moos1.Q), data = Moos1comb.2021, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.moos2.Q), data = Moos2comb.2021,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = DateTime, y = MeanDischarge), data = moos.final.discharge.2021, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 4000) +
  ggtitle("Moose1(light) & Moose2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

### VAUL ###
# PT1 #
Vaul1comb.2021$pred.vaul1.Q <- coef(VAUL1.lm.2021)[2] * Vaul1comb.2021$WaterLevel+ coef(VAUL1.lm.2021)[1]
ggplot(aes(x = DateTime, y = pred.vaul1.Q), data = Vaul1comb.2021) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vaul1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") 

vaul.final.discharge.2021 <- Vaul1comb.2021
### Rename Columns ###
moos.final.discharge.2021$Site <- "MOOS" # adding site identifier
moos.final.discharge.2021 <- moos.final.discharge.2021[,-c(2:3) ] # Removing the two individual columns
poke.final.discharge.2021$Site <- "POKE"
poke.final.discharge.2021 <- poke.final.discharge.2021[,-c(2:3) ] # Removing the two individual columns
frch.final.discharge.2021$Site <- "FRCH" # adding site identifier
frch.final.discharge.2021 <- frch.final.discharge.2021[,-c(2:3) ] # Removing the two individual columns
strt.final.discharge.2021$Site <- "STRT" # adding site identifier
strt.final.discharge.2021 <- strt.final.discharge.2021[,-c(2:3) ] # Removing the two individual columns
vaul.final.discharge.2021 <- vaul.final.discharge.2021[,-c(2:4,6:13)]
names(vaul.final.discharge.2021) <- c("Site", "DateTime", "MeanDischarge")


all.discharge.2021 <- rbind(frch.final.discharge.2021, moos.final.discharge.2021, 
                            poke.final.discharge.2021, strt.final.discharge.2021,
                            vaul.final.discharge.2021)

Q_2021 <- all.discharge.2021
Q_2021$day = format(as.POSIXct(Q_2021$DateTime,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
Q_2021$day = as.POSIXct(Q_2021$day, "%Y-%m-%d", tz="America/Anchorage")


Q.daily.2021 = with(Q_2021, tapply(MeanDischarge, list(day, Site), mean))
Q.daily.2021 = as.data.frame(Q.daily.2021)

write.csv(Q_2021,"~/Documents/DoD_Discharge/Predicted_Discharge/2021/Q_2021.csv", row.names = FALSE)
write.csv(Q.daily.2021,"~/Documents/DoD_Discharge/Predicted_Discharge/2021/Q.daily.2021.csv", row.names = FALSE)

################ model selection for adding on other data to sites missing a discharge record from before 6/29(VAUL, MOOS, FRCH)
# import precip data # 
strt.rain <- read.csv("~/Documents/DoD_2021/RainGauge/strt.precip.discharge.csv")
strt.rain$DateTime <- ymd_hms(strt.rain$DateTime)
attributes(strt.rain$DateTime)$tzone <- 'America/Anchorage'
strt.rain <- strt.rain[,-c(2:5)]
vaul.rain <- read.csv("~/Documents/DoD_2021/RainGauge/vaul.precip.discharge.csv")
vaul.rain$DateTime <- ymd_hms(vaul.rain$DateTime)
attributes(vaul.rain$DateTime)$tzone <- 'America/Anchorage'
vaul.rain <- vaul.rain[,-c(2:5)]

Vaul.pre.model <- QSummary.VA.2021[, -c(1:6,8,9)] # just Q and datetime from discrete measurements
names(Vaul.pre.model) <- c("Q", "DateTime")
vaul.final.discharge.2021.pre.model <- vaul.final.discharge.2021[,-1]
names(vaul.final.discharge.2021.pre.model) <- c("DateTime", "Q")
vaul.model <- rbind(Vaul.pre.model, vaul.final.discharge.2021.pre.model)
vaul.model <- left_join(vaul.model, vaul.rain, by = "DateTime")
vaul.model$doy <- yday(vaul.model$DateTime)

Poke.pre.model <- QSummary.PO.2021[, -c(1:6,8,9)]
names(Poke.pre.model) <- c("Q", "DateTime")
poke.final.discharge.2021.pre.model <- poke.final.discharge.2021[,-3]
names(poke.final.discharge.2021.pre.model) <- c("DateTime", "Q")
poke.model <- rbind(Poke.pre.model, poke.final.discharge.2021.pre.model)
poke.model <- left_join(poke.model, strt.rain, by = "DateTime")
poke.model$doy <- yday(poke.model$DateTime)


vaul.poke.model <- left_join(vaul.model, poke.model, by = "DateTime")

vaul.poke.lm.1 <- lm(vaul.poke.model$Q.x ~ vaul.poke.model$Q.y)
summary(vaul.poke.lm.1)
vaul.poke.lm.2 <- lm(vaul.poke.model$Q.x ~ vaul.poke.model$Q.y + vaul.poke.model$inst_rainfall_mm.x)
summary(vaul.poke.lm.2)
vaul.poke.lm.3 <- lm(vaul.poke.model$Q.x ~ vaul.poke.model$Q.y + vaul.poke.model$inst_rainfall_mm.x + vaul.poke.model$doy.x)
summary(vaul.poke.lm.3)
vaul.poke.lm.4 <- lm(vaul.poke.model$Q.x ~ vaul.poke.model$Q.y + vaul.poke.model$twentyfour.x)
summary(vaul.poke.lm.4)
vaul.poke.lm.5 <- lm(vaul.poke.model$Q.x ~ vaul.poke.model$Q.y + vaul.poke.model$twentyfour.x + vaul.poke.model$doy.x)
summary(vaul.poke.lm.5)


vaul.poke.model$pred.vaul1.Q <- coef(vaul.poke.lm.1)[2] * vaul.poke.model$Q.y+ coef(vaul.poke.lm.1)[1]
vaul.poke.model$pred.vaul2.Q <- coef(vaul.poke.lm.2)[2] * vaul.poke.model$Q.y+ coef(vaul.poke.lm.1)[1]
vaul.poke.model$pred.vaul3.Q <- coef(vaul.poke.lm.3)[2] * vaul.poke.model$Q.y+ coef(vaul.poke.lm.3)[1]
vaul.poke.model$pred.vaul4.Q <- coef(vaul.poke.lm.4)[2] * vaul.poke.model$Q.y+ coef(vaul.poke.lm.4)[1]
vaul.poke.model$pred.vaul5.Q <- coef(vaul.poke.lm.5)[2] * vaul.poke.model$Q.y+ coef(vaul.poke.lm.5)[1]

vaul.pre.pt <- vaul.poke.model %>% subset(vaul.poke.model$DateTime > "2021-05-10" & vaul.poke.model$DateTime < "2021-06-29") # clipping off data that is empty in vaul data 

vaul.final.discharge.2021.1 <- left_join(vaul.final.discharge.2021, vaul.pre.pt) # joining to make a complete data frame throughout the year

vaul.final.discharge.2021.1$Q_final <- rowMeans(vaul.final.discharge.2021.1[,c(3,14)], na.rm = TRUE) # taking the average of the vaul mean discharge and the new column from the lm
vaul.final.discharge.2021.1$Q_final.2 <- rowMeans(vaul.final.discharge.2021.1[,c(3,15)], na.rm = TRUE) # taking the average of the vaul mean discharge and the new column from the lm
vaul.final.discharge.2021.1$Q_final.3 <- rowMeans(vaul.final.discharge.2021.1[,c(3,16)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm
vaul.final.discharge.2021.1$Q_final.4 <- rowMeans(vaul.final.discharge.2021.1[,c(3,17)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm
vaul.final.discharge.2021.1$Q_final.5 <- rowMeans(vaul.final.discharge.2021.1[,c(3,18)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm

vaul.final.discharge.2021.2 <- full_join(vaul.final.discharge.2021.1, QSummary.VA.2021) # joining the discrete measurements with the predicted Q 

ggplot(aes(x = DateTime, y = Q_final), data = vaul.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vaul1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.2), data = vaul.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vaul1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") # plotting 

ggplot(aes(x = DateTime, y = Q_final.3), data = vaul.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vaul1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.4), data = vaul.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vaul1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.5), data = vaul.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vaul1 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = MeanDischarge), data = poke.final.discharge.2021) +
  geom_line(color="#A6CEE3", size=1.25) +
  theme_classic() +
  ggtitle("poke average predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

# FRCH # 
Frch.pre.model <- QSummary.FR.2021[, -c(1:6,8,9)] # just Q and datetime from discrete measurements
names(Frch.pre.model) <- c("Q", "DateTime")
frch.final.discharge.2021.pre.model <- frch.final.discharge.2021[,-c(1:4,6:13)]
names(frch.final.discharge.2021.pre.model) <- c("DateTime", "Q")
frch.model <- rbind(Frch.pre.model, frch.final.discharge.2021.pre.model)
frch.model <- left_join(frch.model, strt.rain, by = "DateTime")
frch.model$doy <- yday(frch.model$DateTime)

Strt.pre.model <- QSummary.ST.2021[, -c(1:6,8,9)]
names(Strt.pre.model) <- c("Q", "DateTime")
strt.final.discharge.2021.pre.model <- strt.final.discharge.2021[,-c(1:4,6:13)]
names(strt.final.discharge.2021.pre.model) <- c("DateTime", "Q")
#strt.model <- rbind(Strt.pre.model, strt.final.discharge.2021.pre.model)
strt.model <- left_join(strt.model, strt.rain, by = "DateTime")
strt.model$doy <- yday(strt.model$DateTime)

frch.strt.model <- left_join(frch.model, strt.model, by = "DateTime")

frch.strt.lm.1 <- lm(frch.strt.model$Q.x ~ frch.strt.model$Q.y)
summary(frch.strt.lm.1)
frch.strt.lm.2 <- lm(frch.strt.model$Q.x ~ frch.strt.model$Q.y + frch.strt.model$inst_rainfall_mm.x)
summary(frch.strt.lm.2)
frch.strt.lm.3 <- lm(frch.strt.model$Q.x ~ frch.strt.model$Q.y + frch.strt.model$inst_rainfall_mm.x + frch.strt.model$doy.x)
summary(frch.strt.lm.3)
frch.strt.lm.4 <- lm(frch.strt.model$Q.x ~ frch.strt.model$Q.y + frch.strt.model$twentyfour.x)
summary(frch.strt.lm.4)
frch.strt.lm.5 <- lm(frch.strt.model$Q.x ~ frch.strt.model$Q.y + frch.strt.model$twentyfour.x + frch.strt.model$doy.x)
summary(frch.strt.lm.5)


frch.strt.model$pred.frch1.Q <- coef(frch.strt.lm.1)[2] * frch.strt.model$Q.y+ coef(frch.strt.lm.1)[1]
frch.strt.model$pred.frch2.Q <- coef(frch.strt.lm.2)[2] * frch.strt.model$Q.y+ coef(frch.strt.lm.2)[1]
frch.strt.model$pred.frch3.Q <- coef(frch.strt.lm.3)[2] * frch.strt.model$Q.y+ coef(frch.strt.lm.3)[1]
frch.strt.model$pred.frch4.Q <- coef(frch.strt.lm.4)[2] * frch.strt.model$Q.y+ coef(frch.strt.lm.4)[1]
frch.strt.model$pred.frch5.Q <- coef(frch.strt.lm.5)[2] * frch.strt.model$Q.y+ coef(frch.strt.lm.5)[1]

frch.pre.pt <- frch.strt.model %>% subset(frch.strt.model$DateTime > "2021-05-10" & frch.strt.model$DateTime < "2021-06-29") # clipping off data that is empty in vaul data 

frch.final.discharge.2021.1 <- left_join(frch.final.discharge.2021.pre.model, frch.pre.pt) # joining to make a complete data frame throughout the year

frch.final.discharge.2021.1$Q_final <- rowMeans(frch.final.discharge.2021.1[,c(2,16)], na.rm = TRUE) # taking the average of the vaul mean discharge and the new column from the lm
frch.final.discharge.2021.1$Q_final.2 <- rowMeans(frch.final.discharge.2021.1[,c(2,17)], na.rm = TRUE) # taking the average of the vaul mean discharge and the new column from the lm
frch.final.discharge.2021.1$Q_final.3 <- rowMeans(frch.final.discharge.2021.1[,c(2,18)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm
frch.final.discharge.2021.1$Q_final.4 <- rowMeans(frch.final.discharge.2021.1[,c(2,19)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm
frch.final.discharge.2021.1$Q_final.5 <- rowMeans(frch.final.discharge.2021.1[,c(2,20)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm

frch.final.discharge.2021.2 <- full_join(frch.final.discharge.2021.1, QSummary.FR.2021) # joining the discrete measurements with the predicted Q 

ggplot(aes(x = DateTime, y = Q_final), data = frch.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("FRCH2 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.2), data = frch.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("FRCH2 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") # plotting 

ggplot(aes(x = DateTime, y = Q_final.3), data = frch.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("FRCH2 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.4), data = frch.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("FRCH2 predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.5), data = frch.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("FRCH predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

# MOOS #
Moos.pre.model <- QSummary.MO.2021[, -c(1:6,8,9)] # just Q and datetime from discrete measurements
names(Moos.pre.model) <- c("Q", "DateTime")
moos.final.discharge.2021.pre.model <- moos.final.discharge.2021[,-c(2,3)]
names(moos.final.discharge.2021.pre.model) <- c("DateTime", "Q")
moos.model <- rbind(Moos.pre.model, moos.final.discharge.2021.pre.model)
moos.model <- left_join(moos.model, strt.rain, by = "DateTime")
moos.model$doy <- yday(moos.model$DateTime)

moos.strt.model <- left_join(moos.model, strt.model, by = "DateTime")

moos.strt.lm.1 <- lm(moos.strt.model$Q.x ~ moos.strt.model$Q.y)
summary(moos.strt.lm.1)
moos.strt.lm.2 <- lm(moos.strt.model$Q.x ~ moos.strt.model$Q.y + moos.strt.model$inst_rainfall_mm.x)
summary(moos.strt.lm.2)
moos.strt.lm.3 <- lm(moos.strt.model$Q.x ~ moos.strt.model$Q.y + moos.strt.model$inst_rainfall_mm.x + moos.strt.model$doy.x)
summary(moos.strt.lm.3)
moos.strt.lm.4 <- lm(moos.strt.model$Q.x ~ moos.strt.model$Q.y + moos.strt.model$twentyfour.x)
summary(moos.strt.lm.4)
moos.strt.lm.5 <- lm(moos.strt.model$Q.x ~ moos.strt.model$Q.y + moos.strt.model$twentyfour.x + moos.strt.model$doy.x)
summary(moos.strt.lm.5)


moos.strt.model$pred.vaul1.Q <- coef(moos.strt.lm.1)[2] * moos.strt.model$Q.y+ coef(moos.strt.lm.1)[1]
moos.strt.model$pred.vaul2.Q <- coef(moos.strt.lm.2)[2] * moos.strt.model$Q.y+ coef(moos.strt.lm.2)[1]
moos.strt.model$pred.vaul3.Q <- coef(moos.strt.lm.3)[2] * moos.strt.model$Q.y+ coef(moos.strt.lm.3)[1]
moos.strt.model$pred.vaul4.Q <- coef(moos.strt.lm.4)[2] * moos.strt.model$Q.y+ coef(moos.strt.lm.4)[1]
moos.strt.model$pred.vaul5.Q <- coef(moos.strt.lm.5)[2] * moos.strt.model$Q.y+ coef(moos.strt.lm.5)[1]

moos.pre.pt <- moos.strt.model %>% subset(moos.strt.model$DateTime > "2021-05-10" & moos.strt.model$DateTime < "2021-06-29") # clipping off data that is empty in vaul data 

moos.final.discharge.2021.1 <- left_join(moos.final.discharge.2021.pre.model, moos.pre.pt) # joining to make a complete data frame throughout the year

moos.final.discharge.2021.1$Q_final <- rowMeans(moos.final.discharge.2021.1[,c(2,16)], na.rm = TRUE) # taking the average of the vaul mean discharge and the new column from the lm
moos.final.discharge.2021.1$Q_final.2 <- rowMeans(moos.final.discharge.2021.1[,c(2,17)], na.rm = TRUE) # taking the average of the vaul mean discharge and the new column from the lm
moos.final.discharge.2021.1$Q_final.3 <- rowMeans(moos.final.discharge.2021.1[,c(2,18)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm
moos.final.discharge.2021.1$Q_final.4 <- rowMeans(moos.final.discharge.2021.1[,c(2,19)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm
moos.final.discharge.2021.1$Q_final.5 <- rowMeans(moos.final.discharge.2021.1[,c(2,20)], na.rm = TRUE)# taking the average of the vaul mean discharge and the new column from the lm

moos.final.discharge.2021.2 <- full_join(moos.final.discharge.2021.1, QSummary.MO.2021) # joining the discrete measurements with the predicted Q 


ggplot(aes(x = DateTime, y = Q_final), data = moos.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("MOOS predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.2), data = moos.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("MOOS predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") # plotting 

ggplot(aes(x = DateTime, y = Q_final.3), data = moos.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("MOOS predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.4), data = moos.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("MOOS predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

ggplot(aes(x = DateTime, y = Q_final.5), data = moos.final.discharge.2021.2) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("MOOS predicted all measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)")  # plotting 

