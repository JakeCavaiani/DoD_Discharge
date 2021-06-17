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
library(here)

dir.create(here("Predicted_Discharge", "Processed"))
dir.create(here("Predicted_Discharge", "Processed", "FRCH"))
dir.create(here("Predicted_Discharge", "Processed", "MOOS"))
dir.create(here("Predicted_Discharge", "Processed", "POKE"))
dir.create(here("Predicted_Discharge", "Processed", "STRT"))
dir.create(here("Predicted_Discharge", "Processed", "VAUL"))


### FRCH ###
# PT1 #
French1comb$pred.french1.Q <- coef(French1.lm)[2] * French1comb$WaterLevel+ coef(French1.lm)[1]
ggplot(aes(x = DateTime, y = pred.french1.Q), data=French1comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  ggtitle("French1 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") 
# PT2 #
French2comb$pred.french2.Q <- coef(French2.lm)[2] * French2comb$WaterLevel+ coef(French2.lm)[1]
ggplot(aes(x = DateTime, y = pred.french2.Q), data=French2comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  ggtitle("French") +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15","2020-10-10")))

# Final Discharge # 
frch.final.discharge <- data.frame(French1comb$DateTime, French1comb$pred.french1.Q, French2comb$pred.french2.Q)

frch.final.discharge$MeanDischarge <- rowMeans(frch.final.discharge[,c ('French1comb.pred.french1.Q', 'French2comb.pred.french2.Q')], na.rm = TRUE) 

### French1 (light blue), French2 (dark blue), and mean (red) with observed Q.
French.final <- ggplot(aes(x = DateTime, y = pred.french1.Q), data = French1comb) +
  geom_line(aes(x = DateTime, y = pred.french1.Q), data = French1comb, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.french2.Q), data = French2comb,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = French1comb.DateTime, y = MeanDischarge), data = frch.final.discharge, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ggtitle("French1(light) & French2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")
French.final


### MOOS ### 
# PT1 #
Moose1comb$pred.moos1.Q <- coef(MOOS1.lm)[2] * Moose1comb$WaterLevel+ coef(MOOS1.lm)[1]
ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose1 predicted all measured Q") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("Date") +
  ylab("Predicted Discharge") 

# PT2 # 
Moose2comb$pred.moos2.Q <- coef(MOOS2.lm)[2] * Moose2comb$WaterLevel+ coef(MOOS2.lm)[1]
ggplot(aes(x = DateTime, y = pred.moos2.Q), data = Moose2comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Moose") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))

# Final Discharge # 
moos.final.discharge <- data.frame(Moose2comb$DateTime, Moose1comb$pred.moos1.Q, Moose2comb$pred.moos2.Q)

moos.final.discharge$MeanDischarge <- rowMeans(moos.final.discharge[,c('Moose1comb.pred.moos1.Q', 'Moose2comb.pred.moos2.Q')], na.rm = TRUE) 

write.csv(moos.final.discharge,"~/Google Drive/My Drive/DoD project/2020 AK sensors/Discharge/Pressure Transducer/MOOS/MOOS_Final.csv", row.names = FALSE)


### Moose1 (light blue), Moose2 (dark blue), and mean (red) with observed Q.

ggplot(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb) +
  geom_line(aes(x = DateTime, y = pred.moos1.Q), data = Moose1comb, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.moos2.Q), data = Moose2comb,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = Moose2comb.DateTime, y = MeanDischarge), data = moos.final.discharge, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3500) +
  ggtitle("Moose1(light) & Moose2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

### POKE ###
# PT1 #
Poke1comb$pred.poke1.Q <- coef(POKE1.lm)[2] * Poke1comb$WaterLevel+ coef(POKE1.lm)[1]
ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker1 predicted all measured Q") +
  xlab("Date") +
  ylab("Predicted Discharge") 

# PT2 #
Poke2comb$pred.poke2.Q <- coef(POKE2.lm)[2] * Poke2comb$WaterLevel+ coef(POKE2.lm)[1]
ggplot(aes(x = DateTime, y = pred.poke2.Q), data = Poke2comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Poker") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge(L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))

# POKE 
poke.final.discharge <- data.frame(Poke1comb$DateTime, Poke1comb$pred.poke1.Q, Poke2comb$pred.poke2.Q)

poke.final.discharge$MeanDischarge <- rowMeans(poke.final.discharge[,c('Poke1comb.pred.poke1.Q', 'Poke2comb.pred.poke2.Q')], na.rm = TRUE) 


### Poker1 (light blue) and Poker2 (dark blue) with observed Q.
ggplot(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb) +
  geom_line(aes(x = DateTime, y = pred.poke1.Q), data = Poke1comb, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.poke2.Q), data = Poke2comb,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = Poke1comb.DateTime, y = MeanDischarge), data = poke.final.discharge, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Poker1(light) & Poker2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

### STRT ###
# PT1 #
Strt1comb$pred.strt1.Q <- coef(STRT1.lm)[2] * Strt1comb$WaterLevel+ coef(STRT1.lm)[1]
ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-09")))

# PT2 #
Strt2comb$pred.strt2.Q <- coef(STRT2.lm)[2] * Strt2comb$WaterLevel+ coef(STRT2.lm)[1]
ggplot(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Stuart2 predicted all measured Q") +
  scale_shape_discrete(name = "Method", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("Date") +
  ylab("Predicted Discharge") 

# Final STRT # 
strt.final.discharge <- data.frame(Strt1comb$DateTime, Strt1comb$pred.strt1.Q)

strt.final.discharge <- strt.final.discharge %>%  subset(strt.stream.one$DateTime > "2020-06-16")


### Stuart1 (light blue), Stuart2 (dark blue), and Stuart1 (red) because STRT2 seems bad with observed Q.

ggplot(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb) +
  geom_line(aes(x = DateTime, y = pred.strt1.Q), data = Strt1comb, color="#A6CEE3", size=1.25) +
  geom_line(aes(x = DateTime, y = pred.strt2.Q), data = Strt2comb,color="#1F78B4", size=1.25, alpha = 0.75) +
  geom_line(aes(x = Strt1comb.DateTime, y = Strt1comb.pred.strt1.Q), data = strt.final.discharge, color = "red", size = 1.25, alpha = 0.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls), size=2) +
  theme_classic() +
  ylim(0, 3000) +
  ggtitle("Stuart1(light) & Stuart2(dark) predicted all measured Q") +
  ylab("Predicted discharge L/s") +
  xlab("Time")

### VAUL ###
# PT2 # 
Vaul2comb$pred.vaul2.Q <- coef(VAUL2.lm)[2] * Vaul2comb$WaterLevel+ coef(VAUL2.lm)[1]
ggplot(aes(x = DateTime, y = pred.vaul2.Q), data = Vaul2comb) +
  geom_line(color="#A6CEE3", size=1.25) +
  geom_point(aes(x = DateTime, y = MeasuredQ_Ls, shape = Method), size=3) +
  theme_classic() +
  ggtitle("Vault") +
  scale_shape_discrete(name = "Mehtod", labels = c("Wading Rod", "Salt Dilution", "")) +
  xlab("") +
  ylab("Discharge (L/s)") +
  scale_x_datetime(limits = as_datetime(c("2020-05-15", "2020-10-10")))

# Final VAUL #
vaul.final.discharge <- data.frame(Vaul2comb$DateTime, Vaul2comb$pred.vaul2.Q)

vaul.final.discharge <- vaul.final.discharge %>%  subset(vaul.stream$DateTime > "2020-06-16")

setwd(here())
# check: should be at DoD_Discharge

### Write CSV ### 
write.csv(frch.final.discharge,"Predicted_Discharge/processed/FRCH/FRCH.csv", row.names = FALSE)
write.csv(moos.final.discharge,"Predicted_Discharge/processed/MOOS/MOOS.csv", row.names = FALSE)
write.csv(poke.final.discharge,"Predicted_Discharge/processed/POKE/POKE.csv", row.names = FALSE)
write.csv(strt.final.discharge,"Predicted_Discharge/processed/STRT/STRT.csv", row.names = FALSE)
write.csv(vaul.final.discharge,"Predicted_Discharge/processed/VAUL/VAUL.csv", row.names = FALSE)








