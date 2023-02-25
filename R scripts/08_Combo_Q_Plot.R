### Combined Q plots
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

Q.2015 <- read.csv(here("Predicted_Discharge/2015/Predicted_Q_2015.csv"))
names(Q.2015) <- c(NA, "Site", "Q", "DateTime")
Q.2018 <- read.csv(here("Predicted_Discharge/2018/Q_2018.csv"))
Q.2019 <- read.csv(here("Predicted_Discharge/2019/Predicted_Q_2019_gapfill.csv"))
Q.2019 <- Q.2019 %>% pivot_longer(c("FRCH", "MOOS", "POKE", "VAUL", "STRT", "POKE.Q.int"), "Site") %>% filter(!Site %in% c("POKE") )
Q.2019 <- Q.2019[,c(2,5,6)]
Q.2020 <- read.csv(here("Predicted_Discharge/2020/Q_2020.csv"))
Q.2021 <- read.csv(here("Predicted_Discharge/2021/Predicted_Q_2021_gapfill.csv"))
Q.2021 <- Q.2021 %>% pivot_longer(c("FRCH", "MOOS", "POKE", "VAUL", "STRT", "POKE.Q.int", "FRCH.Q.int", "MOOS.Q.int", "VAUL.Q.int", "STRT.Q.int"), "Site")  %>% filter(!Site %in% c("MOOS", "STRT", "VAUL", "POKE", "FRCH"))
Q.2021 <- Q.2021[,c(2,5,6)]
Q.2022 <- read.csv(here("Predicted_Discharge/2022/Predicted_Q_2022_gapfill.csv"))
Q.2022 <- Q.2022 %>% pivot_longer(c("FRCH", "MOOS", "POKE", "VAUL", "STRT", "POKE.Q.int", "MOOS.Q.int", "STRT.Q.int"), "Site")  %>% filter(!Site %in% c("MOOS", "STRT", "POKE"))
Q.2022 <- Q.2022[,c(2,4,5)]
names(Q.2022) <- c("DateTime", "Site", "value")


Q.dat.1 <- rbind(Q.2015[,-1],Q.2018, Q.2020)
Q.dat.2 <- rbind(Q.2019, Q.2021, Q.2022)
Q.dat.3 <- Q.dat.2 %>% mutate(Site2 = case_when(Site == "POKE.Q.int" ~ "POKE",
                                                      Site == "MOOS.Q.int" ~ "MOOS",
                                                      Site == "FRCH.Q.int" ~ "FRCH",
                                                      Site == "VAUL.Q.int" ~ "VAUL",
                                                      Site == "STRT.Q.int" ~ "STRT",
                                                Site == "POKE" ~ "POKE",
                                                Site == "MOOS" ~ "MOOS",
                                                Site == "FRCH" ~ "FRCH",
                                                Site == "VAUL" ~ "VAUL",
                                                Site == "STRT" ~ "STRT"
                                                      ))
names(Q.dat.3) <- c("DateTime", "v", "Q", "Site")
Q.dat <- rbind(Q.dat.1, Q.dat.3[,-2])
Q.dat$DateTime <- as.POSIXct(Q.dat$DateTime, tzone = "America/Anchorage")
Q.dat$Year <- Q.dat$DateTime %>% format('%Y')
Q.dat$Julian <- as.POSIXlt(Q.dat$DateTime)$yday 

## Q plots
Q.dat %>% na.omit() %>% ggplot(aes(Julian, Q, color = Site)) + geom_point(size = 0.2) + 
  facet_wrap(~Year, scales = "free_y", ncol = 2, nrow = 3) +
  xlab("Julian day") + ylab("Discharge (L/s)") + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2)))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15)) +
  scale_color_discrete(labels = c("French", "Moose", "Poker", "Stuart", "Vault"))
ggsave("All Q by year.png", width = 8, height = 6)

site_lab <- c("French", "Moose", "Poker", "Stuart", "Vault")
names(site_lab) <- c("FRCH", "MOOS", "POKE", "STRT", "VAUL")

Q.dat %>% na.omit() %>% ggplot(aes(Julian, Q, color = Year)) + geom_point(size = 0.2) + 
  facet_wrap(~Site, scales = "free_y", labeller = labeller(Site = site_lab), ncol = 2, nrow = 3) +
  xlab("Julian day") + ylab("Discharge (L/s)") + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2), nrow = 2))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15), legend.position = c(0.75, 0.15))
  
ggsave("All Q by site.png", width = 8, height = 6)

Q.dat %>% na.omit() %>% ggplot(aes(Julian, Q, color = Year)) + geom_point(size = 1) + 
  facet_wrap(~Site, scales = "free_y", labeller = labeller(Site = site_lab), ncol = 1, nrow = 5) +
  xlab("Julian day") + ylab("Discharge (L/s)") + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2), nrow = 2))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15), legend.position = "bottom")

ggsave("All Q by site long.png", width = 8, height = 13)


Q.dat %>% na.omit() %>% ggplot(aes(Julian, Q, color = Site)) + geom_point(size = 1) + 
  facet_wrap(~Year, scales = "free_y", ncol = 1, nrow = 6) +
  xlab("Julian day") + ylab("Discharge (L/s)") + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2)))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15), legend.position = "bottom") +
  scale_color_discrete(labels = c("French", "Moose", "Poker", "Stuart", "Vault"))
ggsave("All Q by year long.png", width = 8, height = 15)


### Cumulative Q plots
Q.dat <- Q.dat[order(Q.dat$DateTime), ]
Q.dat.cum <- Q.dat %>% group_by(Site, Year) %>% na.omit() %>% summarise(DateTime = DateTime,
                                                          Q = Q,
                                                          cumul = cumsum(Q*900), # 900 seconds in 15 minutes
                                                      Julian = Julian
                                                      )
Q.dat.cum$cumul_m3 <- Q.dat.cum$cumul*.001
Q.dat.cum <- Q.dat.cum %>% mutate(catch_size_km = case_when(Site == "POKE" ~ 62,
                                                           Site == "MOOS" ~ 111,
                                                           Site == "FRCH" ~ 44,
                                                           Site == "VAUL" ~ 35,
                                                           Site == "STRT" ~ 125))
Q.dat.cum$cumul_m3_km2 <- Q.dat.cum$cumul_m3/Q.dat.cum$catch_size_km 


Q.dat.cum %>% filter(Year %in% c("2019", "2020", "2021", "2022")) %>% ggplot(aes(Julian, cumul_m3, color = Site)) + geom_point(size = 1) + 
  facet_wrap(~Year) +
  xlab("Julian day") + ylab(expression(paste("Discharge (", m^{3}, ")"))) + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2)))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15)) +
  scale_color_discrete(labels = c("French", "Moose", "Poker", "Stuart", "Vault"))
ggsave("Cumulative Q by year.png", width = 8, height = 6)

site_lab <- c("French", "Moose", "Poker", "Stuart", "Vault")
names(site_lab) <- c("FRCH", "MOOS", "POKE", "STRT", "VAUL")

Q.dat.cum %>% filter(Year %in% c("2019", "2020", "2021", "2022")) %>% ggplot(aes(Julian, cumul_m3, color = Year)) + geom_point(size = 1) + 
  facet_wrap(~Site, scales = "free_y", labeller = labeller(Site = site_lab), ncol = 2, nrow = 3) +
  xlab("Julian day") + ylab(expression(paste("Discharge (", m^{3}, ")"))) + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2), nrow = 2))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15), legend.position = c(0.75, 0.15))

ggsave("Cumulative Q by site.png", width = 8, height = 6)

# Divided by catchment size
Q.dat.cum %>% filter(Year %in% c("2019", "2020", "2021", "2022")) %>% ggplot(aes(Julian, cumul_m3_km2, color = Site)) + geom_point(size = 1) + 
  facet_wrap(~Year) +
  xlab("Julian day") + ylab(expression(paste("Discharge (", m^{3},km^{-2},")"))) + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2)))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15)) +
  scale_color_discrete(labels = c("French", "Moose", "Poker", "Stuart", "Vault"))
ggsave("Cumulative Q by catch size by year.png", width = 8, height = 6)

site_lab <- c("French", "Moose", "Poker", "Stuart", "Vault")
names(site_lab) <- c("FRCH", "MOOS", "POKE", "STRT", "VAUL")

Q.dat.cum %>% filter(Year %in% c("2019", "2020", "2021", "2022")) %>% ggplot(aes(Julian, cumul_m3_km2, color = Year)) + geom_point(size = 1) + 
  facet_wrap(~Site, scales = "free_y", labeller = labeller(Site = site_lab), ncol = 2, nrow = 3) +
  xlab("Julian day") + ylab(expression(paste("Discharge (", m^{3},km^{-2},")"))) + theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=2), nrow = 2))  +
  theme (strip.background =element_rect (fill="white"), text = element_text(size =15), legend.position = c(0.75, 0.15))

ggsave("Cumulative Q by catch size by site.png", width = 8, height = 6)


