#### READ ME ####
# The purpose of this script is to read in catchment characteristics and determine which 
  # candidate predictors to ues for our models 
# Step 1: Load in AK_Polygon data with catchment characteristcs for all sites
# Step 2: scatterplot matrix of all candidate predictors 
# Step 3: VIFs for catchment characteristics (>2 is no bueno)

library(tidyverse)
library(stats)
library(readr)
library(ggplot2)
library(plotly)
library(GGally)
library(ggpmisc)
library(ggpubr)
library(ggExtra)
library(lubridate)
library(nlme)
library(MuMIn)
library(multcomp)

###### CATCHMENT CHARACTERISTICS ####
# Read in polygon data 
catchment <- read.csv("~/Documents/Storms_clean_repo/Ancillary_data/AK_polys_190903_Predictors.csv")

catchment <- catchment[,-c(2:12,14:17,19,21:27,29:35,37:48)] # i just want: 1) site
  # 2) Slope, 3) area burn_recent, 4) pct_burn large 5) PF extent, 6) NDVI_p50_mean

pairs(catchment[,2:6])

ggpairs(catchment,
        columns = 2:6,
        title="Correlation matrix: All sites") 
# this shows that slope and PF are highly correlated

highlight_df <- filter(catchment, site %in% c("Caribou_CJ", "French", "Poker_PJ",
                                              "Moose", "Vault", "Stuart"))

catchment %>% 
  ggplot(aes(x = SLOPE_MEAN, y = NDVI_p50__mean)) + 
  geom_point(alpha=0.3) +
  geom_point(data = highlight_df, 
             aes(x = SLOPE_MEAN, y = NDVI_p50__mean), 
             color='red',
             size=3)


# combine AMC and catchment_characteristics
AMC <- read.csv("~/Documents/Storms_clean_repo/Output_from_analysis/07_Combine_HI_BETA_FI/antecedent_HI_FI_AllYears.csv")
DOD_catchment <- read.csv("Ancillary_data/DOD_Sites_AK_polys_190903_Predictors.csv")
# 
AMC <- full_join(AMC, DOD_catchment)
# 
write.csv(AMC, "~/Documents/Storms_clean_repo/Output_from_analysis/08_Catchment_characteristics/Antecedent_HI_BETA_Catchment.csv")


AMC <- read_csv("Output_from_analysis/08_Catchment_characteristics/Antecedent_HI_BETA_Catchment.csv")

HI.median<- AMC %>% group_by(site.ID, response_var) %>%  
  summarise_at(vars(Hyst_index), list(HI = median)) # takes the median by site response and year 

HI.median <- AMC %>% group_by(response_var,site.ID, year) %>%
  summarize(MedianHI = mean(Hyst_index),
            MedianFI = mean(Flush_index, na.rm = TRUE),
            MedianBETA = mean(Beta_index, na.rm = TRUE),
            sdHI = sd(Hyst_index),
            sdFI = sd(Flush_index, na.rm = TRUE),
            sdBETA = sd(Beta_index, na.rm = TRUE),
            CVhi = sdHI/MedianHI,
            CVfi = sdFI/MedianFI,
            CVbeta = sdBETA/MedianBETA,
            pf = paste(pf),
            burn = paste(burn),
            Pf_Prob_1m_mean_x = paste(Pf_Prob_1m_mean_x),
            pctburn_lg = paste(pctburn_lg),
            Slope = paste(SLOPE_MEAN),
            NDVI = paste(NDVI_p50__mean))


HI.median$pf <- ifelse(HI.median$site.ID == "MOOS"| HI.median$site.ID == "FRCH"| HI.median$site.ID == "STRT",
                       "Moderate", "High")

CV.all <- HI.median[!duplicated(HI.median$MedianHI), ] # removing duplicated rows 
CV.average.pf <- HI.median %>% 
  group_by(pf, response_var) %>% 
  dplyr::summarise(AverageCVHI = mean(CVhi, na.rm = TRUE),
                   AverageCVFI = mean(CVfi, na.rm = TRUE),
                   AverageCVBETA = mean(CVbeta, na.rm = TRUE))
CV.average.pf <- CV.average.pf[!duplicated(CV.average.pf$AverageCVHI), ] # removing duplicated rows 
write.table(CV.average.pf, file = "CV.average.pf.csv", sep = ",", col.names = NA,
            qmethod = "double")

NO3.catchment <- subset(HI.median, HI.median$response_var == "NO3")
fDOM.catchment <- subset(HI.median, HI.median$response_var == "fDOM")
SPC.catchment <- subset(HI.median, HI.median$response_var == "SPC")
turb.catchment <- subset(HI.median, HI.median$response_var == "turb")

#### HI ###
# PF 
ggplot(HI.median, aes(x = Pf_Prob_1m_mean_x, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/HI", 
       filename = "HI_PF.pdf",
       width = 7, height = 7)

# This is taking an ANOVA of the mean HI for each catchment 
one.way.no3 <- aov(MedianHI ~ Pf_Prob_1m_mean_x, data = NO3.catchment)
summary(one.way.no3) # seemingly not significant  (p-value =0.679)


TukeyHSD(one.way.no3, which = "site.ID", na.rm = TRUE)


# burn 
ggplot(HI.median, aes(x = pctburn_lg, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/HI", 
       filename = "HI_burn.pdf",
       width = 7, height = 7)

# SLOPE 
ggplot(HI.median, aes(x = Slope, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/HI", 
       filename = "HI_slope.pdf",
       width = 7, height = 7)

# NDVI 
ggplot(HI.median, aes(x = NDVI, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/HI", 
       filename = "HI_NDVI.pdf",
       width = 7, height = 7)

#### BETA ###
# PF 
ggplot(HI.median, aes(x = Pf_Prob_1m_mean_x, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/BETA", 
       filename = "BETA_PF.pdf",
       width = 7, height = 7)

# WF 
ggplot(HI.median, aes(x = pctburn_lg, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/BETA", 
       filename = "BETA_WF.pdf",
       width = 7, height = 7)

# Slope 
ggplot(HI.median, aes(x = Slope, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/BETA", 
       filename = "BETA_slope.pdf",
       width = 7, height = 7)

# NDVI 
ggplot(HI.median, aes(x = NDVI, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave(path = "~/Documents/Storms_clean_repo/plots/Catchment_characteristics/BETA", 
       filename = "BETA_NDVI.pdf",
       width = 10, height = 10)

#


###### HI_BETA PLOTS ####
# Load in Antecedent moisture conditions dataframe
AMC <- read.csv("~/Documents/Storms_clean_repo/Output_from_analysis/07_Combine_HI_BETA_FI/antecedent_HI_FI_AllYears.csv") 

AMC <- AMC[,-c(1:2,14,15,17,27)] # cleaning up columns that are unnecessary 

colNames <- c("Hyst_index", "HI_ymin", "HI_ymax", "site.ID", "storm.ID", "month", 
              "day", "response_var", "Flush_index", "FI_ymin", "FI_ymax", "year", 
              "Parameter", "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", "t", 
              "df", "p", "StormPrecip", "StormTemp", "PrecipWeek", "PrecipMonth", 
              "ThreeMonth", "TempWeek", "Duration", "Intensity", "doy", "burn", "pf", 
              "date", "TimeSinceChena")

names(AMC)<- colNames # renaming columns 

##subsetting by solute 
# NO3 #
HI_FI_NO3 = subset(AMC, response_var == "NO3")
# fDOM #
HI_FI_fDOM = subset(AMC, response_var == "fDOM")
# SPC #
HI_FI_SPC = subset(AMC, response_var == "SPC")
# turb #
HI_FI_turb = subset(AMC, response_var == "turb")

#### PLOTS ####

# plots 
# NO3
vn = expression(paste(N*O[3]^"-"))

HI_BETA_NO3.p = 
  ggplot(HI_FI_NO3, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = pf), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle(vn)+ 
  ylab("HI") +
  xlab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "none") +
    labs(
      colour = "Catchment",
      shape = "PF Extent")

a <- ggMarginal(HI_BETA_NO3.p, groupColour = TRUE, groupFill = TRUE)
  
  
# fDOM
HI_BETA_fDOM.p = 
    ggplot(HI_FI_fDOM, aes(Beta_index, Hyst_index)) + 
    geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
    geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
    geom_point(aes(colour = factor(site.ID), shape = pf), size = 2.5) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
    theme_bw() +
    ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
    ggtitle("fDOM")+ 
    ylab("") +
    xlab("") +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          text = element_text(size = 15),
          legend.position = "none") 
  
b <- ggMarginal(HI_BETA_fDOM.p, groupColour = TRUE, groupFill = TRUE)
 

# SPC
HI_BETA_SPC.p = 
  ggplot(HI_FI_SPC, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = pf), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("SPC")+ 
  ylab("HI") +
  xlab("BETA") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "none") 

c <- ggMarginal(HI_BETA_SPC.p, groupColour = TRUE, groupFill = TRUE)


# turb
HI_BETA_turb.p = 
  ggplot(HI_FI_turb, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = pf), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + 
  xlim(-1.5, 1.5) +
  ggtitle("Turb")+ 
  ylab("") +
  xlab("BETA") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "none") 

d <- ggMarginal(HI_BETA_turb.p, groupColour = TRUE, groupFill = TRUE)



 
ggarrange(a, b,
          c,d, 
          labels = c("A", "B",
                     "C", "D"))
ggsave(path = "~/Documents/Storms_clean_repo/plots/HI_BETA", 
       filename = "HI_BETA.pdf",
       width = 10, height = 10)







#### Storm summary stats ####
### Investigating which storms are negative beta for Turbidity ####
# by year 
HI_FI_turb_year <- HI_FI_turb
HI_FI_turb_year$year <- as.character(HI_FI_turb_year$year)

HI_FI_turb_year_2021 <- subset(HI_FI_turb_year, year == "2021")

ggplot(HI_FI_turb_year_2021, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID)), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-5, 5)+
  ggtitle("Turb")+ 
  ylab("") +
  xlab("BETA") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "bottom") 

ggplot(HI_FI_turb_year, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = year), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-5, 5)+
  ggtitle("Turb")+ 
  ylab("") +
  xlab("BETA") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "bottom") 

# this is telling me what percentage of storms fall in each quadrant and tells me how many cross 0 which are non-significant

#NO3
table(sign(HI_FI_NO3$Hyst_index))
which(HI_FI_NO3$HI_ymin < 0 & HI_FI_NO3$HI_ymax > 0 & HI_FI_NO3$Hyst_index > 0)
which(HI_FI_NO3$HI_ymin < 0 & HI_FI_NO3$HI_ymax > 0 & HI_FI_NO3$Hyst_index < 0)

table(sign(HI_FI_NO3$Beta_index))
which(HI_FI_NO3$Beta_ymin < 0 & HI_FI_NO3$Beta_ymax > 0 & HI_FI_NO3$Beta_index > 0)
which(HI_FI_NO3$Beta_ymin < 0 & HI_FI_NO3$Beta_ymax > 0 & HI_FI_NO3$Beta_index < 0)



#fDOM
table(sign(HI_FI_fDOM$Hyst_index))
which(HI_FI_fDOM$HI_ymin < 0 & HI_FI_fDOM$HI_ymax > 0 & HI_FI_fDOM$Hyst_index > 0)
which(HI_FI_fDOM$HI_ymin < 0 & HI_FI_fDOM$HI_ymax > 0 & HI_FI_fDOM$Hyst_index < 0)

table(sign(HI_FI_fDOM$Beta_index))
which(HI_FI_fDOM$Beta_ymin < 0 & HI_FI_fDOM$Beta_ymax > 0 & HI_FI_fDOM$Beta_index > 0)
which(HI_FI_fDOM$Beta_ymin < 0 & HI_FI_fDOM$Beta_ymax > 0 & HI_FI_fDOM$Beta_index < 0)


#SPC
table(sign(HI_FI_SPC$Hyst_index))
which(HI_FI_SPC$HI_ymin < 0 & HI_FI_SPC$HI_ymax > 0 & HI_FI_SPC$Hyst_index > 0)
which(HI_FI_SPC$HI_ymin < 0 & HI_FI_SPC$HI_ymax > 0 & HI_FI_SPC$Hyst_index < 0)

table(sign(HI_FI_SPC$Beta_index))
which(HI_FI_SPC$Beta_ymin < 0 & HI_FI_SPC$Beta_ymax > 0 & HI_FI_SPC$Beta_index > 0)
which(HI_FI_SPC$Beta_ymin < 0 & HI_FI_SPC$Beta_ymax > 0 & HI_FI_SPC$Beta_index < 0)


#Turb
table(sign(HI_FI_turb$Hyst_index))
which(HI_FI_turb$HI_ymin < 0 & HI_FI_turb$HI_ymax > 0 & HI_FI_turb$Hyst_index > 0)
which(HI_FI_turb$HI_ymin < 0 & HI_FI_turb$HI_ymax > 0 & HI_FI_turb$Hyst_index < 0)

table(sign(HI_FI_turb$Beta_index))
which(HI_FI_turb$Beta_ymin < 0 & HI_FI_turb$Beta_ymax > 0 & HI_FI_turb$Beta_index > 0)
which(HI_FI_turb$Beta_ymin < 0 & HI_FI_turb$Beta_ymax > 0 & HI_FI_turb$Beta_index < 0)


# by site and year  ####

# 2018  ####
# FRCH ####
# NO3
HI_FI_NO3_FRCH_2018 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_NO3_FRCH_2018$Hyst_index))
which(HI_FI_NO3_FRCH_2018$HI_ymin < 0 & HI_FI_NO3_FRCH_2018$HI_ymax > 0 & HI_FI_NO3_FRCH_2018$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2018$HI_ymin < 0 & HI_FI_NO3_FRCH_2018$HI_ymax > 0 & HI_FI_NO3_FRCH_2018$Hyst_index < 0)

# fDOM
HI_FI_fDOM_FRCH_2018 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_fDOM_FRCH_2018$Hyst_index))
which(HI_FI_fDOM_FRCH_2018$HI_ymin < 0 & HI_FI_fDOM_FRCH_2018$HI_ymax > 0 & HI_FI_fDOM_FRCH_2018$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2018$HI_ymin < 0 & HI_FI_fDOM_FRCH_2018$HI_ymax > 0 & HI_FI_fDOM_FRCH_2018$Hyst_index < 0)

#SPC
HI_FI_SPC_FRCH_2018 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_SPC_FRCH_2018$Hyst_index))
which(HI_FI_SPC_FRCH_2018$HI_ymin < 0 & HI_FI_SPC_FRCH_2018$HI_ymax > 0 & HI_FI_SPC_FRCH_2018$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2018$HI_ymin < 0 & HI_FI_SPC_FRCH_2018$HI_ymax > 0 & HI_FI_SPC_FRCH_2018$Hyst_index < 0)

#turb
HI_FI_turb_FRCH_2018 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_turb_FRCH_2018$Hyst_index))
which(HI_FI_turb_FRCH_2018$HI_ymin < 0 & HI_FI_turb_FRCH_2018$HI_ymax > 0 & HI_FI_turb_FRCH_2018$Hyst_index > 0)
which(HI_FI_turb_FRCH_2018$HI_ymin < 0 & HI_FI_turb_FRCH_2018$HI_ymax > 0 & HI_FI_turb_FRCH_2018$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2018$Beta_index))
which(HI_FI_turb_FRCH_2018$Beta_ymin < 0 & HI_FI_turb_FRCH_2018$Beta_ymax > 0 & HI_FI_turb_FRCH_2018$Beta_index > 0)
which(HI_FI_turb_FRCH_2018$Beta_ymin < 0 & HI_FI_turb_FRCH_2018$Beta_ymax > 0 & HI_FI_turb_FRCH_2018$Beta_index < 0)

# MOOS ####
# NO3 
HI_FI_NO3_MOOS_2018 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_NO3_MOOS_2018$Hyst_index))
which(HI_FI_NO3_MOOS_2018$HI_ymin < 0 & HI_FI_NO3_MOOS_2018$HI_ymax > 0 & HI_FI_NO3_MOOS_2018$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2018$HI_ymin < 0 & HI_FI_NO3_MOOS_2018$HI_ymax > 0 & HI_FI_NO3_MOOS_2018$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2018 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_fDOM_MOOS_2018$Hyst_index))
which(HI_FI_fDOM_MOOS_2018$HI_ymin < 0 & HI_FI_fDOM_MOOS_2018$HI_ymax > 0 & HI_FI_fDOM_MOOS_2018$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2018$HI_ymin < 0 & HI_FI_fDOM_MOOS_2018$HI_ymax > 0 & HI_FI_fDOM_MOOS_2018$Hyst_index < 0)

#SPC
HI_FI_SPC_MOOS_2018 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_SPC_MOOS_2018$Hyst_index))
which(HI_FI_SPC_MOOS_2018$HI_ymin < 0 & HI_FI_SPC_MOOS_2018$HI_ymax > 0 & HI_FI_SPC_MOOS_2018$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2018$HI_ymin < 0 & HI_FI_SPC_MOOS_2018$HI_ymax > 0 & HI_FI_SPC_MOOS_2018$Hyst_index < 0)

#turb
HI_FI_turb_MOOS_2018 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_turb_MOOS_2018$Hyst_index))
which(HI_FI_turb_MOOS_2018$HI_ymin < 0 & HI_FI_turb_MOOS_2018$HI_ymax > 0 & HI_FI_turb_MOOS_2018$Hyst_index > 0)
which(HI_FI_turb_MOOS_2018$HI_ymin < 0 & HI_FI_turb_MOOS_2018$HI_ymax > 0 & HI_FI_turb_MOOS_2018$Hyst_index < 0)






# CARI ####
# NO3 
HI_FI_NO3_CARI_2018 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_NO3_CARI_2018$Hyst_index))
which(HI_FI_NO3_CARI_2018$HI_ymin < 0 & HI_FI_NO3_CARI_2018$HI_ymax > 0 & HI_FI_NO3_CARI_2018$Hyst_index > 0)
which(HI_FI_NO3_CARI_2018$HI_ymin < 0 & HI_FI_NO3_CARI_2018$HI_ymax > 0 & HI_FI_NO3_CARI_2018$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2018 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_fDOM_CARI_2018$Hyst_index))
which(HI_FI_fDOM_CARI_2018$HI_ymin < 0 & HI_FI_fDOM_CARI_2018$HI_ymax > 0 & HI_FI_fDOM_CARI_2018$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2018$HI_ymin < 0 & HI_FI_fDOM_CARI_2018$HI_ymax > 0 & HI_FI_fDOM_CARI_2018$Hyst_index < 0)

#SPC
HI_FI_SPC_CARI_2018 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_SPC_CARI_2018$Hyst_index))
which(HI_FI_SPC_CARI_2018$HI_ymin < 0 & HI_FI_SPC_CARI_2018$HI_ymax > 0 & HI_FI_SPC_CARI_2018$Hyst_index > 0)
which(HI_FI_SPC_CARI_2018$HI_ymin < 0 & HI_FI_SPC_CARI_2018$HI_ymax > 0 & HI_FI_SPC_CARI_2018$Hyst_index < 0)

#turb
HI_FI_turb_CARI_2018 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_turb_CARI_2018$Hyst_index))
which(HI_FI_turb_CARI_2018$HI_ymin < 0 & HI_FI_turb_CARI_2018$HI_ymax > 0 & HI_FI_turb_CARI_2018$Hyst_index > 0)
which(HI_FI_turb_CARI_2018$HI_ymin < 0 & HI_FI_turb_CARI_2018$HI_ymax > 0 & HI_FI_turb_CARI_2018$Hyst_index < 0)










#

# 2019 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2019 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_NO3_FRCH_2019$Hyst_index))
which(HI_FI_NO3_FRCH_2019$HI_ymin < 0 & HI_FI_NO3_FRCH_2019$HI_ymax > 0 & HI_FI_NO3_FRCH_2019$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2019$HI_ymin < 0 & HI_FI_NO3_FRCH_2019$HI_ymax > 0 & HI_FI_NO3_FRCH_2019$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2019 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_fDOM_FRCH_2019$Hyst_index))
which(HI_FI_fDOM_FRCH_2019$HI_ymin < 0 & HI_FI_fDOM_FRCH_2019$HI_ymax > 0 & HI_FI_fDOM_FRCH_2019$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2019$HI_ymin < 0 & HI_FI_fDOM_FRCH_2019$HI_ymax > 0 & HI_FI_fDOM_FRCH_2019$Hyst_index < 0)

#SPC
HI_FI_SPC_FRCH_2019 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_SPC_FRCH_2019$Hyst_index))
which(HI_FI_SPC_FRCH_2019$HI_ymin < 0 & HI_FI_SPC_FRCH_2019$HI_ymax > 0 & HI_FI_SPC_FRCH_2019$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2019$HI_ymin < 0 & HI_FI_SPC_FRCH_2019$HI_ymax > 0 & HI_FI_SPC_FRCH_2019$Hyst_index < 0)

#turb
HI_FI_turb_FRCH_2019 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_turb_FRCH_2019$Hyst_index))
which(HI_FI_turb_FRCH_2019$HI_ymin < 0 & HI_FI_turb_FRCH_2019$HI_ymax > 0 & HI_FI_turb_FRCH_2019$Hyst_index > 0)
which(HI_FI_turb_FRCH_2019$HI_ymin < 0 & HI_FI_turb_FRCH_2019$HI_ymax > 0 & HI_FI_turb_FRCH_2019$Hyst_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2019 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_NO3_MOOS_2019$Hyst_index))
which(HI_FI_NO3_MOOS_2019$HI_ymin < 0 & HI_FI_NO3_MOOS_2019$HI_ymax > 0 & HI_FI_NO3_MOOS_2019$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2019$HI_ymin < 0 & HI_FI_NO3_MOOS_2019$HI_ymax > 0 & HI_FI_NO3_MOOS_2019$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2019 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_fDOM_MOOS_2019$Hyst_index))
which(HI_FI_fDOM_MOOS_2019$HI_ymin < 0 & HI_FI_fDOM_MOOS_2019$HI_ymax > 0 & HI_FI_fDOM_MOOS_2019$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2019$HI_ymin < 0 & HI_FI_fDOM_MOOS_2019$HI_ymax > 0 & HI_FI_fDOM_MOOS_2019$Hyst_index < 0)

#SPC
HI_FI_SPC_MOOS_2019 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_SPC_MOOS_2019$Hyst_index))
which(HI_FI_SPC_MOOS_2019$HI_ymin < 0 & HI_FI_SPC_MOOS_2019$HI_ymax > 0 & HI_FI_SPC_MOOS_2019$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2019$HI_ymin < 0 & HI_FI_SPC_MOOS_2019$HI_ymax > 0 & HI_FI_SPC_MOOS_2019$Hyst_index < 0)

#turb
HI_FI_turb_MOOS_2019 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_turb_MOOS_2019$Hyst_index))
which(HI_FI_turb_MOOS_2019$HI_ymin < 0 & HI_FI_turb_MOOS_2019$HI_ymax > 0 & HI_FI_turb_MOOS_2019$Hyst_index > 0)
which(HI_FI_turb_MOOS_2019$HI_ymin < 0 & HI_FI_turb_MOOS_2019$HI_ymax > 0 & HI_FI_turb_MOOS_2019$Hyst_index < 0)

# CARI ####
HI_FI_NO3_CARI_2019 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_NO3_CARI_2019$Hyst_index))
which(HI_FI_NO3_CARI_2019$HI_ymin < 0 & HI_FI_NO3_CARI_2019$HI_ymax > 0 & HI_FI_NO3_CARI_2019$Hyst_index > 0)
which(HI_FI_NO3_CARI_2019$HI_ymin < 0 & HI_FI_NO3_CARI_2019$HI_ymax > 0 & HI_FI_NO3_CARI_2019$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2019 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_fDOM_CARI_2019$Hyst_index))
which(HI_FI_fDOM_CARI_2019$HI_ymin < 0 & HI_FI_fDOM_CARI_2019$HI_ymax > 0 & HI_FI_fDOM_CARI_2019$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2019$HI_ymin < 0 & HI_FI_fDOM_CARI_2019$HI_ymax > 0 & HI_FI_fDOM_CARI_2019$Hyst_index < 0)

#SPC
HI_FI_SPC_CARI_2019 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_SPC_CARI_2019$Hyst_index))
which(HI_FI_SPC_CARI_2019$HI_ymin < 0 & HI_FI_SPC_CARI_2019$HI_ymax > 0 & HI_FI_SPC_CARI_2019$Hyst_index > 0)
which(HI_FI_SPC_CARI_2019$HI_ymin < 0 & HI_FI_SPC_CARI_2019$HI_ymax > 0 & HI_FI_SPC_CARI_2019$Hyst_index < 0)

#turb
HI_FI_turb_CARI_2019 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_turb_CARI_2019$Hyst_index))
which(HI_FI_turb_CARI_2019$HI_ymin < 0 & HI_FI_turb_CARI_2019$HI_ymax > 0 & HI_FI_turb_CARI_2019$Hyst_index > 0)
which(HI_FI_turb_CARI_2019$HI_ymin < 0 & HI_FI_turb_CARI_2019$HI_ymax > 0 & HI_FI_turb_CARI_2019$Hyst_index < 0)

# POKE ####
HI_FI_NO3_POKE_2019 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_NO3_POKE_2019$Hyst_index))
which(HI_FI_NO3_POKE_2019$HI_ymin < 0 & HI_FI_NO3_POKE_2019$HI_ymax > 0 & HI_FI_NO3_POKE_2019$Hyst_index > 0)
which(HI_FI_NO3_POKE_2019$HI_ymin < 0 & HI_FI_NO3_POKE_2019$HI_ymax > 0 & HI_FI_NO3_POKE_2019$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2019 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_fDOM_POKE_2019$Hyst_index))
which(HI_FI_fDOM_POKE_2019$HI_ymin < 0 & HI_FI_fDOM_POKE_2019$HI_ymax > 0 & HI_FI_fDOM_POKE_2019$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2019$HI_ymin < 0 & HI_FI_fDOM_POKE_2019$HI_ymax > 0 & HI_FI_fDOM_POKE_2019$Hyst_index < 0)

#SPC
HI_FI_SPC_POKE_2019 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_SPC_POKE_2019$Hyst_index))
which(HI_FI_SPC_POKE_2019$HI_ymin < 0 & HI_FI_SPC_POKE_2019$HI_ymax > 0 & HI_FI_SPC_POKE_2019$Hyst_index > 0)
which(HI_FI_SPC_POKE_2019$HI_ymin < 0 & HI_FI_SPC_POKE_2019$HI_ymax > 0 & HI_FI_SPC_POKE_2019$Hyst_index < 0)

#turb
HI_FI_turb_POKE_2019 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_turb_POKE_2019$Hyst_index))
which(HI_FI_turb_POKE_2019$HI_ymin < 0 & HI_FI_turb_POKE_2019$HI_ymax > 0 & HI_FI_turb_POKE_2019$Hyst_index > 0)
which(HI_FI_turb_POKE_2019$HI_ymin < 0 & HI_FI_turb_POKE_2019$HI_ymax > 0 & HI_FI_turb_POKE_2019$Hyst_index < 0)

# STRT ####
HI_FI_NO3_STRT_2019 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_NO3_STRT_2019$Hyst_index))
which(HI_FI_NO3_STRT_2019$HI_ymin < 0 & HI_FI_NO3_STRT_2019$HI_ymax > 0 & HI_FI_NO3_STRT_2019$Hyst_index > 0)
which(HI_FI_NO3_STRT_2019$HI_ymin < 0 & HI_FI_NO3_STRT_2019$HI_ymax > 0 & HI_FI_NO3_STRT_2019$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2019 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_fDOM_STRT_2019$Hyst_index))
which(HI_FI_fDOM_STRT_2019$HI_ymin < 0 & HI_FI_fDOM_STRT_2019$HI_ymax > 0 & HI_FI_fDOM_STRT_2019$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2019$HI_ymin < 0 & HI_FI_fDOM_STRT_2019$HI_ymax > 0 & HI_FI_fDOM_STRT_2019$Hyst_index < 0)

#SPC
HI_FI_SPC_STRT_2019 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_SPC_STRT_2019$Hyst_index))
which(HI_FI_SPC_STRT_2019$HI_ymin < 0 & HI_FI_SPC_STRT_2019$HI_ymax > 0 & HI_FI_SPC_STRT_2019$Hyst_index > 0)
which(HI_FI_SPC_STRT_2019$HI_ymin < 0 & HI_FI_SPC_STRT_2019$HI_ymax > 0 & HI_FI_SPC_STRT_2019$Hyst_index < 0)

#turb
HI_FI_turb_STRT_2019 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_turb_STRT_2019$Hyst_index))
which(HI_FI_turb_STRT_2019$HI_ymin < 0 & HI_FI_turb_STRT_2019$HI_ymax > 0 & HI_FI_turb_STRT_2019$Hyst_index > 0)
which(HI_FI_turb_STRT_2019$HI_ymin < 0 & HI_FI_turb_STRT_2019$HI_ymax > 0 & HI_FI_turb_STRT_2019$Hyst_index < 0)


# VAUL ####
HI_FI_NO3_VAUL_2019 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_NO3_VAUL_2019$Hyst_index))
which(HI_FI_NO3_VAUL_2019$HI_ymin < 0 & HI_FI_NO3_VAUL_2019$HI_ymax > 0 & HI_FI_NO3_VAUL_2019$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2019$HI_ymin < 0 & HI_FI_NO3_VAUL_2019$HI_ymax > 0 & HI_FI_NO3_VAUL_2019$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2019 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_fDOM_VAUL_2019$Hyst_index))
which(HI_FI_fDOM_VAUL_2019$HI_ymin < 0 & HI_FI_fDOM_VAUL_2019$HI_ymax > 0 & HI_FI_fDOM_VAUL_2019$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2019$HI_ymin < 0 & HI_FI_fDOM_VAUL_2019$HI_ymax > 0 & HI_FI_fDOM_VAUL_2019$Hyst_index < 0)

#SPC
HI_FI_SPC_VAUL_2019 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_SPC_VAUL_2019$Hyst_index))
which(HI_FI_SPC_VAUL_2019$HI_ymin < 0 & HI_FI_SPC_VAUL_2019$HI_ymax > 0 & HI_FI_SPC_VAUL_2019$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2019$HI_ymin < 0 & HI_FI_SPC_VAUL_2019$HI_ymax > 0 & HI_FI_SPC_VAUL_2019$Hyst_index < 0)

#turb
HI_FI_turb_VAUL_2019 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_turb_VAUL_2019$Hyst_index))
which(HI_FI_turb_VAUL_2019$HI_ymin < 0 & HI_FI_turb_VAUL_2019$HI_ymax > 0 & HI_FI_turb_VAUL_2019$Hyst_index > 0)
which(HI_FI_turb_VAUL_2019$HI_ymin < 0 & HI_FI_turb_VAUL_2019$HI_ymax > 0 & HI_FI_turb_VAUL_2019$Hyst_index < 0)















# 2020 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2020 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_NO3_FRCH_2020$Hyst_index))
which(HI_FI_NO3_FRCH_2020$HI_ymin < 0 & HI_FI_NO3_FRCH_2020$HI_ymax > 0 & HI_FI_NO3_FRCH_2020$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2020$HI_ymin < 0 & HI_FI_NO3_FRCH_2020$HI_ymax > 0 & HI_FI_NO3_FRCH_2020$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2020 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_fDOM_FRCH_2020$Hyst_index))
which(HI_FI_fDOM_FRCH_2020$HI_ymin < 0 & HI_FI_fDOM_FRCH_2020$HI_ymax > 0 & HI_FI_fDOM_FRCH_2020$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2020$HI_ymin < 0 & HI_FI_fDOM_FRCH_2020$HI_ymax > 0 & HI_FI_fDOM_FRCH_2020$Hyst_index < 0)

#SPC
HI_FI_SPC_FRCH_2020 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_SPC_FRCH_2020$Hyst_index))
which(HI_FI_SPC_FRCH_2020$HI_ymin < 0 & HI_FI_SPC_FRCH_2020$HI_ymax > 0 & HI_FI_SPC_FRCH_2020$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2020$HI_ymin < 0 & HI_FI_SPC_FRCH_2020$HI_ymax > 0 & HI_FI_SPC_FRCH_2020$Hyst_index < 0)

#turb
HI_FI_turb_FRCH_2020 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_turb_FRCH_2020$Hyst_index))
which(HI_FI_turb_FRCH_2020$HI_ymin < 0 & HI_FI_turb_FRCH_2020$HI_ymax > 0 & HI_FI_turb_FRCH_2020$Hyst_index > 0)
which(HI_FI_turb_FRCH_2020$HI_ymin < 0 & HI_FI_turb_FRCH_2020$HI_ymax > 0 & HI_FI_turb_FRCH_2020$Hyst_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2020 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_NO3_MOOS_2020$Hyst_index))
which(HI_FI_NO3_MOOS_2020$HI_ymin < 0 & HI_FI_NO3_MOOS_2020$HI_ymax > 0 & HI_FI_NO3_MOOS_2020$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2020$HI_ymin < 0 & HI_FI_NO3_MOOS_2020$HI_ymax > 0 & HI_FI_NO3_MOOS_2020$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2020 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_fDOM_MOOS_2020$Hyst_index))
which(HI_FI_fDOM_MOOS_2020$HI_ymin < 0 & HI_FI_fDOM_MOOS_2020$HI_ymax > 0 & HI_FI_fDOM_MOOS_2020$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2020$HI_ymin < 0 & HI_FI_fDOM_MOOS_2020$HI_ymax > 0 & HI_FI_fDOM_MOOS_2020$Hyst_index < 0)

#SPC
HI_FI_SPC_MOOS_2020 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_SPC_MOOS_2020$Hyst_index))
which(HI_FI_SPC_MOOS_2020$HI_ymin < 0 & HI_FI_SPC_MOOS_2020$HI_ymax > 0 & HI_FI_SPC_MOOS_2020$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2020$HI_ymin < 0 & HI_FI_SPC_MOOS_2020$HI_ymax > 0 & HI_FI_SPC_MOOS_2020$Hyst_index < 0)

#turb
HI_FI_turb_MOOS_2020 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_turb_MOOS_2020$Hyst_index))
which(HI_FI_turb_MOOS_2020$HI_ymin < 0 & HI_FI_turb_MOOS_2020$HI_ymax > 0 & HI_FI_turb_MOOS_2020$Hyst_index > 0)
which(HI_FI_turb_MOOS_2020$HI_ymin < 0 & HI_FI_turb_MOOS_2020$HI_ymax > 0 & HI_FI_turb_MOOS_2020$Hyst_index < 0)

# CARI ####
HI_FI_NO3_CARI_2020 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_NO3_CARI_2020$Hyst_index))
which(HI_FI_NO3_CARI_2020$HI_ymin < 0 & HI_FI_NO3_CARI_2020$HI_ymax > 0 & HI_FI_NO3_CARI_2020$Hyst_index > 0)
which(HI_FI_NO3_CARI_2020$HI_ymin < 0 & HI_FI_NO3_CARI_2020$HI_ymax > 0 & HI_FI_NO3_CARI_2020$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2020 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_fDOM_CARI_2020$Hyst_index))
which(HI_FI_fDOM_CARI_2020$HI_ymin < 0 & HI_FI_fDOM_CARI_2020$HI_ymax > 0 & HI_FI_fDOM_CARI_2020$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2020$HI_ymin < 0 & HI_FI_fDOM_CARI_2020$HI_ymax > 0 & HI_FI_fDOM_CARI_2020$Hyst_index < 0)

#SPC
HI_FI_SPC_CARI_2020 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_SPC_CARI_2020$Hyst_index))
which(HI_FI_SPC_CARI_2020$HI_ymin < 0 & HI_FI_SPC_CARI_2020$HI_ymax > 0 & HI_FI_SPC_CARI_2020$Hyst_index > 0)
which(HI_FI_SPC_CARI_2020$HI_ymin < 0 & HI_FI_SPC_CARI_2020$HI_ymax > 0 & HI_FI_SPC_CARI_2020$Hyst_index < 0)

#turb
HI_FI_turb_CARI_2020 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_turb_CARI_2020$Hyst_index))
which(HI_FI_turb_CARI_2020$HI_ymin < 0 & HI_FI_turb_CARI_2020$HI_ymax > 0 & HI_FI_turb_CARI_2020$Hyst_index > 0)
which(HI_FI_turb_CARI_2020$HI_ymin < 0 & HI_FI_turb_CARI_2020$HI_ymax > 0 & HI_FI_turb_CARI_2020$Hyst_index < 0)

# POKE ####
HI_FI_NO3_POKE_2020 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_NO3_POKE_2020$Hyst_index))
which(HI_FI_NO3_POKE_2020$HI_ymin < 0 & HI_FI_NO3_POKE_2020$HI_ymax > 0 & HI_FI_NO3_POKE_2020$Hyst_index > 0)
which(HI_FI_NO3_POKE_2020$HI_ymin < 0 & HI_FI_NO3_POKE_2020$HI_ymax > 0 & HI_FI_NO3_POKE_2020$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2020 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_fDOM_POKE_2020$Hyst_index))
which(HI_FI_fDOM_POKE_2020$HI_ymin < 0 & HI_FI_fDOM_POKE_2020$HI_ymax > 0 & HI_FI_fDOM_POKE_2020$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2020$HI_ymin < 0 & HI_FI_fDOM_POKE_2020$HI_ymax > 0 & HI_FI_fDOM_POKE_2020$Hyst_index < 0)

#SPC
HI_FI_SPC_POKE_2020 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_SPC_POKE_2020$Hyst_index))
which(HI_FI_SPC_POKE_2020$HI_ymin < 0 & HI_FI_SPC_POKE_2020$HI_ymax > 0 & HI_FI_SPC_POKE_2020$Hyst_index > 0)
which(HI_FI_SPC_POKE_2020$HI_ymin < 0 & HI_FI_SPC_POKE_2020$HI_ymax > 0 & HI_FI_SPC_POKE_2020$Hyst_index < 0)

#turb
HI_FI_turb_POKE_2020 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_turb_POKE_2020$Hyst_index))
which(HI_FI_turb_POKE_2020$HI_ymin < 0 & HI_FI_turb_POKE_2020$HI_ymax > 0 & HI_FI_turb_POKE_2020$Hyst_index > 0)
which(HI_FI_turb_POKE_2020$HI_ymin < 0 & HI_FI_turb_POKE_2020$HI_ymax > 0 & HI_FI_turb_POKE_2020$Hyst_index < 0)

# STRT ####
HI_FI_NO3_STRT_2020 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_NO3_STRT_2020$Hyst_index))
which(HI_FI_NO3_STRT_2020$HI_ymin < 0 & HI_FI_NO3_STRT_2020$HI_ymax > 0 & HI_FI_NO3_STRT_2020$Hyst_index > 0)
which(HI_FI_NO3_STRT_2020$HI_ymin < 0 & HI_FI_NO3_STRT_2020$HI_ymax > 0 & HI_FI_NO3_STRT_2020$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2020 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_fDOM_STRT_2020$Hyst_index))
which(HI_FI_fDOM_STRT_2020$HI_ymin < 0 & HI_FI_fDOM_STRT_2020$HI_ymax > 0 & HI_FI_fDOM_STRT_2020$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2020$HI_ymin < 0 & HI_FI_fDOM_STRT_2020$HI_ymax > 0 & HI_FI_fDOM_STRT_2020$Hyst_index < 0)

#SPC
HI_FI_SPC_STRT_2020 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_SPC_STRT_2020$Hyst_index))
which(HI_FI_SPC_STRT_2020$HI_ymin < 0 & HI_FI_SPC_STRT_2020$HI_ymax > 0 & HI_FI_SPC_STRT_2020$Hyst_index > 0)
which(HI_FI_SPC_STRT_2020$HI_ymin < 0 & HI_FI_SPC_STRT_2020$HI_ymax > 0 & HI_FI_SPC_STRT_2020$Hyst_index < 0)

#turb
HI_FI_turb_STRT_2020 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_turb_STRT_2020$Hyst_index))
which(HI_FI_turb_STRT_2020$HI_ymin < 0 & HI_FI_turb_STRT_2020$HI_ymax > 0 & HI_FI_turb_STRT_2020$Hyst_index > 0)
which(HI_FI_turb_STRT_2020$HI_ymin < 0 & HI_FI_turb_STRT_2020$HI_ymax > 0 & HI_FI_turb_STRT_2020$Hyst_index < 0)


# VAUL ####
HI_FI_NO3_VAUL_2020 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_NO3_VAUL_2020$Hyst_index))
which(HI_FI_NO3_VAUL_2020$HI_ymin < 0 & HI_FI_NO3_VAUL_2020$HI_ymax > 0 & HI_FI_NO3_VAUL_2020$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2020$HI_ymin < 0 & HI_FI_NO3_VAUL_2020$HI_ymax > 0 & HI_FI_NO3_VAUL_2020$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2020 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_fDOM_VAUL_2020$Hyst_index))
which(HI_FI_fDOM_VAUL_2020$HI_ymin < 0 & HI_FI_fDOM_VAUL_2020$HI_ymax > 0 & HI_FI_fDOM_VAUL_2020$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2020$HI_ymin < 0 & HI_FI_fDOM_VAUL_2020$HI_ymax > 0 & HI_FI_fDOM_VAUL_2020$Hyst_index < 0)

#SPC
HI_FI_SPC_VAUL_2020 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_SPC_VAUL_2020$Hyst_index))
which(HI_FI_SPC_VAUL_2020$HI_ymin < 0 & HI_FI_SPC_VAUL_2020$HI_ymax > 0 & HI_FI_SPC_VAUL_2020$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2020$HI_ymin < 0 & HI_FI_SPC_VAUL_2020$HI_ymax > 0 & HI_FI_SPC_VAUL_2020$Hyst_index < 0)

#turb
HI_FI_turb_VAUL_2020 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_turb_VAUL_2020$Hyst_index))
which(HI_FI_turb_VAUL_2020$HI_ymin < 0 & HI_FI_turb_VAUL_2020$HI_ymax > 0 & HI_FI_turb_VAUL_2020$Hyst_index > 0)
which(HI_FI_turb_VAUL_2020$HI_ymin < 0 & HI_FI_turb_VAUL_2020$HI_ymax > 0 & HI_FI_turb_VAUL_2020$Hyst_index < 0)



















# 2021 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2021 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_NO3_FRCH_2021$Hyst_index))
which(HI_FI_NO3_FRCH_2021$HI_ymin < 0 & HI_FI_NO3_FRCH_2021$HI_ymax > 0 & HI_FI_NO3_FRCH_2021$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2021$HI_ymin < 0 & HI_FI_NO3_FRCH_2021$HI_ymax > 0 & HI_FI_NO3_FRCH_2021$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2021 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_fDOM_FRCH_2021$Hyst_index))
which(HI_FI_fDOM_FRCH_2021$HI_ymin < 0 & HI_FI_fDOM_FRCH_2021$HI_ymax > 0 & HI_FI_fDOM_FRCH_2021$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2021$HI_ymin < 0 & HI_FI_fDOM_FRCH_2021$HI_ymax > 0 & HI_FI_fDOM_FRCH_2021$Hyst_index < 0)

#SPC
HI_FI_SPC_FRCH_2021 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_SPC_FRCH_2021$Hyst_index))
which(HI_FI_SPC_FRCH_2021$HI_ymin < 0 & HI_FI_SPC_FRCH_2021$HI_ymax > 0 & HI_FI_SPC_FRCH_2021$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2021$HI_ymin < 0 & HI_FI_SPC_FRCH_2021$HI_ymax > 0 & HI_FI_SPC_FRCH_2021$Hyst_index < 0)

#turb
HI_FI_turb_FRCH_2021 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_turb_FRCH_2021$Hyst_index))
which(HI_FI_turb_FRCH_2021$HI_ymin < 0 & HI_FI_turb_FRCH_2021$HI_ymax > 0 & HI_FI_turb_FRCH_2021$Hyst_index > 0)
which(HI_FI_turb_FRCH_2021$HI_ymin < 0 & HI_FI_turb_FRCH_2021$HI_ymax > 0 & HI_FI_turb_FRCH_2021$Hyst_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2021 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_NO3_MOOS_2021$Hyst_index))
which(HI_FI_NO3_MOOS_2021$HI_ymin < 0 & HI_FI_NO3_MOOS_2021$HI_ymax > 0 & HI_FI_NO3_MOOS_2021$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2021$HI_ymin < 0 & HI_FI_NO3_MOOS_2021$HI_ymax > 0 & HI_FI_NO3_MOOS_2021$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2021 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_fDOM_MOOS_2021$Hyst_index))
which(HI_FI_fDOM_MOOS_2021$HI_ymin < 0 & HI_FI_fDOM_MOOS_2021$HI_ymax > 0 & HI_FI_fDOM_MOOS_2021$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2021$HI_ymin < 0 & HI_FI_fDOM_MOOS_2021$HI_ymax > 0 & HI_FI_fDOM_MOOS_2021$Hyst_index < 0)

#SPC
HI_FI_SPC_MOOS_2021 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_SPC_MOOS_2021$Hyst_index))
which(HI_FI_SPC_MOOS_2021$HI_ymin < 0 & HI_FI_SPC_MOOS_2021$HI_ymax > 0 & HI_FI_SPC_MOOS_2021$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2021$HI_ymin < 0 & HI_FI_SPC_MOOS_2021$HI_ymax > 0 & HI_FI_SPC_MOOS_2021$Hyst_index < 0)

#turb
HI_FI_turb_MOOS_2021 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_turb_MOOS_2021$Hyst_index))
which(HI_FI_turb_MOOS_2021$HI_ymin < 0 & HI_FI_turb_MOOS_2021$HI_ymax > 0 & HI_FI_turb_MOOS_2021$Hyst_index > 0)
which(HI_FI_turb_MOOS_2021$HI_ymin < 0 & HI_FI_turb_MOOS_2021$HI_ymax > 0 & HI_FI_turb_MOOS_2021$Hyst_index < 0)

# CARI ####
HI_FI_NO3_CARI_2021 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_NO3_CARI_2021$Hyst_index))
which(HI_FI_NO3_CARI_2021$HI_ymin < 0 & HI_FI_NO3_CARI_2021$HI_ymax > 0 & HI_FI_NO3_CARI_2021$Hyst_index > 0)
which(HI_FI_NO3_CARI_2021$HI_ymin < 0 & HI_FI_NO3_CARI_2021$HI_ymax > 0 & HI_FI_NO3_CARI_2021$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2021 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_fDOM_CARI_2021$Hyst_index))
which(HI_FI_fDOM_CARI_2021$HI_ymin < 0 & HI_FI_fDOM_CARI_2021$HI_ymax > 0 & HI_FI_fDOM_CARI_2021$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2021$HI_ymin < 0 & HI_FI_fDOM_CARI_2021$HI_ymax > 0 & HI_FI_fDOM_CARI_2021$Hyst_index < 0)

#SPC
HI_FI_SPC_CARI_2021 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_SPC_CARI_2021$Hyst_index))
which(HI_FI_SPC_CARI_2021$HI_ymin < 0 & HI_FI_SPC_CARI_2021$HI_ymax > 0 & HI_FI_SPC_CARI_2021$Hyst_index > 0)
which(HI_FI_SPC_CARI_2021$HI_ymin < 0 & HI_FI_SPC_CARI_2021$HI_ymax > 0 & HI_FI_SPC_CARI_2021$Hyst_index < 0)

#turb
HI_FI_turb_CARI_2021 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_turb_CARI_2021$Hyst_index))
which(HI_FI_turb_CARI_2021$HI_ymin < 0 & HI_FI_turb_CARI_2021$HI_ymax > 0 & HI_FI_turb_CARI_2021$Hyst_index > 0)
which(HI_FI_turb_CARI_2021$HI_ymin < 0 & HI_FI_turb_CARI_2021$HI_ymax > 0 & HI_FI_turb_CARI_2021$Hyst_index < 0)

# POKE ####
HI_FI_NO3_POKE_2021 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_NO3_POKE_2021$Hyst_index))
which(HI_FI_NO3_POKE_2021$HI_ymin < 0 & HI_FI_NO3_POKE_2021$HI_ymax > 0 & HI_FI_NO3_POKE_2021$Hyst_index > 0)
which(HI_FI_NO3_POKE_2021$HI_ymin < 0 & HI_FI_NO3_POKE_2021$HI_ymax > 0 & HI_FI_NO3_POKE_2021$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2021 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_fDOM_POKE_2021$Hyst_index))
which(HI_FI_fDOM_POKE_2021$HI_ymin < 0 & HI_FI_fDOM_POKE_2021$HI_ymax > 0 & HI_FI_fDOM_POKE_2021$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2021$HI_ymin < 0 & HI_FI_fDOM_POKE_2021$HI_ymax > 0 & HI_FI_fDOM_POKE_2021$Hyst_index < 0)

#SPC
HI_FI_SPC_POKE_2021 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_SPC_POKE_2021$Hyst_index))
which(HI_FI_SPC_POKE_2021$HI_ymin < 0 & HI_FI_SPC_POKE_2021$HI_ymax > 0 & HI_FI_SPC_POKE_2021$Hyst_index > 0)
which(HI_FI_SPC_POKE_2021$HI_ymin < 0 & HI_FI_SPC_POKE_2021$HI_ymax > 0 & HI_FI_SPC_POKE_2021$Hyst_index < 0)

#turb
HI_FI_turb_POKE_2021 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_turb_POKE_2021$Hyst_index))
which(HI_FI_turb_POKE_2021$HI_ymin < 0 & HI_FI_turb_POKE_2021$HI_ymax > 0 & HI_FI_turb_POKE_2021$Hyst_index > 0)
which(HI_FI_turb_POKE_2021$HI_ymin < 0 & HI_FI_turb_POKE_2021$HI_ymax > 0 & HI_FI_turb_POKE_2021$Hyst_index < 0)

# STRT ####
HI_FI_NO3_STRT_2021 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_NO3_STRT_2021$Hyst_index))
which(HI_FI_NO3_STRT_2021$HI_ymin < 0 & HI_FI_NO3_STRT_2021$HI_ymax > 0 & HI_FI_NO3_STRT_2021$Hyst_index > 0)
which(HI_FI_NO3_STRT_2021$HI_ymin < 0 & HI_FI_NO3_STRT_2021$HI_ymax > 0 & HI_FI_NO3_STRT_2021$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2021 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_fDOM_STRT_2021$Hyst_index))
which(HI_FI_fDOM_STRT_2021$HI_ymin < 0 & HI_FI_fDOM_STRT_2021$HI_ymax > 0 & HI_FI_fDOM_STRT_2021$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2021$HI_ymin < 0 & HI_FI_fDOM_STRT_2021$HI_ymax > 0 & HI_FI_fDOM_STRT_2021$Hyst_index < 0)

#SPC
HI_FI_SPC_STRT_2021 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_SPC_STRT_2021$Hyst_index))
which(HI_FI_SPC_STRT_2021$HI_ymin < 0 & HI_FI_SPC_STRT_2021$HI_ymax > 0 & HI_FI_SPC_STRT_2021$Hyst_index > 0)
which(HI_FI_SPC_STRT_2021$HI_ymin < 0 & HI_FI_SPC_STRT_2021$HI_ymax > 0 & HI_FI_SPC_STRT_2021$Hyst_index < 0)

#turb
HI_FI_turb_STRT_2021 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_turb_STRT_2021$Hyst_index))
which(HI_FI_turb_STRT_2021$HI_ymin < 0 & HI_FI_turb_STRT_2021$HI_ymax > 0 & HI_FI_turb_STRT_2021$Hyst_index > 0)
which(HI_FI_turb_STRT_2021$HI_ymin < 0 & HI_FI_turb_STRT_2021$HI_ymax > 0 & HI_FI_turb_STRT_2021$Hyst_index < 0)


# VAUL ####
HI_FI_NO3_VAUL_2021 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_NO3_VAUL_2021$Hyst_index))
which(HI_FI_NO3_VAUL_2021$HI_ymin < 0 & HI_FI_NO3_VAUL_2021$HI_ymax > 0 & HI_FI_NO3_VAUL_2021$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2021$HI_ymin < 0 & HI_FI_NO3_VAUL_2021$HI_ymax > 0 & HI_FI_NO3_VAUL_2021$Hyst_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2021 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_fDOM_VAUL_2021$Hyst_index))
which(HI_FI_fDOM_VAUL_2021$HI_ymin < 0 & HI_FI_fDOM_VAUL_2021$HI_ymax > 0 & HI_FI_fDOM_VAUL_2021$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2021$HI_ymin < 0 & HI_FI_fDOM_VAUL_2021$HI_ymax > 0 & HI_FI_fDOM_VAUL_2021$Hyst_index < 0)

#SPC
HI_FI_SPC_VAUL_2021 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_SPC_VAUL_2021$Hyst_index))
which(HI_FI_SPC_VAUL_2021$HI_ymin < 0 & HI_FI_SPC_VAUL_2021$HI_ymax > 0 & HI_FI_SPC_VAUL_2021$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2021$HI_ymin < 0 & HI_FI_SPC_VAUL_2021$HI_ymax > 0 & HI_FI_SPC_VAUL_2021$Hyst_index < 0)

#turb
HI_FI_turb_VAUL_2021 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_turb_VAUL_2021$Hyst_index))
which(HI_FI_turb_VAUL_2021$HI_ymin < 0 & HI_FI_turb_VAUL_2021$HI_ymax > 0 & HI_FI_turb_VAUL_2021$Hyst_index > 0)
which(HI_FI_turb_VAUL_2021$HI_ymin < 0 & HI_FI_turb_VAUL_2021$HI_ymax > 0 & HI_FI_turb_VAUL_2021$Hyst_index < 0)


















# Figuring out how many days are missing from the record  ####
# 2018 ####
MOOS_2018 <- read.csv("~/Documents/DOD_2018_repo/EXO_data/from_internal_harddrive/processed/EXO_MOOS_final_formatted.csv")
FRCH_2018 <- read.csv("~/Documents/DOD_2018_repo/EXO_data/from_internal_harddrive/processed/EXO_FRCH_final_formatted.csv")

# converting to datetime 
MOOS_2018$datetimeAK <- ymd_hms(MOOS_2018$datetimeAK)
FRCH_2018$datetimeAK <- ymd_hms(FRCH_2018$datetimeAK)

#plot
ggplot(FRCH_2018, aes(x = datetimeAK, y = fDOM.QSU.mn.adj)) +
  geom_point()

#  MOOS
MOOS_fDOM <- MOOS_2018[,-c(1:14, 16,17) ]
MOOS_SPC <- MOOS_2018[,-c(1:14, 17,18) ]
MOOS_turb <- MOOS_2018[,-c(1:14, 16,18) ]

#  FRCH
FRCH_2018$DOY <- yday(FRCH_2018$datetimeAK)
FRCH_fDOM <- FRCH_2018[,-c(1:14, 16,17) ]
FRCH_SPC <- FRCH_2018[,-c(1:14, 17,18) ]
FRCH_turb <- FRCH_2018[,-c(1:14, 16,18) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_turb$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate #
FRCH_SUNA <- read.csv("~/Documents/DOD_2018_repo/SUNA_data/from_internal_harddrive/processed/FRCH_SUNA_means_detailed_clean.csv")
MOOS_SUNA <- read.csv("~/Documents/DOD_2018_repo/SUNA_data/from_internal_harddrive/processed/MOOS_SUNA_means_detailed_clean.csv")

# converting to datetime 
FRCH_SUNA$datetimeAK <- ymd_hms(FRCH_SUNA$datetimeAK)
MOOS_SUNA$datetimeAK <- ymd_hms(MOOS_SUNA$datetimeAK)

#  FRCH
FRCH_SUNA$DOY <- yday(FRCH_SUNA$datetimeAK)
FRCH_NO3 <- FRCH_SUNA[,-c(3:25) ]

#  MOOS
MOOS_SUNA$DOY <- yday(MOOS_SUNA$datetimeAK)
MOOS_NO3 <- MOOS_SUNA[,-c(3:25) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(MOOS_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# 2019 ####
POKE_2019 <- read.csv("~/Documents/DoD_2019/EXO_processed/POKE_EXO_stitched_formatted.csv")
STRT_2019 <- read.csv("~/Documents/DoD_2019/EXO_processed/STRT_EXO_stitched_formatted.csv")
VAUL_2019 <- read.csv("~/Documents/DoD_2019/EXO_processed/VAUL_EXO_stitched_formatted.csv")
MOOS_2019 <- read.csv("~/Documents/DoD_2019/EXO_processed/MOOS_EXO_stitched_formatted.csv")
FRCH_2019 <- read.csv("~/Documents/DoD_2019/EXO_processed/FRCH_EXO_stitched_formatted.csv")

# converting to datetime 
POKE_2019$datetimeAK <- ymd_hms(POKE_2019$datetimeAK)
STRT_2019$datetimeAK <- ymd_hms(STRT_2019$datetimeAK)
VAUL_2019$datetimeAK <- ymd_hms(VAUL_2019$datetimeAK)
MOOS_2019$datetimeAK <- ymd_hms(MOOS_2019$datetimeAK)
FRCH_2019$datetimeAK <- ymd_hms(FRCH_2019$datetimeAK)

#plot
ggplot(FRCH_2019, aes(x = datetimeAK, y = FRCH_2019$fDOM.QSU)) +
  geom_point()

# POKE
POKE_2019$DOY <- yday(POKE_2019$datetimeAK)
POKE_fDOM <- POKE_2019[,-c(1:6,8:20) ]
POKE_SPC <- POKE_2019[,-c(1:12, 14:20) ]
POKE_turb <- POKE_2019[,-c(1:15, 17:20) ]

# STRT
STRT_2019$DOY <- yday(STRT_2019$datetimeAK)
STRT_fDOM <- STRT_2019[,-c(1:6,8:20) ]
STRT_SPC <- STRT_2019[,-c(1:12, 14:20) ]
STRT_turb <- STRT_2019[,-c(1:15, 17:20) ]

# VAUL
VAUL_2019$DOY <- yday(VAUL_2019$datetimeAK)
VAUL_fDOM <- VAUL_2019[,-c(1:6,8:20) ]
VAUL_SPC <- VAUL_2019[,-c(1:12, 14:20) ]
VAUL_turb <- VAUL_2019[,-c(1:15, 17:20) ]

#  MOOS
MOOS_2019$DOY <- yday(MOOS_2019$datetimeAK)
MOOS_fDOM <- MOOS_2019[,-c(1:6,8:20) ]
MOOS_SPC <- MOOS_2019[,-c(1:12, 14:20) ]
MOOS_turb <- MOOS_2019[,-c(1:15, 17:20) ]

#  FRCH
FRCH_2019$DOY <- yday(FRCH_2019$datetimeAK)
FRCH_fDOM <- FRCH_2019[,-c(1:6,8:20) ]
FRCH_SPC <- FRCH_2019[,-c(1:12, 14:20) ]
FRCH_turb <- FRCH_2019[,-c(1:15, 17:20) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_turb$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in seconds
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate
POKE_SUNA <- read.csv("~/Documents/DoD_2019/SUNA_processed/POKE_SUNA_burst_detailed_clean.csv")
STRT_SUNA <- read.csv("~/Documents/DoD_2019/SUNA_processed/STRT_SUNA_burst_detailed_clean.csv")
VAUL_SUNA <- read.csv("~/Documents/DoD_2019/SUNA_processed/VAUL_SUNA_burst_detailed_clean.csv")
MOOS_SUNA <- read.csv("~/Documents/DoD_2019/SUNA_processed/MOOS_SUNA_burst_detailed_clean.csv")
FRCH_SUNA <- read.csv("~/Documents/DoD_2019/SUNA_processed/FRCH_SUNA_burst_detailed_clean.csv")

# converting to datetime 
POKE_SUNA$datetimeAK <- ymd_hms(POKE_SUNA$datetimeAK)
STRT_SUNA$datetimeAK <- ymd_hms(STRT_SUNA$datetimeAK)
VAUL_SUNA$datetimeAK <- ymd_hms(VAUL_SUNA$datetimeAK)
MOOS_SUNA$datetimeAK <- ymd_hms(MOOS_SUNA$datetimeAK)
FRCH_SUNA$datetimeAK <- ymd_hms(FRCH_SUNA$datetimeAK)

#plot
ggplot(POKE_NO3, aes(x = datetimeAK, y = POKE_NO3$nitrateuM)) +
  geom_point()


# POKE
POKE_SUNA$DOY <- yday(POKE_SUNA$datetimeAK)
POKE_NO3 <- POKE_SUNA[,-c(1:3,5:29, 31:33) ]

# STRT
STRT_SUNA$DOY <- yday(STRT_SUNA$datetimeAK)
STRT_NO3 <- STRT_SUNA[,-c(1:3,5:29, 31:33) ]

# VAUL
VAUL_SUNA$DOY <- yday(VAUL_SUNA$datetimeAK)
VAUL_NO3 <- VAUL_SUNA[,-c(1:3,5:29, 31:33) ]

# MOOS
MOOS_SUNA$DOY <- yday(MOOS_SUNA$datetimeAK)
MOOS_NO3 <- MOOS_SUNA[,-c(1:3,5:29, 31:33) ]

# FRCH
FRCH_SUNA$DOY <- yday(FRCH_SUNA$datetimeAK)
FRCH_NO3 <- FRCH_SUNA[,-c(1:3,5:29, 31:33) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in seconds
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# 2020 ####
POKE_2020 <- read.csv("~/Documents/DoD_2020_Alex/EXO_processed/POKE.EXO.cl.csv")
STRT_2020 <- read.csv("~/Documents/DoD_2020_Alex/EXO_processed/STRT.EXO.cl.csv")
VAUL_2020 <- read.csv("~/Documents/DoD_2020_Alex/EXO_processed/VAUL.EXO.cl.csv")
MOOS_2020 <- read.csv("~/Documents/DoD_2020_Alex/EXO_processed/MOOS.EXO.cl.csv")
FRCH_2020 <- read.csv("~/Documents/DoD_2020_Alex/EXO_processed/FRCH.EXO.cl.csv")

# converting to datetime 
POKE_2020$datetimeAK <- ymd_hms(POKE_2020$datetimeAK)
STRT_2020$datetimeAK <- ymd_hms(STRT_2020$datetimeAK)
VAUL_2020$datetimeAK <- ymd_hms(VAUL_2020$datetimeAK)
MOOS_2020$datetimeAK <- ymd_hms(MOOS_2020$datetimeAK)
FRCH_2020$datetimeAK <- ymd_hms(FRCH_2020$datetimeAK)



#plot
ggplot(FRCH_2020, aes(x = datetimeAK, y = FRCH_2020$fDOM.QSU.mn)) +
  geom_point()


# POKE
POKE_2020$DOY <- yday(POKE_2020$datetimeAK)
POKE_fDOM <- POKE_2020[,-c(1:3,5:21,23:28) ]
POKE_SPC <- POKE_2020[,-c(1:11,13:21,23:38) ]
POKE_turb <- POKE_2020[,-c(1:13, 15:21,23:28) ]

# STRT
STRT_2020$DOY <- yday(STRT_2020$datetimeAK)
STRT_fDOM <- STRT_2020[,-c(1:3,5:21,23:28) ]
STRT_SPC <- STRT_2020[,-c(1:11,13:21,23:38) ]
STRT_turb <- STRT_2020[,-c(1:13, 15:21,23:28) ]

# VAUL
VAUL_2020$DOY <- yday(VAUL_2020$datetimeAK)
VAUL_fDOM <- VAUL_2020[,-c(1:3,5:21,23:28) ]
VAUL_SPC <- VAUL_2020[,-c(1:11,13:21,23:38) ]
VAUL_turb <- VAUL_2020[,-c(1:13, 15:21,23:28) ]

#  MOOS
MOOS_2020$DOY <- yday(MOOS_2020$datetimeAK)
MOOS_fDOM <- MOOS_2020[,-c(1:3,5:21,23:28) ]
MOOS_SPC <- MOOS_2020[,-c(1:11,13:21,23:38) ]
MOOS_turb <- MOOS_2020[,-c(1:13, 15:21,23:28) ]

#  FRCH
FRCH_2020$DOY <- yday(FRCH_2020$datetimeAK)
FRCH_fDOM <- FRCH_2020[,-c(1:3,5:21,23:28) ]
FRCH_SPC <- FRCH_2020[,-c(1:11,13:21,23:38) ]
FRCH_turb <- FRCH_2020[,-c(1:13, 15:21,23:28) ]


# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_turb$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate 
POKE_SUNA <- read_csv("~/Documents/DoD_2020_Alex/SUNA_processed/POKE_SUNA_means_detailed_clean.csv")
STRT_SUNA <- read_csv("~/Documents/DoD_2020_Alex/SUNA_processed/STRT_SUNA_means_detailed_clean.csv")
VAUL_SUNA <- read_csv("~/Documents/DoD_2020_Alex/SUNA_processed/VAUL_SUNA_means_detailed_clean.csv")
MOOS_SUNA <- read_csv("~/Documents/DoD_2020_Alex/SUNA_processed/MOOS_SUNA_means_detailed_clean.csv")
FRCH_SUNA <- read_csv("~/Documents/DoD_2020_Alex/SUNA_processed/FRCH_SUNA_means_detailed_clean.csv")

# converting to datetime 
POKE_SUNA$datetimeAK <- ymd_hms(POKE_SUNA$datetimeAK)
STRT_SUNA$datetimeAK <- ymd_hms(STRT_SUNA$datetimeAK)
VAUL_SUNA$datetimeAK <- ymd_hms(VAUL_SUNA$datetimeAK)
MOOS_SUNA$datetimeAK <- ymd_hms(MOOS_SUNA$datetimeAK)
FRCH_SUNA$datetimeAK <- ymd_hms(FRCH_SUNA$datetimeAK)


#plot
ggplot(POKE_SUNA, aes(x = datetimeAK, y = POKE_SUNA$nitrateuM.adj.mn)) +
  geom_point()

# POKE
POKE_SUNA$DOY <- yday(POKE_SUNA$datetimeAK)
POKE_NO3 <- POKE_SUNA[,-c(3:27) ]

# STRT
STRT_SUNA$DOY <- yday(STRT_SUNA$datetimeAK)
STRT_NO3 <- STRT_SUNA[,-c(3:27) ]

# VAUL
VAUL_SUNA$DOY <- yday(VAUL_SUNA$datetimeAK)
VAUL_NO3 <- VAUL_SUNA[,-c(3:27) ]

# MOOS
MOOS_SUNA$DOY <- yday(MOOS_SUNA$datetimeAK)
MOOS_NO3 <- MOOS_SUNA[,-c(3:27) ]

# FRCH
FRCH_SUNA$DOY <- yday(FRCH_SUNA$datetimeAK)
FRCH_NO3 <- FRCH_SUNA[,-c(3:27) ]


# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]



# 2021 ####
EXO_processed <- read_csv("~/Documents/DoD_2021/EXO_data/from_internal_harddrive/processed/EXO.processed.csv")

POKE_2021 <- subset(EXO_processed, site.ID == "POKE")
STRT_2021 <- subset(EXO_processed, site.ID == "STRT")
VAUL_2021 <- subset(EXO_processed, site.ID == "VAUL")
MOOS_2021 <- subset(EXO_processed, site.ID == "MOOS")
FRCH_2021 <- subset(EXO_processed, site.ID == "FRCH")

#plot
ggplot(POKE_2021, aes(x = datetimeAK, y = POKE_2021$fDOM.QSU)) +
  geom_point()


# POKE
POKE_2021$DOY <- yday(POKE_2021$datetimeAK)
POKE_fDOM <- POKE_2021[,-c(1:6,8:22) ]
POKE_SPC <- POKE_2021[,-c(1:13,15:22) ]
POKE_turb <- POKE_2021[,-c(1:15, 17:22) ]

# STRT
STRT_2021$DOY <- yday(STRT_2021$datetimeAK)
STRT_fDOM <- STRT_2021[,-c(1:6,8:22) ]
STRT_SPC <- STRT_2021[,-c(1:13,15:22) ]
STRT_turb <- STRT_2021[,-c(1:15, 17:22) ]

# VAUL
VAUL_2021$DOY <- yday(VAUL_2021$datetimeAK)
VAUL_fDOM <- VAUL_2021[,-c(1:6,8:22) ]
VAUL_SPC <- VAUL_2021[,-c(1:13,15:22) ]
VAUL_turb <- VAUL_2021[,-c(1:15, 17:22) ]

#  MOOS
MOOS_2021$DOY <- yday(MOOS_2021$datetimeAK)
MOOS_fDOM <- MOOS_2021[,-c(1:6,8:22) ]
MOOS_SPC <- MOOS_2021[,-c(1:13,15:22) ]
MOOS_turb <- MOOS_2021[,-c(1:15, 17:22) ]

#  FRCH
FRCH_2021$DOY <- yday(FRCH_2021$datetimeAK)
FRCH_fDOM <- FRCH_2021[,-c(1:6,8:22) ]
FRCH_SPC <- FRCH_2021[,-c(1:13,15:22) ]
FRCH_turb <- FRCH_2021[,-c(1:15, 17:22) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_fDOM$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate 
SUNA_processed <- read_csv("~/Documents/DoD_2021/SUNA_data/from_internal_harddrive/processed/SUNA.processed.csv")

POKE_SUNA <- subset(SUNA_processed, site.ID == "POKE")
STRT_SUNA <- subset(SUNA_processed, site.ID == "STRT")
VAUL_SUNA <- subset(SUNA_processed, site.ID == "VAUL")
MOOS_SUNA <- subset(SUNA_processed, site.ID == "MOOS")
FRCH_SUNA <- subset(SUNA_processed, site.ID == "FRCH")

# POKE
POKE_NO3 <- POKE_SUNA[,-c(1:5,7:292) ]

# STRT
STRT_NO3 <- STRT_SUNA[,-c(1:5,7:292) ]

# VAUL
VAUL_NO3 <- VAUL_SUNA[,-c(1:5,7:292) ]

# MOOS
MOOS_NO3 <- MOOS_SUNA[,-c(1:5,7:292) ]

# FRCH
FRCH_NO3 <- FRCH_SUNA[,-c(1:5,7:292) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# Duration 
AMC <- read_csv("Output_from_analysis/08_Catchment_characteristics/Antecedent_HI_BETA_Catchment.csv")
View(Antecedent_HI_BETA_Catchment)

mean_duration <-  AMC %>% 
  group_by(storm.ID, site.ID, year) %>% 
  dplyr::summarise(Duration = mean(TOTAL.TIME))

mean_duration <- mean_duration %>%
  mutate(across(c(Duration), 
                ~ifelse(Duration > 500, NA, .)))
range(mean_duration$Duration, na.rm = TRUE)
mean(mean_duration$Duration, na.rm = TRUE) #65.40994
65.40994/24 # 2.725414

sd(mean_duration$Duration, na.rm = TRUE) # 53.486
53.486/24



mean_duration_site <- AMC %>% 
  group_by(site.ID) %>% 
  dplyr::summarise(Duration = mean(TOTAL.TIME, na.rm = TRUE))



#### MEAN SOLUTE CONCENTRATIONS ####
DOD_2018 <- read.csv("~/Documents/Storms_clean_repo/Q/Q_chem/DOD.2018.csv")
DOD_2019 <- read.csv("~/Documents/Storms_clean_repo/Q/Q_chem/DOD.2019.csv")
DOD_2019 <- DOD_2019[,-1]
DOD_2020 <- read.csv("~/Documents/Storms_clean_repo/Q/Q_chem/DOD.2020.csv")
DOD_2020 <- DOD_2020[,-1]
colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "Q", "day")
names(DOD_2020)<- colNames # renaming columns 
DOD_2021 <- read.csv("~/Documents/Storms_clean_repo/Q/Q_chem/DOD.2021.csv")
DOD_2021 <- DOD_2021[,-1]

DOD_2018$year <- "2018"
DOD_2019$year <- "2019"
DOD_2020$year <- "2020"
DOD_2021$year <- "2021"

DOD_2021$datetimeAK <- mdy_hm(DOD_2021$datetimeAK)
DOD_2021$datetimeAK <- as.character(DOD_2021$datetimeAK)

DOD_chem <- rbind(DOD_2018, DOD_2019, DOD_2020, DOD_2021)

# read in Caribou data 
CARI_2018 <- read_csv("~/Documents/NEON/CARI/WaterQuality2018.csv", 
                      col_types = cols(NO3 = col_double()))
CARI_2018$Discharge <- NA
CARI_2018 <- CARI_2018[, c(1,2,8,3,4,5,6,7)] # reorganizing column headers

CARI_2019 <- read_csv("~/Documents/NEON/CARI/NEON_Q_WaterQuality2019.csv", 
                                    col_types = cols(fDOM = col_double(), 
                                                     SPC = col_double(), Turb = col_double()))
CARI_2019 <- CARI_2019[,-c(4)]
names(CARI_2019)[names(CARI_2019) == 'site.ID.x'] <- 'site.ID'
CARI_2019$site.ID <- "CARI"

CARI_2020 <- read_csv("~/Documents/NEON/CARI/NEON_Q_WaterQuality2020.csv", 
                                    col_types = cols(site.ID.y = col_double(), 
                                                     NO3 = col_double(), fDOM = col_double(), 
                                                     SPC = col_double(), Turb = col_double()))
CARI_2020 <- CARI_2020[,-c(4)]
names(CARI_2020)[names(CARI_2020) == 'site.ID.x'] <- 'site.ID'
CARI_2020$site.ID <- "CARI"

CARI_2021 <- read_csv("~/Documents/NEON/CARI/NEON_Q_WaterQuality2021.csv", 
                                     col_types = cols(NO3 = col_double(), 
                                                      fDOM = col_double(), SPC = col_double(), 
                                                      Turb = col_double()))
CARI_2021 <- CARI_2021[,-c(4)]
names(CARI_2021)[names(CARI_2021) == 'site.ID.x'] <- 'site.ID'
CARI_2021$site.ID <- "CARI"


CARI_2018$year <- "2018"
CARI_2019$year <- "2019"
CARI_2020$year <- "2020"
CARI_2021$year <- "2021"

CARI_chem <- rbind(CARI_2018, CARI_2019, CARI_2020, CARI_2021)
CARI_chem$day <- as.character(CARI_chem$DateTimeAK)
CARI_chem <- CARI_chem[, c(2,1,5,6,7,4,3,9,8)] # reorganizing column headers


colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "Q", "day", "year")
names(CARI_chem)<- colNames # renaming columns 

# # plotting to see if they came out correctly
# # CARI 
# chem.2021.long <- CARI_2021 %>%
#   pivot_longer(
#     cols = NO3:Turb,
#     names_to = "response_var",
#     values_to = "concentration",
#     values_drop_na = TRUE
#   ) # converting to a long format so each response_var is within a single column
# 
# 
# 
# ggplot(chem.2021.long, aes(x = DateTimeAK, y = concentration, color = site.ID)) +
#   geom_point(size = 0.5) +
#   scale_color_manual(values=c("#3288BD")) +
#   facet_wrap(~response_var, scales = "free") +
#   theme_classic()


# merge CARI and DOD sites 
CARI_chem$datetimeAK <- ymd_hms(CARI_chem$datetimeAK)
DOD_chem$datetimeAK <- ymd_hms(DOD_chem$datetimeAK)

DOD_chem <- rbind(DOD_chem, CARI_chem)
DOD_chem <- DOD_chem[order(DOD_chem$datetimeAK),]
DOD_chem$julian <- yday(DOD_chem$datetimeAK)

# check for outliers
which(DOD_chem$Turb > 1500)
DOD_chem <- DOD_chem[-c(94689, 207878, 213192, 213198, 213474, 213480, 268683), ]

# plotting to make sure this merged properly
chem.long <- DOD_chem %>%
     pivot_longer(
       cols = fDOM:NO3,
       names_to = "response_var",
       values_to = "concentration",
       values_drop_na = TRUE) # converting to a long format so each response_var is within a single column

ggplot(chem.long, aes(x = julian, y = concentration, color = site.ID)) +
  geom_point(size = 0.5) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_grid(response_var~year, scales = "free")

# Filtering by year to compare concentrations across years 

chem_2018 <- subset(DOD_chem, year == "2018")
chem_2019 <- subset(DOD_chem, year == "2019")
chem_2020 <- subset(DOD_chem, year == "2020")
chem_2021 <- subset(DOD_chem, year == "2021")

# The common window for time since peak chena is day 35-142 so the dates are as follows:
# these dates are in the Summart_statistics csv summary file 
# 2018 TPC: 5/23
# 2019 TPC: 5/12
# 2020 TPC: 5/13
# 2021 TPC: 5/8

chem_2018 <- subset(chem_2018, datetimeAK > "2018-06-27" & datetimeAK < "2018-10-12")
chem_2019 <- subset(chem_2019, datetimeAK > "2019-06-16" & datetimeAK < "2019-10-01")
chem_2020 <- subset(chem_2020, datetimeAK > "2020-06-17" & datetimeAK < "2020-09-30")
chem_2021 <- subset(chem_2021, datetimeAK > "2021-06-12" & datetimeAK < "2021-09-27")

# make a julian day columnn:
chem_2018$julian <- yday(chem_2018$datetimeAK)
chem_2018$TSC <- chem_2018$julian-143 # TSC column

chem_2019$julian <- yday(chem_2019$datetimeAK)
chem_2019$TSC <- chem_2019$julian-132 # TSC column

chem_2020$julian <- yday(chem_2020$datetimeAK)
chem_2020$TSC <- chem_2020$julian-134 # TSC column

chem_2021$julian <- yday(chem_2021$datetimeAK)
chem_2021$TSC <- chem_2021$julian-128 # TSC column

# combine them to be able to plot it 
similar_chem_year <- rbind(chem_2018, chem_2019, chem_2020, chem_2021)

CARI_year <- subset(similar_chem_year, site.ID == "CARI")
FRCH_year <- subset(similar_chem_year, site.ID == "FRCH")
MOOS_year <- subset(similar_chem_year, site.ID == "MOOS")
POKE_year <- subset(similar_chem_year, site.ID == "POKE")
STRT_year <- subset(similar_chem_year, site.ID == "STRT")
VAUL_year <- subset(similar_chem_year, site.ID == "VAUL")

similar_chem_year$day <- as.Date(similar_chem_year$datetimeAK)

similar_chem_year[, 6][similar_chem_year[, 6] < 10] <- NA # there are values of 0 for nitrate
  # in VAUL data starting in August that are not accurate so I am setting them to NA
# Mean daily concentration 
mean_daily <- similar_chem_year %>% 
  group_by(day, site.ID, year) %>% 
  summarise(dailyfDOM = mean(fDOM, na.rm = TRUE),
            dailyNO3 = mean(NO3, na.rm = TRUE),
            dailySPC = mean(SPC, na.rm = TRUE),
            dailyTurb = mean(Turb, na.rm = TRUE))

mean_daily$julian <- yday(mean_daily$day)
mean_daily$TSC <- NA
mean_daily[c(1:290), 9] <- mean_daily[c(1:290), 8] - 143 # 2018 
mean_daily[c(291:928), 9] <- mean_daily[c(291:928), 8] - 132 # 2019
mean_daily[c(929:1541), 9] <- mean_daily[c(929:1541), 8] - 134 # 2020
mean_daily[c(1542:2074), 9] <- mean_daily[c(1542:2074), 8] - 128 # 2021

mean_daily <- mean_daily[-2130, ] # last row
write.csv(mean_daily, "~/Documents/Storms_clean_repo/Output_from_analysis/08_Catchment_characteristics/mean_daily.csv")
# plot
# across all years 
# NO3
ggplot(mean_daily, aes(x = site.ID, y = dailyNO3, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()
# fDOM
ggplot(mean_daily, aes(x = site.ID, y = dailyfDOM, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()

# SPC
ggplot(mean_daily, aes(x = site.ID, y = dailySPC, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()

# turb
ggplot(mean_daily, aes(x = site.ID, y = dailyTurb, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()

# by year 
# NO3
ggplot(mean_daily, aes(x = site.ID, y = dailyNO3, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# fDOM
ggplot(mean_daily, aes(x = site.ID, y = dailyfDOM, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# SPC
ggplot(mean_daily, aes(x = site.ID, y = dailySPC, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# turb
ggplot(mean_daily, aes(x = site.ID, y = dailyTurb, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

## Time Series ##
# NO3-
ggplot(mean_daily, aes(x = TSC, y = dailyNO3, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# fDOM
ggplot(mean_daily, aes(x = TSC, y = dailyfDOM, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# SPC
ggplot(mean_daily, aes(x = TSC, y = dailySPC, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# turb
ggplot(mean_daily, aes(x = TSC, y = dailyTurb, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# Output tables for summary tables #
write.table(mean_daily, file = "chem_year_similar.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.cari <- CARI_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))

write.table(year.cari, file = "cari.csv", sep = ",", col.names = NA,
            qmethod = "double")


year.frch <- FRCH_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))

write.table(year.frch, file = "frch.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.moos <- MOOS_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.moos, file = "moos.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.poke <- POKE_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.poke, file = "poke.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.vaul <- VAUL_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.vaul, file = "vaul.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.strt <- STRT_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.strt, file = "strt.csv", sep = ",", col.names = NA,
            qmethod = "double")


### ANOVA ####
#fDOM

fDOM <- aov(dailyfDOM ~ site.ID*year, data = mean_daily)

summary(fDOM) # everything is significant 

TukeyHSD(fDOM, which = "site.ID") # this shows that STRT and CARI are the only ones not sig
    # different from each other 
plot(fDOM)

#NO3
NO3 <- aov(dailyNO3 ~ site.ID*year, data = mean_daily)

summary(NO3) # Site and year and Site*year is significant 


TukeyHSD(NO3, which = "site.ID")
# this shows that POKE and CARI are not significantly different neither is STRT/MOOS
plot(NO3)

# SPC
SPC <- aov(dailySPC ~ site.ID*year, data = mean_daily)

summary(SPC) # Site and year and Site*year is significant 


TukeyHSD(SPC, which = "site.ID")
# this shows that POKE/MOOS are not significantly different neither is STRT/MOOS
plot(SPC)

# turb
turb <- aov(dailyTurb ~ site.ID*year, data = mean_daily)

summary(turb) # Site and year and Site*year is significant 


TukeyHSD(turb, which = "site.ID")
# this shows that POKE/CARI are not significantly different neither is MOOS/FRCH, STRT/CARI,
      #VAUL/FRCH, VAUL/MOOS,STRT/POKE
plot(turb)


#mean_daily <- na.omit(mean_daily)


