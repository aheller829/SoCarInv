# Analyze SRC data

library(tidyverse)
library(ggplot2)



# Set working directory (select folder where SRC files are stored)
setwd("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SRC\\Clean")
# Check to make working directory was set correctly
getwd()


# Read in cleaned data files
src18 <- read.csv("Clean_FID18_SRC.csv")
src19 <- read.csv("Clean_FID19_SRC.csv")
src20 <- read.csv("Clean_FID20_SRC.csv")
src21 <- read.csv("Clean_FID21_SRC.csv")
src22 <- read.csv("Clean_FID22_SRC.csv")
src23 <- read.csv("Clean_FID23_SRC.csv")
src24 <- read.csv("Clean_FID24_SRC.csv")
src25 <- read.csv("Clean_FID18_SRC.csv")
src26 <- read.csv("Clean_FID26_SRC.csv")
src27 <- read.csv("Clean_FID27_SRC.csv")
src28 <- read.csv("Clean_FID28_SRC.csv")
src33 <- read.csv("Clean_FID33_SRC.csv")
src34 <- read.csv("Clean_FID34_SRC.csv")
src35 <- read.csv("Clean_FID35_SRC.csv")
src36 <- read.csv("Clean_FID36_SRC.csv")
src37 <- read.csv("Clean_FID37_SRC.csv")
src38 <- read.csv("Clean_FID38_SRC.csv")
src39 <- read.csv("Clean_FID39_SRC.csv")
src40 <- read.csv("Clean_FID40_SRC.csv")
src41 <- read.csv("Clean_FID41_SRC.csv")
src42 <- read.csv("Clean_FID42_SRC.csv")
src43 <- read.csv("Clean_FID43_SRC.csv")
src44 <- read.csv("Clean_FID44_SRC.csv")


# Join tables and compare across sites
srcs <- src18 %>%
  rbind(src19) %>%
  rbind(src20) %>%
  rbind(src21) %>%
  rbind(src22) %>%
  rbind(src23) %>%
  rbind(src24) %>%
  rbind(src25) %>%
  rbind(src26) %>%
  rbind(src27) %>%
  rbind(src28) %>%
  rbind(src33) %>%
  rbind(src34) %>%
  rbind(src35) %>%
  rbind(src36) %>%
  rbind(src37) %>%
  rbind(src38) %>%
  rbind(src39) %>%
  rbind(src40) %>%
  rbind(src41) %>%
  rbind(src42) %>%
  rbind(src43) %>%
  rbind(src44)






# Join ESG/recovery class to SRC data
plotchar <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\PlotChar.csv")
esg <- dplyr::select(plotchar, FID, ESG, Recovery.Class, Cover, Treatment, Inside.Outside)
esg$Recovery.Class <- round(esg$Recovery.Class, 0)
esg <- tidyr::unite(esg, ESG.RC, ESG, Recovery.Class, sep = ".", remove = FALSE)
esg$FID <- as.factor(esg$FID)

# Make recovery class proper data type for analysis
srcs$FID <- as.factor(srcs$FID)
srcs <- dplyr::left_join(srcs, esg)
srcs$Recovery.Class <- as.factor(srcsv$Recovery.Class)
unique(srcs$Recovery.Class)
srcs$Recovery.Class <- factor(srcs$Recovery.Class,levels = c("1", "2", "3", "4", "5"))


# Select final SRC reading from each plot
srcsslice <- srcs %>%
  dplyr::group_by(FID, Plot.No.) %>%
  dplyr::slice(which.max(DT)) %>%
  dplyr::ungroup()



# Make a boxplot to see how much variability there is at a site (between plots)
ggplot(srcsslice, aes(x = FID, y = SRL_Rate, fill = FID)) +
  geom_boxplot() 


# Remove outlier plot FID 22 (measurements taken at night)
srcsslice <- dplyr::filter(srcsslice, FID != 22)

# Make a boxplot to see how much variability there is at a site (between plots)
ggplot(srcsslice, aes(x = FID, y = SRL_Rate, fill = FID)) +
  geom_boxplot() 



# Get average by plot and site (single mean SRC reading for each site)
srcsvar <- srcsslice %>%
  dplyr::group_by(FID) %>%
  dplyr::summarise(MeanSRL = mean(SRL_Rate),
                   SDSRL = sd(SRL_Rate),
                   MeanSRQ = mean(SRQ_Rate),
                   SDSRQ = sd(SRQ_Rate))




# Box plots by state
str(esg)
str(srcsvar)
srcsvar$FID <- as.factor(srcsvar$FID)
srcsvar <- dplyr::left_join(srcsvar, esg)
srcsvar$Recovery.Class <- as.factor(srcsvar$Recovery.Class)


colors <- c("blue", "green3","yellow", "orange", "orangered")


esg3 <- dplyr::filter(srcsvar, ESG == "ESG 3")

ggplot(esg3, aes(x = Recovery.Class, y = MeanSRL, fill = Recovery.Class)) +
  scale_fill_manual(values = colors) +
  geom_boxplot() +
  labs(y = bquote('Average soil respiration rate (g'~m^2~hr^1*')'),
       x = "Recovery class") +
  ggtitle(bquote('Soil respiration across states, ESG 3')) +
  theme(legend.position = "none", 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank())


esg5 <- dplyr::filter(srcsvar, ESG == "ESG 5")

ggplot(esg5, aes(x = Recovery.Class, y = MeanSRL, fill = Recovery.Class)) +
  scale_fill_manual(values = colors) +
  geom_boxplot() +
  labs(y = bquote('Average soil respiration rate (g'~m^2~hr^1*')'),
       x = "Recovery class") +
  ggtitle("Soil respiration rates in ESG 5") +
  theme(legend.position = "none")



esg8 <- dplyr::filter(srcsvar, ESG == "ESG 8")

ggplot(esg8, aes(x = Recovery.Class, y = MeanSRL, fill = Recovery.Class)) +
  scale_fill_manual(values = colors) +
  geom_boxplot() +
  labs(y = bquote('Average soil respiration rate (g'~m^2~hr^1*')'),
       x = "Recovery class") +
  ggtitle("Soil respiration rates in ESG 8") +
  theme(legend.position = "none")




# By RC, all esgs
ggplot(srcsvar, aes(x = Recovery.Class, y = MeanSRL, fill = Recovery.Class)) +
  scale_fill_manual(values = colors) +
  geom_boxplot() +
  labs(y = bquote('Average soil respiration rate (g'~m^2~hr^1*')'),
       x = "Recovery class") +
  ggtitle("Soil respiration rates, all ESGs") +
  theme(legend.position = "none")






# Plot inside exclosures vs. outside exclosures
pairedplots <- dplyr::filter(srcsvar, Treatment == "Y")

pairedplots %>%
 ggplot(aes(x = Inside.Outside, y = MeanSRL, fill = Inside.Outside)) +
  geom_boxplot() +
  scale_fill_manual(values = c("dodgerblue2", "darkorchid3")) +
  xlab("Treatment") +
  ylab(bquote('Average soil respiration rate (g'~m^2~hr^1*')')) +
  ggtitle("Soil respiration across grazing exclosures") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~ESG)



# Compare SRC reading to moisture/temperature
ggplot(srcsslice, aes(x = SRL_Rate, y = Tsoil)) + 
  geom_point()


ggplot(srcsslice, aes(x = SRL_Rate, y = Tair)) + 
  geom_point()



ggplot(srcsslice, aes(x = SRL_Rate, y = Msoil)) + 
  geom_point()




# Compare with carbon stocks
soc <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SOCstocksoutput.csv")

# Calculate cumulative SOC stock across horizons
totalsoc <- soc %>%
  dplyr::group_by(FID) %>%
  dplyr::summarise(TotalSOCstock = sum(SOCstock))

totalsoc <- dplyr::filter(totalsoc, !is.na(TotalSOCstock))
totalsoc$FID <- as.factor(totalsoc$FID)

srcsvar <- dplyr::left_join(srcsvar, totalsoc)


ggplot(srcsvar, aes(x = MeanSRL, y = TotalSOCstock, label = FID)) + 
  geom_text()








# Plot SRC against SRQ - a big difference between mean SRQ and SRQ could mean that the seal between
# the chamber and the soil was not tight, producing leakage and a non-linear reading
ggplot(srcsslice, aes(x = SRL_Rate, y = Msoil)) + 
  geom_point()

ggplot(srcsvar) +
  geom_segment(aes(x = FID, xend = FID, y=MeanSRL, yend=MeanSRQ), color="grey") +
  geom_point( aes(x=FID, y=MeanSRL), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=FID, y=MeanSRQ), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme(
    legend.position = "none",
  ) +
  xlab("Plot FID") +
  ylab("Soil respiration rate")





