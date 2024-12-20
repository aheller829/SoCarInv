
library(tidyverse)
library(ggplot2)
library(ggpattern)
library(aqp)
library(sf)


# Read in carbon data, soil characteristics, and plot characteristics
soilchar <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\Field Data\\CSV\\SoilChar.csv")
plotchar <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\Field Data\\CSV\\PlotChar.csv")
soc <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\Field Data\\CSV\\SOCstocksoutput.csv")


# Count how many sites were visted in each ESG
esgcount <- plotchar %>%
  dplyr::group_by(ESG) %>%
  dplyr::summarise(Count = n())




# Select desired variables dataframes
names(soilchar)
soilchar <- dplyr::select(soilchar, FID, HorizonNumber, Texture, Eff, Hue, Value, Chroma)

names(plotchar)
plotchar <- dplyr::select(plotchar, Site, FID, Latitude, Longitude, Elevation, Slope, Cover,
                          ESG, Recovery.Class, Treatment, Inside.Outside, Grazed, Haymaking)

names(soc)

# Calculate cumulative SOC stock from 0-30 cm
totalsoc <- soc %>%
  dplyr::group_by(FID) %>%
  dplyr::summarise(TotalSOCstock = sum(SOCstock))

totalsoc <- dplyr::filter(totalsoc, !is.na(TotalSOCstock))


# Join SOC stock with plot characteristic data
soiljoin <- plotchar %>%
  dplyr::left_join(totalsoc)







# Look at BD values across recovery classes
soiljoin %>%
  dplyr::filter(HorizonNumber ==  2 & ESG == "ESG 5") %>%
  ggplot(aes(x = Recovery.Class, y = AvgBD, fill = Recovery.Class)) +
  geom_boxplot() +
  labs(x = "Recovery class",
       y = bquote('Mean bulk density (g/'~cm^3*')')) +
  ggtitle("Bulk density in ESG 5, 0-10 cm") +
  scale_fill_manual(values = c("green3", "yellow3", "darkorange")) +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


# Plot BD against SOC stock
soiljoin %>%
  dplyr::filter(ESG == "ESG 8" | ESG == "ESG 3" | ESG == "ESG 5") %>%
ggplot(aes(x = TotalSOCstock, y = AvgBD, color = ESG)) + 
  geom_point() +
  facet_wrap(~ESG)
# Vague subtle pattern of increased SOC with lower BD






# Commpare inside/outside plots
pairedplots <- dplyr::filter(soiljoin, Treatment == "Y")

pairedplots %>%
ggplot(aes(x = Inside.Outside, y = TotalSOCstock, fill = Inside.Outside)) +
  geom_boxplot() +
  scale_fill_manual(values = c("dodgerblue2", "darkorchid3")) +
  xlab("Treatment") +
  ylab("SOC stock (t C/ha)") +
  ggtitle("SOC stocks across grazing exclosures") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0,60),
                     breaks = seq(0,60,10)) +
  facet_wrap(~ESG)



pairedplots %>%
  ggplot(aes(fill = Inside.Outside, y = TotalSOCstock, x = ESG)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("dodgerblue2", "darkorchid3"), name = "Treatment type") +
  xlab("Ecological site group") +
  ylab("SOC stock (t C/ha)") +
  ggtitle("SOC stocks across grazing exclosures") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0,70),
                     breaks = seq(0,70,10))




# Find averages
SOC_esgavg <- soiljoin %>%
  dplyr::filter(!is.na(SOCstock)) %>%
  dplyr::group_by(ESG) %>%
  dplyr::summarise(MeanSOC = mean(SOCstock),
                   SDSOC = sd(SOCstock),
                   MinSOC = min(SOCstock),
                   MaxSOC = max(SOCstock))





# Barplot
esg3 <- dplyr::filter(soiljoin, ESG == "ESG 3")
esg3sum <- esg3 %>%
  dplyr::group_by(Recovery.Class) %>%
  dplyr::summarise(MeanSOC = mean(TotalSOCstock))

esg3sum %>%
  ggplot(aes(fill = Recovery.Class, y = MeanSOC, x = Recovery.Class)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("green3", "yellow3", "darkorange")) +
  xlab("Recovery class") +
  ylab("Mean SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESG 3") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


# Boxplot
ggplot(esg3, aes(x = Recovery.Class, y = TotalSOCstock, fill = Recovery.Class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("green3", "yellow3", "darkorange")) +
  xlab("Recovery class") +
  ylab("SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESG 3") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0,50,10)) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed", color = "red")


# ESG 5
esg5 <- dplyr::filter(soiljoin, ESG == "ESG 5")
esg5 <- dplyr::ungroup(esg5)


esg5$HorizonNumber <- factor(esg5$HorizonNumber, levels = c("1", "2"), 
                             labels = c("0-10 cm", "10-30 cm"))

esg5 <- dplyr::filter(esg5, !is.na(HorizonNumber))


esg5sum <- esg5 %>%
  dplyr::group_by(Recovery.Class) %>%
  dplyr::summarise(MeanSOC = mean(TotalSOCstock))

# Barplot
esg5 %>%
  ggplot(aes(fill = Recovery.Class, y = TotalSOCstock, x = Recovery.Class)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("green3", "yellow3", "darkorange")) +
  xlab("Recovery class") +
  ylab("Mean SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESG 5") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 




# Boxplot
ggplot(esg5, aes(x = Recovery.Class, y = TotalSOCstock, fill = Recovery.Class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("green3", "yellow3", "darkorange")) +
  xlab("Recovery class") +
  ylab("SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESG 3") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0,50,10)) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed", color = "red")


# ESG 8
esg8 <- dplyr::filter(soiljoin, ESG == "ESG 8")
esg8 <- dplyr::ungroup(esg8)


esg8$HorizonNumber <- factor(esg8$HorizonNumber, levels = c("1", "2"), 
                             labels = c("0-10 cm", "10-30 cm"))



esg8sum <- esg8 %>%
  dplyr::filter(!is.na(TotalSOCstock)) %>%
  dplyr::group_by(Recovery.Class) %>%
  dplyr::summarise(MeanSOC = mean(TotalSOCstock))



# Barplot
esg8 %>%
  ggplot(aes(fill = Recovery.Class, y = TotalSOCstock, x = Recovery.Class)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("green3", "yellow3", "darkorange", "firebrick1", "firebrick3")) +
  xlab("Recovery class") +
  ylab("Mean SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESG 8") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 



esg8sum %>%
  ggplot(aes(fill = Recovery.Class, y = MeanSOC, x = Recovery.Class)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("green3", "yellow3", "darkorange", "firebrick1", "firebrick3")) +
  xlab("Recovery class") +
  ylab("Mean SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESG 8") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 





# All ESGs boxplot
soiljoin %>%
  dplyr::filter(ESG == "ESG 3" | ESG == "ESG 7" | ESG == "ESG 5" | ESG == "ESG 8") %>%
  dplyr::filter(Site != "Dry Steppe C 1" & Site != "Caragena 1") %>%
  ggplot(aes(x = Recovery.Class, y = TotalSOCstock, fill = Recovery.Class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("green3", "yellow3", "darkorange", "firebrick1", "firebrick3")) +
  xlab("Recovery class") +
  ylab("SOC stock (t C/ha)") +
  ggtitle("SOC stocks across ESGs") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0,60,10)) +
  facet_wrap(~ESG)





# SOC concentration
str(c$HorizonNumber)
str(soiljoin$HorizonNumber)
c <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\LabData_C.csv")
c$HorizonNumber <- as.factor(c$HorizonNumber)

soiljoin <- dplyr::left_join(soiljoin, c)

oc <- soiljoin %>%
  dplyr::group_by(FID) %>%
  dplyr::summarise(TotalC = sum(OrganicC))

soiljoin <- dplyr::left_join(soiljoin, oc)

soiljoin$Recovery.Class <- as.factor(soiljoin$Recovery.Class)

soiljoin %>%
  ggplot(aes(x = Recovery.Class, y = TotalC)) +
  geom_boxplot() + 
  facet_wrap(~ESG)







# Show grazed/ungrazed plots
soiljoin <- tidyr::unite(soiljoin, H_G, HorizonNumber, Grazed, sep = "_", remove = FALSE)
# Change factor levels for legend plotting
unique(soiljoin$H_G)
soiljoin$H_G <- factor(soiljoin$H_G, levels = c("1_N", "2_N", "1_Y", "2_Y"))
unique(soiljoin$ESG)
soiljoin$ESG <- factor(soiljoin$ESG, levels = c("ESG 1", "ESG 2", "ESG 3", "ESG 5", "ESG 7", "ESG 8", "ESG 9"))

soiljoin <- tidyr::unite(soiljoin, ESG_Name, ESG, FID, sep = "_", remove = FALSE)

soiljoin <- dplyr::mutate(soiljoin, Grazed = ifelse(Grazed == "Y", "Yes",
                                                    ifelse(Grazed == "N", "No", NA)))

soiljoin %>%
  dplyr::filter(!is.na(TotalSOCstock) & !is.na(Grazed) & Grazed != "") %>%
  ggplot(aes(fill = Grazed, y = TotalSOCstock, x = reorder(FID, -TotalSOCstock))) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "SOC stocks on grazed and ungrazed sites",
       x = "Site", y = "Average SOC (t C/ha)", fill = "Grazing usage") +
  scale_fill_manual(values = c("chartreuse4", "cyan4", "gray")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())







# Plot carbon by soil texture
soiljoin %>%
  dplyr::filter(!is.na(Texture)) %>%
  ggplot(aes(x = Texture, y = TotalSOCstock, fill = Texture)) +
  geom_boxplot() +
  xlab("Soil texture") +
  ylab("SOC stock (t C/ha)") +
  ggtitle("SOC stocks across soil textures") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


