
# Analyzing carbon stocks

library(tidyverse)
library(ggplot2)
library(ggpattern)

c <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\LabData_C.csv")

bd <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\LabData_BD.csv")

soilchar <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SoilChar.csv")


# Equation 1: BD finesoil = Oven Dry Weight/Volume = BD whole*(1-R)

# Equation 2: SOC stock = (OrganicC/100) * BD * (1-R) * (HorizonThickness*100)

names(bd)
# Calculate mass fraction (rock fragments)
bd <- dplyr::mutate(bd, MassFraction = ((Rock_g*100)/TotalWetWeight)/100)
bd <- dplyr::mutate(bd, MassFraction = ifelse(is.nan(MassFraction), 0, MassFraction))

# Find average bulk density by site and horizon
avgbd <- bd %>%
  dplyr::group_by(FID, Site, HorizonNumber) %>%
  dplyr::summarise(AvgBD = mean(VolumetricWeight_gcm3),
                   AvgMassFraction = mean(MassFraction)) 


# Join carbon and bulk density into a single table 
names(c)
soiljoin <- dplyr::left_join(c, avgbd)


# Add horizon thickness
soiljoin$HorizonThickness <- soiljoin$HorizonEnd - soiljoin$HorizonStart



# Calculate SOC stocks 
soiljoin <- dplyr::mutate(soiljoin, SOCstock = (OrganicC/100) * AvgBD * (1-AvgMassFraction) * (HorizonThickness) * (100))



write.csv(soiljoin, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\CSV\\SOCstocksoutput.csv", row.names = FALSE)

















