# Converting sampling requirement calculations from the RECSOIL excel sheet

# Load libraries 
library(tidyverse)
library(gt)
library(gtable)
library(sf)



# Read in Dorgont and Odnoo data and combine into single shapefile

odnoo <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\OdnooTargetedPlots.shp")

dorgont <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\DorgontTargeted.shp")

names(odnoo)
names(dorgont)

odnoo <- dplyr::select(odnoo, PlotName, Year, ESG, RC, Longitude, Latitude)

dorgont <- tidyr::separate(dorgont, State, into = c("ESG", "State"), "[.]")
dorgont <- dplyr::select(dorgont, PlotName = Plot_numbe, ESG, RC = Recovery_c, Longitude, Latitude)
dorgont$Year <- 2024

SteppePlots <- rbind(dorgont, odnoo)

sf::st_write(SteppePlots, "C:\\Users\\TRAVAH\\Documents\\GIS\\SteppeMonitoringPlots.shp", driver = 'ESRI Shapefile', append = FALSE)








plotchar <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Field Data\\plotchar.csv")
names(plotchar)

plotsum <- plotchar %>%
  dplyr::group_by(ESG, Recovery.Class) %>%
  dplyr::summarise(Count = n())


# Make tables
alreadysampled <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Sample design\\PlotNeeds_alreadysampled.csv")
soums <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Sample design\\PlotNeeds_soums.csv")
esgs <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Sample design\\PlotNeeds_targetESGs.csv")

alreadysampled <- dplyr::rename(alreadysampled, "Recovery Class" = Recovery.Class,
                                "Number of Plots" = Number.of.Plots)

soums <- dplyr::rename(soums, "ESG 3" = ESG.3, "ESG 5" = ESG.5, "ESG 7" = ESG.7, "ESG 9" = ESG.9)


esgs <- dplyr::rename(esgs, "Natural Zone" = Natural.Zone, "Recovery Class" = Recovery.Class,
                      "Already Collected" = Already.Collected)


tab <- gt(esgs)
tab

tab<- cols_align(tab, align = "center", columns = everything())
tab <- tab_header(tab, title = md("**Sample Sites Needed by ESG and Recovery Class**"))
tab <- tab_style(tab, style = cell_text(weight = "bold"),
                locations = cells_column_labels())
tab


gt::gtsave(tab, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Sample design\\", filename = "SitesNeeded.png", vwidth = 1500, vheight = 1000)





# Read in data
ests <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\Aimag CO2 estimates\\dornod_co2.csv")

ests <- na.omit(ests)

calc <- ifelse(!is.na(max(2, if(ests$AREA > 0) max(1, ceiling(ests$AL[19])) else "")), 
       max(2, if(data$C[19] > 0) max(1, ceiling(data$AL[19])) else ""),
       "")

calc <- dplyr::mutate(ests, PlotsPerStratum = ifelse(!is.na(max)))