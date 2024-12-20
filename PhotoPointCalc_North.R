# Clean shapefiles

# Load libraries
library(tidyverse)
library(sf)
library(sp)
library(gt)
library(readr)

# Read in and merge shapefiles so they can be symbolized together
# First, photo monitoring points
s_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\SelengePhotoPointsESGs.shp")
ae_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\AlagErdenePhotoPointsESGs.shp")

names(s_soum)
names(ae_soum)

s_soum$X2024_rc <- NA
s_soum <- dplyr::select(s_soum, F1, Soum_name, Land_name = Land_ngme, X2018_rc:X2023_rc, X2024_rc, Name_MNG)

ae_soum <- dplyr::select(ae_soum, F1 = F_, Soum_name, Land_name, X2018_rc:X2024_rc, Name_MNG)

allphotopoints <- rbind(s_soum, ae_soum)
plot(allphotopoints)

# Add numeric ESG column
allphotopoints$ESG <- as.numeric(gsub("\\D", "", allphotopoints$Name_MNG))

# Drop geometry
allphotopoints <- sf::st_drop_geometry(allphotopoints)

# Filter out forest
allphotopoints <- dplyr::filter(allphotopoints, !is.na(ESG))

names(allphotopoints)
dclasses <- allphotopoints %>%
  dplyr::group_by(Soum_name, ESG, X2023_rc) %>%
  dplyr::summarize(ClassCount = n()) %>%
  dplyr::ungroup()



# Add numeric state
dclasses <- dplyr::mutate(dclasses, State = ifelse(X2023_rc == "I", 1,
                                                   ifelse(X2023_rc == "II", 2,
                                                          ifelse(X2023_rc == "III", 3,
                                                                 ifelse(X2023_rc == "IV", 4,
                                                                        ifelse(X2023_rc == "V", 5, NA))))))


dclasses <- dplyr::select(dclasses, Soum_name, ESG, State, PhotoPoints = ClassCount)
dclasses <- dplyr::arrange(dclasses, ESG, State)


table <- gt::gt(dclasses)

table

table <- 
  table |>
  tab_header(
    title = "Photo Points by ESG, State, and Location")

table

gt::gtsave(table, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "NorthernSoumsExistingPointsbyESG.png", vwidth = 1500, vheight = 1000)


# Photopoints by selected ESGs
abtab <- dplyr::filter(dclasses, ESG == 1 | ESG == 3)
abtab <- dplyr::ungroup(abtab)
abtab <- dplyr::arrange(abtab , ESG, State)

abtable <- gt::gt(abtab)
abtable

abtable <- 
  abtable |>
  tab_header(
    title = "Photo Points in ESGs 1,3")

abtable

abtable <-
  cols_align(
    abtable,
    align = "center",
    columns = everything()
  )

abtable

names(dclasses)

gt::gtsave(abtable, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "NorthernSelectedESGphotopoints.png", vwidth = 1500, vheight = 1000)




# Table of photopoints by ESG/state, no soum location
str(abtab)
abnogeog <- abtab %>%
  dplyr::select(ESG, State, PhotoPoints) %>%
  dplyr::group_by(ESG, State) %>%
  dplyr::summarise(ExistingPlots = sum(PhotoPoints)) %>%
  dplyr::ungroup()

# By group, no state
plots_esgs <- abnogeog %>%
  dplyr::group_by(ESG) %>%
  dplyr::summarise(Sum = sum(ExistingPlots)) %>%
  dplyr::ungroup()


# Soum, no state
abtab <- dplyr::filter(dclasses, ESG == 1 | ESG == 3)
abtab <- dplyr::ungroup(abtab)
abtab <- dplyr::select(abtab, -State)
abtab <- dplyr::arrange(abtab , ESG)

esg_soum <- abtab %>%
  dplyr::group_by(Soum_name, ESG) %>%
  dplyr::summarise(PhotoPoints = sum(PhotoPoints)) %>%
  dplyr::ungroup()

abtable <- 
  abtable |>
  tab_header(
    title = "Photo Points in ESGs 1,3")

abtable

abtable <-
  cols_align(
    abtable,
    align = "center",
    columns = everything()
  )

abtable

names(dclasses)

gt::gtsave(abtable, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "NorthernSelectedESGphotopoints.png", vwidth = 1500, vheight = 1000)

