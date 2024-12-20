# Clean shapefiles

# Load libraries
library(tidyverse)
library(sf)
library(sp)
library(gt)
library(readr)

# Read in and merge shapefiles so they can be symbolized together
# First, photo monitoring points
tv_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\Tuvshinshiree\\Tuvshinshiree_Photomonitoring_point.shp")
k_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\4 soums shape file\\Khulunbuir_soum\\Khulunbuir_Photomonitoring_point_map.shp")
ts_soum <-sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\4 soums shape file\\Tsagaan-Ovoo_soum\\Tsagaan-Ovoo_Photomonitoring_point.shp")
tu_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\4 soums shape file\\Tumentsogt_soum\\Tumentsogt_Photomonitoring_point.shp")


allphotopoints <- rbind(tv_soum, k_soum, ts_soum, tu_soum)
plot(allphotopoints)

# Get rid of extra columns
names(allphotopoints)
allphotopoints <- dplyr::select(allphotopoints, ID:F13, geometry)


# Calculate total area of ESGs
esgs <-  sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\FinalESGMerge.shp")

names(esgs)

areas <- esgs %>%
  dplyr::group_by(Name_ENG) %>%
  dplyr::summarise(ESGarea = sum(Shape_Area))

photos <-  sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\FinalPhotoMonMerge.shp")

names(photos)
photos <- dplyr::rename(photos, AimagMN = 2, SoumMN = 3, ESGMN = 9)

dclasses <- photos %>%
  dplyr::group_by(AimagMN, SoumMN, ESGMN, F13) %>%
  dplyr::summarize(ClassCount = n()) %>%
  dplyr::ungroup()

dclasses <- sf::st_drop_geometry(dclasses)

dclasses <- dplyr::ungroup(dclasses)

# Change soum/aimag names from cyrillic
dclasses <- dplyr::mutate(dclasses, Aimag = ifelse(AimagMN == "Дорнод", "Dornod",
                                            ifelse(AimagMN == "Сүхбаатар", "Sukhbaatar", NA)),
                                    Soum = ifelse(SoumMN == "Хөлөнбуйр", "Khulunbuir",
                                            ifelse(SoumMN == "Цагаан-Овоо", "Tsaagan-Ovoo",
                                                  ifelse(SoumMN == "Түмэнцогт", "Tumensogt",
                                                ifelse(SoumMN == "Түвшинширээ", "Tuvshinshiree", NA)))))

dclasses <- dplyr::mutate(dclasses, ESG = readr::parse_number(as.character(ESGMN)))

names(dclasses)
dclasses <- dplyr::select(dclasses, ESG, F13, 
                          PhotoPoints = ClassCount, Soum, Aimag)

# Add numeric state
dclasses <- dplyr::mutate(dclasses, State = ifelse(F13 == "I", 1,
                                                   ifelse(F13 == "II", 2,
                                                          ifelse(F13 == "III", 3,
                                            ifelse(F13 == "IV", 4,
                                                   ifelse(F13 == "V", 5, NA))))))


dclasses <- dplyr::arrange(dclasses, ESG, State)

dclasses <- dplyr::select(dclasses, ESG, State, PhotoPoints, Soum, Aimag)
                           
table <- gt::gt(dclasses)

table

table <- 
  table |>
  tab_header(
    title = "Photo Points by ESG, State, and Location")

table

gt::gtsave(table, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "EasternSoumsExistingPointsbyESG.png", vwidth = 1500, vheight = 1000)


# Photopoints by selected ESGs
abtab <- dplyr::filter(dclasses, ESG == 9 | ESG == 6)
abtab <- dplyr::ungroup(abtab)
abtab <- dplyr::arrange(abtab , ESG, State)

abtable <- gt::gt(abtab)
abtable

abtable <- 
  abtable |>
  tab_header(
    title = "Photo Points in ESGs 6, 7, 9")

abtable

abtable <-
cols_align(
  abtable,
  align = "center",
  columns = everything()
)

abtable

names(dclasses)

gt::gtsave(abtable, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "EasternSelectedESGphotopoints.png", vwidth = 1500, vheight = 1000)




# Table of photpoints by ESG/state, no soum location
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

gt::gtsave(abnogeog, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "ExistingPointsNoGeog.png", vwidth = 1500, vheight = 1000)


# Read ESG 6 and 9 photomonitoring plots in and select randomly when more than 3
esg9 <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\PhotoPlots9.shp")
esg6 <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\PhotoPlots6.shp")

esg9 <- dplyr::select(esg9, 1:3, 9, 13)
esg6 <- dplyr::select(esg6, 1:3, 9, 13)


groupsums9 <- esg9 %>%
  dplyr::group_by(F13) %>%
  dplyr::summarise(Count = n())

randoms9 <- dplyr::filter(esg9, F13 == "II" | F13 == "III" | F13 == "IV")

randoms9 <- randoms9 %>%
  dplyr::group_by(F13) %>% 
  dplyr::slice_sample(n = 3)

# 6
groupsums6 <- esg6 %>%
  dplyr::group_by(F13) %>%
  dplyr::summarise(Count = n())

randoms6 <- dplyr::filter(esg6, F13 == "II" | F13 == "III")

randoms6 <- randoms6 %>%
  dplyr::group_by(F13) %>% 
  dplyr::slice_sample(n = 3)


# Join
samplepoints <- rbind(randoms6, randoms9)
# Rename variables so it can be saved as shapefile
samplepoints <- dplyr::rename(samplepoints, AimagMN = 2, SoumMN = 3, ESGMN = 4)


samplepoints <- dplyr::mutate(samplepoints, Aimag = ifelse(AimagMN == "Дорнод", "Dornod",
                                                   ifelse(AimagMN == "Сүхбаатар", "Sukhbaatar", NA)),
                          Soum = ifelse(SoumMN == "Хөлөнбуйр", "Khulunbuir",
                                        ifelse(SoumMN == "Цагаан-Овоо", "Tsaagan-Ovoo",
                                               ifelse(SoumMN == "Түмэнцогт", "Tumensogt",
                                                      ifelse(SoumMN == "Түвшинширээ", "Tuvshinshiree", NA)))))

samplepoints <- dplyr::mutate(samplepoints, ESG = readr::parse_number(as.character(ESGMN)))

names(samplepoints)
samplepoints <- dplyr::select(samplepoints, ID, Aimag, Soum, ESG, State = F13, geometry)

sf::st_write(samplepoints, "C:\\Users\\TRAVAH\\Documents\\GIS\\samplepoints6and9.shp", driver = 'ESRI Shapefile')
