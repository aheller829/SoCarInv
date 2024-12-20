# Make MS tables
library(tidyverse)
library(gt)
library(gtable)
library(sf)

production <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\production.csv")
names(production)
production <- dplyr::rename(production, "Ecological Zone" = Natural.Zone, "State Number" = State.Number,
                            "Recovery Class" = Recovery.class, "State Name" = State.Name, 
                            "Production min (kg/ha)" = Production.min..kg.ha., 
                            "Production max (kg/ha)" = Production.max..kg.ha.)
ptab <- gt(production)

ptab

ptab <- ptab |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md("**Average Annual Plant Production by Ecological Site Groups and States**")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
 # ) |> 
 # tab_style(
  #  style = cell_borders(sides = c("bottom"), weight = px(0.3)),
  #  locations = cells_body(rows = c(3, 7, 11, 14))
#  ) |> 
#  tab_style(
#    style = cell_borders(sides = c("top"), weight = px(0.3)),
 #   locations = cells_body(rows = c(1))
  )
  



ptab


gt::gtsave(ptab, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "ProductionTable.png", vwidth = 1500, vheight = 1000)




# Carbon estimates

co2 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\studyco2est.csv")
names(co2)
co2 <- dplyr::rename(co2, "Ecological Zone" = Natural.Zone, "ESG Name" = ESG.Name,
                            "Minimum SOC (t/ha)" = Minimum.estimated.CO2, "Maximum SOC (t/ha)" = Maximum.estimated.CO2, 
                            "Mean SOC (t/ha)" = Mean.estimated.CO2, 
                            "Median SOC (t/ha)" = Median.estimated.CO2,
                            "Standard Deviation SOC (t/ha)" = Standard.deviation.estimated.CO2)
ctab <- gt(co2)

ctab

ctab <- ctab |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md("**Estimated Soil Organic Carbon (SOC) Stocks**")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_footnote(
    footnote = "Poggio et al. 2021",
    locations = cells_title(groups = "title")
  )





ctab


gt::gtsave(ctab, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "CarbonEst.png", vwidth = 1500, vheight = 1000)







# Sample plot needs tables 
# Read in and merge shapefiles so they can be symbolized together
# First, photo monitoring points
tv_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\Tuvshinshiree\\Tuvshinshiree_Photomonitoring_point.shp")
k_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\4 soums shape file\\Khulunbuir_soum\\Khulunbuir_Photomonitoring_point_map.shp")
ts_soum <-sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\4 soums shape file\\Tsagaan-Ovoo_soum\\Tsagaan-Ovoo_Photomonitoring_point.shp")
tu_soum <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\4 soums shape file\\Tumentsogt_soum\\Tumentsogt_Photomonitoring_point.shp")
nabu <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\NABUphoto_esgs.shp")

# Actually just run the PhotoPointCalc_East file and use dclasses table for eastern soums

names(k_soum)
names(ts_soum)
names(tu_soum)
names(tv_soum)
names(nabu)


# Clean tables so they can be bound together
# Select desired variables, incluing making a most recent recovery class column
k_soum <- dplyr::select(k_soum, ID, Aimag_name = 2, Soum_name = 3, Name_ENG = 9, RecentRC = F13)

ts_soum <- dplyr::select(ts_soum, ID, Aimag_name = 2, Soum_name = 3, Name_ENG = 9, RecentRC = F13)

tu_soum <- dplyr::select(tu_soum, ID, Aimag_name = 2, Soum_name = 3, Name_ENG = 9, RecentRC = F13)

tv_soum <- dplyr::select(tv_soum, ID, Aimag_name = 2, Soum_name = 3, Name_ENG = 9, RecentRC = F13)


names(tv_soum)
names(nabu)
names(dclasses)


nabu <- dplyr::select(nabu, ESG = RASTERVALU, State = StateNm, Soum, Aimag)
nabu <- sf::st_drop_geometry(nabu)

nabu <- nabu %>%
  dplyr::group_by(Aimag, Soum, ESG, State) %>%
  dplyr::summarise(PhotoPoints = n()) %>%
  dplyr::ungroup()

names(dclasses)
names(nabu)



# Photopoints by selected ESGs
dclasses <- allphotopoints
abtab <- dplyr::filter(dclasses, ESG == 9 | ESG == 6 | ESG == 1 | ESG == 3 | ESG == 5)
abtab <- dplyr::ungroup(abtab)
abtab <- dplyr::arrange(abtab, ESG, State)

abtable <- gt::gt(abtab)
abtable

abtable <- 
  abtable |>
  tab_header(
    title = "Photo Points in ESGs 1, 3, 6, 9, 5")

abtable

abtable <-
  cols_align(
    abtable,
    align = "center",
    columns = everything()
  )

abtable

names(dclasses)

gt::gtsave(abtable, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "WholeStudySelectESGphotopointsbySoum.png", vwidth = 1500, vheight = 1000)




# Table of photpoints by ESG/state, no soum location
str(abtab)

central <- c("Arkhangai", "Uvurkhangai")
east <- c("Dornod", "Sukhbaatar")


abtab <- abtab %>%
  dplyr::mutate(Region = ifelse(Aimag %in% central, "Central",
                                ifelse(Aimag %in% east, "East", NA))) 



abnogeog <- abtab %>%
  dplyr::select(Region, ESG, State, PhotoPoints) %>%
  dplyr::group_by(Region, ESG, State) %>%
  dplyr::summarise(ExistingPlots = sum(PhotoPoints)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Region, ESG, State)





# Rename ESGs
abnogeog <- dplyr::mutate(abnogeog, ESG = ifelse(ESG == 1, "1: Festuca-forbs",
                                                                  ifelse(ESG == 3, "3: Forbs-grass-carex",
                                                        ifelse(ESG == 6, "6: Stipa krylovii",
                                                               ifelse(ESG == 9, "9: Stipa grandis", 
                                                                      ifelse(ESG == 5, "5: Grass-forbs", NA))))))

write.csv(abnogeog, "Outputs//abnogeog.csv", row.names = FALSE)

abnogeog <- read.csv("Outputs//abnogeog.csv")

abnogeog <- dplyr::mutate(abnogeog, Targeted = ifelse(ExistingPlots > 2, 0, 
                                                          3 - ExistingPlots))

names(abnogeog)

abnogeog <- dplyr::select(abnogeog, Region, ESG, "Recovery Class" = State, "Existing Points" = ExistingPlots, "Targeted Points" = Targeted)


plottab <- gt(abnogeog)
plottab <- 
  plottab |>
  tab_header(title = md("**Soil Carbon Sampling Points**")
             ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  cols_align(
    align = "center",
    columns = everything())
  
plottab


gt::gtsave(plottab, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "PlotNeeds.png", vwidth = 1500, vheight = 1000)



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


