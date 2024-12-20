# Working with CO2 estimates for EcoFarm selected soums
# The CO2 estimates are for grams of CO2 per kilogram of soil (g/kg) from 0-30 cm
# They come from the ESRI hosted raster layer of Soil Grids
# Pixel/cell size is 250 m

library(tidyverse)
library(sf)
library(stringr)
library(gt)
library(gtable)



# Read in zonal stats tables
xushaat1 <- read.csv("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat\\xushaat1p.csv")
xushaat2 <- read.csv("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat\\xushaat2p.csv")



# rbind into single dataframe
stats <- rbind(xushaat1, xushaat2)

# Remove NAs
stats <- na.omit(stats)

# Keep variables of interest
stats <- dplyr::select(stats, Bagh, Property, Depth, ESG = NAME_ENG, Unit,
                              Area = AREA, Mean = MEAN, StD = STD)

unique(stats$ESG)


stats <- stats %>%
  dplyr::mutate(Bagh = ifelse(Bagh == "1p", "1p bagh", "2p bagh"),
                Property = ifelse(Property == "Organic carbon stocks", "Soil organic carbon stocks",
                                  ifelse(Property == "Soil organic carbon", "Soil organic carbon content", Property)),
                ESG = ifelse(ESG == "1. Festuca-Forbs mountain steppe rangeland in Gravelly hills  and fan ESG, Forest steppe", "ESG 1. Festuca-Forbs mountain steppe rangeland",
                      ifelse(ESG == "5. Grass-Forbs riparian rangeland in High water table ESG, Forest steppe", "ESG 5. Grass-Forbs riparian rangeland",
                      ifelse(ESG == "4. Stipa baicalensis-Forbs meadow steppe rangeland in Mountain valley ESG, Forest steppe", "ESG 4. Stipa baicalensis-Forbs meadow steppe rangeland",
                      ifelse(ESG == "3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland in Loamy fan ESG, Forest steppe", "ESG 3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland",
                      ifelse(ESG == "6. Stipa Krylovii-Small bunch grass-Forbs dry steppe rangeland in Gravelly hills and fan ESG, Steppe", "ESG 6. Stipa Krylovii-Small bunch grass-Forbs dry steppe rangeland",
                      ifelse(ESG == "7. Stipa krylovii-grass dry steppe rangeland in Sandy loam alluvial fan and plain ESG, Steppe", "ESG 7. Stipa krylovii-grass dry steppe rangeland", 
                      ifelse(ESG == "2. Small bunch grass-Forbs  mountain steppe rangeland in Loamy fan ESG, Forest steppe", "ESG 2. Small bunch grass-Forbs mountain steppe rangeland", ESG))))))))
  
# Remove cropland and forest
stats <- dplyr::filter(stats, ESG != "Cropland" & ESG != "Forest")



# Make an area table
areas <- stats %>%
  dplyr::select(Bagh, ESG, "Area, ha" = Area) 

areas <- dplyr::distinct(areas)

areas <- dplyr::mutate_if(areas, is.numeric, round, digits = 0)

areas <- dplyr::arrange(areas, ESG)

tab <- gt(areas, rowname_col = "ESG",
          groupname_col = "Bagh")

tab

tab <- tab_style(tab, style = cell_text(weight = "bold"),
                 locations = cells_column_labels())
tab <- tab_style(tab, style = cell_text(weight = "bold"),
                 locations = cells_row_groups())
tab <- tab_header(tab, title = md("**ESG areas by bagh, Xushaat soum, Selenge aimag**"))


tab


gt::gtsave(tab, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\", filename = "selengeareatable.png", vwidth = 1500, vheight = 1000)






names(stats)
str(stats)

# Split by variable and convert by conversion factors as recommended by SoilGrids
unique(stats$Property)

C <- stats %>%
  dplyr::filter(grepl("Soil organic carbon content", Property)) %>%
  dplyr::mutate(Mean = Mean/10,
                StD = StD/10,
                Unit = "g/kg") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::group_by(Bagh, ESG, Property, Unit) %>%
  dplyr::summarise(Mean = mean(Mean),
                   'Standard deviation' = mean(StD)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_if(is.numeric, round, digits = 0)



N <- stats %>%
  dplyr::filter(grepl("Total nitrogen", Property)) %>%
  dplyr::mutate(Mean = Mean/100,
                StD = StD/100,
                Unit = "g/kg") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::group_by(Bagh, ESG, Property, Unit) %>%
  dplyr::summarise(Mean = mean(Mean),
                   'Standard deviation' = mean(StD)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_if(is.numeric, round, digits = 0)





PH <- stats %>%
  dplyr::filter(grepl("Soil pH", Property)) %>%
  dplyr::mutate(Mean = Mean/10,
                StD = StD/10,
                Unit = "pH") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::group_by(Bagh, ESG, Property, Unit) %>%
  dplyr::summarise(Mean = mean(Mean),
                   'Standard deviation' = mean(StD)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_if(is.numeric, round, digits = 0)


SOC <- stats %>%
  dplyr::filter(grepl("Soil organic carbon stocks", Property)) %>%
  dplyr::mutate(Unit = "t/ha") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::select(Bagh, ESG, Property, Unit, Mean, "Standard deviation" = StD)



# Rejoin as single table
statsrecalc <- rbind(C, N)
statsrecalc <- rbind(statsrecalc, PH)
statsrecalc <- rbind(statsrecalc, SOC)

# Add depth
statsrecalc$"Soil depth" <- "0-30 cm"

# Remove Forest/Cropland
statsrecalc <- dplyr::filter(statsrecalc, ESG != "Cropland" & ESG != "Forest")

names(statsrecalc)
statsrecalc <- dplyr::select(statsrecalc, Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation')

# Write to csv
write.csv(statsrecalc, "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat\\xushaat0to30recalc.csv")


# Bagh level summary
bagh <- statsrecalc %>%
  dplyr::group_by(Bagh, Property, Unit) %>%
  dplyr::summarise(Mean = mean(Mean)) %>%
  dplyr::mutate_if(is.numeric, round, digits = 0)

bagh$Property <- paste0(bagh$Property, ", 0-30 cm")

tabp1 <- gt(bagh, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by bagh, Xushaat soum**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))

tabp1 <- tab_footnote(tabp1,
                      footnote = "Averaged for 0-30 cm soil depth",
                      locations = cells_title(groups = "title"))

tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat", filename = "xushaatbaghlevel.png", vwidth = 1500, vheight = 1000)





# Bagh 1

p1 <- dplyr::filter(statsrecalc, Bagh == "1p bagh")

p1 <- dplyr::select(p1, -Bagh)

tabp1 <- gt(p1, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by ESG in 1p bagh, Xushaat soum**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))

tabp1 <- tab_footnote(tabp1,
                      footnote = "Averaged for 0-30 cm soil depth",
                      locations = cells_title(groups = "title"))

tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat", filename = "xushaatpropertiesbagh1.png", vwidth = 1500, vheight = 1000)


# Bagh 2

p1 <- dplyr::filter(statsrecalc, Bagh == "2p bagh")

p1 <- dplyr::select(p1, -Bagh)

tabp1 <- gt(p1, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by ESG in 2p bagh, Xushaat soum**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))
tabp1 <- tab_footnote(tabp1,
                      footnote = "Averaged for 0-30 cm soil depth",
                      locations = cells_title(groups = "title"))

tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat", filename = "xushaatpropertiesbagh2.png", vwidth = 1500, vheight = 1000)





# Recalc but keep separate by depth
unique(stats$Property)

C <- stats %>%
  dplyr::filter(grepl("Soil organic carbon content", Property)) %>%
  dplyr::mutate(Mean = Mean/10,
                StD = StD/10,
                Unit = "g/kg") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


N <- stats %>%
  dplyr::filter(grepl("Total nitrogen", Property)) %>%
  dplyr::mutate(Mean = Mean/100,
                StD = StD/100,
                Unit = "g/kg") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 




PH <- stats %>%
  dplyr::filter(grepl("Soil pH", Property)) %>%
  dplyr::mutate(Mean = Mean/10,
                StD = StD/10,
                Unit = "pH") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::group_by(Bagh, ESG, Property, Unit) 

SOC <- stats %>%
  dplyr::filter(grepl("Soil organic carbon stocks", Property)) %>%
  dplyr::mutate(Unit = "t/ha") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



# Rejoin as single table
statsrecalc <- rbind(C, N)
statsrecalc <- rbind(statsrecalc, PH)
statsrecalc <- rbind(statsrecalc, SOC)

# Remove area, change var names
names(statsrecalc)
statsrecalc <- dplyr::select(statsrecalc, Bagh, ESG, Property, 'Soil depth' = Depth, Unit, Mean, 'Standard deviation' = StD)

statsrecalc <- dplyr::filter(statsrecalc, ESG != "Forest" & ESG != "Cropland")

statsrecalc$`Soil depth` <- paste0(statsrecalc$`Soil depth`, " cm")


write.csv(statsrecalc, "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Xushaat\\xushaat_recalc_sepdepths.csv", row.names = FALSE)



# Variance/CV for sample size
bornuurstats <- dplyr::mutate(bornuurstats, CV = STD/MEAN)
bornuurstats$CVP <- bornuurstats$CV * 100
bornuurstats$CV <- round(bornuurstats$CV, 1)
bornuurstats$CVP <- round(bornuurstats$CVP, 1)











