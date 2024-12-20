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
bornuur1 <- read.csv("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuur1p.csv")
bornuur2 <- read.csv("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuur2p.csv")
bornuur3 <- read.csv("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuur3p.csv")
bornuur4 <- read.csv("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuur4p.csv")


# rbind into single dataframe
bornuurstats <- rbind(bornuur1, bornuur2)
bornuurstats <- rbind(bornuurstats, bornuur3)
bornuurstats <- rbind(bornuurstats, bornuur4)

# Remove NAs
bornuurstats <- na.omit(bornuurstats)

# Keep variables of interest
bornuurstats <- dplyr::select(bornuurstats, Bagh, Property, ESG = NAME_ENG, Unit,
                              Area = AREA, Mean = MEAN, StD = STD)




# Separate property column
bornuurstats <- tidyr::separate(bornuurstats, Property, c("Property", "Soil Depth"), sep = ",", remove = FALSE)

# Rename fields
names(bornuurstats)
unique(bornuurstats$Bagh)
unique(bornuurstats$Property)
unique(bornuurstats$ESG)

bornuurstats <- bornuurstats %>%
  dplyr::mutate(Bagh = ifelse(Bagh == "1p", "1p bagh",
                      ifelse(Bagh == "2p", "2p bagh",
                      ifelse(Bagh == "3p", "3p bagh", "4p bagh"))),
                Property = ifelse(Property == "Organic carbon stocks",
                                  "Soil organic carbon stocks",
                                  ifelse(Property == "Soil organic carbon", 
                                         "Soil organic carbon content", Property)),
                ESG = ifelse(ESG == "1. Festuca-Forbs mountain steppe rangeland in Gravelly hills  and fan ESG, Forest steppe", "ESG 1. Festuca-Forbs mountain steppe rangeland",
                             ifelse(ESG == "5. Grass-Forbs riparian rangeland in High water table ESG, Forest steppe", "ESG 5. Grass-Forbs riparian rangeland",
                                    ifelse(ESG == "4. Stipa baicalensis-Forbs meadow steppe rangeland in Mountain valley ESG, Forest steppe", "ESG 4. Stipa baicalensis-Forbs meadow steppe rangeland",
                                           ifelse(ESG == "3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland in Loamy fan ESG, Forest steppe", "ESG 3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland",
                                                  ESG)))))



bornuurstats <- dplyr::filter(bornuurstats, ESG != "Cropland" & ESG != "Forest")

# Make an area table
areas <- bornuurstats %>%
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
tab <- tab_header(tab, title = md("**ESG areas by bagh, Bornuur soum, Tuv aimag**"))


tab


gt::gtsave(tab, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\", filename = "bornuurareatable.png", vwidth = 1500, vheight = 1000)




# Bornuur table
names(bornuurstats)
str(bornuurstats)

# Split by variable and convert by conversion factors as recommended by SoilGrids
unique(bornuurstats$Property)

bornuurC <- bornuurstats %>%
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
  


bornuurN <- bornuurstats %>%
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





bornuurPH <- bornuurstats %>%
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


names(bornuurN)

bornuurSOC <- bornuurstats %>%
  dplyr::filter(grepl("Soil organic carbon stocks", Property)) %>%
  dplyr::mutate(Unit = "t/ha") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::select(Bagh, ESG, Property, Unit, Mean, "Standard deviation" = StD)



# Rejoin as single table
bornuurrecalc <- rbind(bornuurC, bornuurN)
bornuurrecalc <- rbind(bornuurrecalc, bornuurPH)
bornuurrecalc <- rbind(bornuurrecalc, bornuurSOC)


# Add depth
bornuurrecalc$"Soil depth" <- "0-30 cm"

bornuurrecalc <- dplyr::select(bornuurrecalc, Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation')

# Write to csv
write.csv(bornuurrecalc, "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuur0to30recalc.csv")


# Bagh level summary
bagh <- bornuurrecalc %>%
  dplyr::group_by(Bagh, Property, Unit) %>%
  dplyr::summarise(Mean = mean(Mean)) %>%
  dplyr::mutate_if(is.numeric, round, digits = 0)

bagh$Property <- paste0(bagh$Property, ", 0-30 cm")

tabp1 <- gt(bagh, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by bagh, Bornuur soum**"))
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


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur", filename = "bornuurbaghlevel.png", vwidth = 1500, vheight = 1000)






# Bagh 1
names(bornuurstats)

p1 <- dplyr::filter(bornuurrecalc, Bagh == "1p bagh")

p1 <- dplyr::select(p1, -Bagh)

tabp1 <- gt(p1, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by ESG in 1p bagh, Bornuur soum**"))
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


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur", filename = "bornuurpropertiesbagh1.png", vwidth = 1500, vheight = 1000)


# Bagh 2

p1 <- dplyr::filter(bornuurrecalc, Bagh == "2p bagh")

p1 <- dplyr::select(p1, -Bagh)

tabp1 <- gt(p1, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by ESG in 2p bagh, Bornuur soum**"))
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


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur", filename = "bornuurpropertiesbagh2.png", vwidth = 1500, vheight = 1000)



# Bagh 3

p1 <- dplyr::filter(bornuurrecalc, Bagh == "3p bagh")

p1 <- dplyr::select(p1, -Bagh)

tabp1 <- gt(p1, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by ESG in 3p bagh, Bornuur soum**"))
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


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur", filename = "bornuurpropertiesbagh3.png", vwidth = 1500, vheight = 1000)



# Bagh 4

p1 <- dplyr::filter(bornuurrecalc, Bagh == "4p bagh")

p1 <- dplyr::select(p1, -Bagh)

tabp1 <- gt(p1, groupname_col = "Property")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil properties by ESG in 4p bagh, Bornuur soum**"))
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


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur", filename = "bornuurpropertiesbagh4.png", vwidth = 1500, vheight = 1000)





# Recalc but keep separate by depth
unique(stats$Property)

C <- bornuurstats %>%
  dplyr::filter(grepl("Soil organic carbon content", Property)) %>%
  dplyr::mutate(Mean = Mean/10,
                StD = StD/10,
                Unit = "g/kg") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


N <- bornuurstats %>%
  dplyr::filter(grepl("Total nitrogen", Property)) %>%
  dplyr::mutate(Mean = Mean/100,
                StD = StD/100,
                Unit = "g/kg") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 




PH <- bornuurstats %>%
  dplyr::filter(grepl("Soil pH", Property)) %>%
  dplyr::mutate(Mean = Mean/10,
                StD = StD/10,
                Unit = "pH") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::group_by(Bagh, ESG, Property, Unit) 

SOC <- bornuurstats %>%
  dplyr::filter(grepl("Soil organic carbon stocks", Property)) %>%
  dplyr::mutate(Unit = "t/ha") %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



# Rejoin as single table
statsrecalc <- rbind(C, N)
statsrecalc <- rbind(statsrecalc, PH)
statsrecalc <- rbind(statsrecalc, SOC)

# Remove area, change var names
names(statsrecalc)
statsrecalc <- dplyr::select(statsrecalc, Bagh, ESG, Property, 'Soil Depth', Unit, Mean, 'Standard deviation' = StD)

statsrecalc <- dplyr::filter(statsrecalc, ESG != "Forest" & ESG != "Cropland")



write.csv(statsrecalc, "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuur_recalc_sepdepths.csv", row.names = FALSE)






# Variance/CV for sample size
names(bornuurrecalc)
str(bornuurrecalc)
bornuurrecalc$CV <- bornuurrecalc$`Standard deviation`/bornuurrecalc$Mean
bornuurrecalc$CVP <- bornuurrecalc$CV * 100
bornuurrecalc$CV <- round(bornuurrecalc$CV, 1)
bornuurrecalc$CVP <- round(bornuurrecalc$CVP, 1)



write.csv(bornuurrecalc, "C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\bornuurCV.csv", row.names = FALSE)


