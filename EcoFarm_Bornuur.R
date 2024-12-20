# Working with CO2 estimates for EcoFarm selected soums
# The CO2 estimates are for grams of CO2 per kilogram of soil (g/kg) from 0-30 cm
# They come from the ESRI hosted raster layer of Soil Grids
# Pixel/cell size is 250 m

library(tidyverse)
library(sf)
library(stringr)
library(gt)
library(gtable)
library(sp)
library(sf)
library(raster)
library(terra)


# Read in ESG bagh polys
esg1p <- terra::vect("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\Bornuur1pESG.shp")
esg2p <- terra::vect("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\Bornuur2pESG.shp")
esg3p <- terra::vect("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\Bornuur3pESG.shp")
esg4p <- terra::vect("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\Bornuur\\Bornuur4pESG.shp")



# Read in rasters
C0to5 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\C0to5.tif")
C5to15 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\C5to15.tif")
C15to30 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\C15to30.tif")

n0to5 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\n0to5.tif")
n5to15 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\n5to15.tif")
n15to30 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\n15to30.tif")

ph0to5 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\ph0to5.tif")
ph5to15 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\ph5to15.tif")
ph15to30 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\ph15to30.tif")

soc0to30 <- raster::raster("C:\\Users\\TRAVAH\\Documents\\GIS\\EcoFarm\\socstock0to30.tif")


# Stack rasters
Cstack <- terra::rast(C0to5, C5to15, C15to30)
Nstack <- terra::rast(n0to5, n5to15, n15to30)
PHstack <- terra::rast(ph0to5, ph5to15, ph15to30)
SOCstack <- terra::rast(soc0to30)

# Calculate cell mean of raster stack
Ccalc <- terra::mean(Cstack, na.rm=TRUE)
Ncalc <- terra::mean(Nstack, na.rm=TRUE)
PHcalc <- terra::mean(PHstack, na.rm=TRUE)


# Calculate mean by bagh and ESG
# Bagh 1
Cmean <- terra::extract(Ccalc, esg1p, method = "simple", fun = mean, bind = TRUE)
Cstd <- terra::extract(Ccalc, esg1p, method = "simple", fun = sd, bind = TRUE)
Cmin <- terra::extract(Ccalc, esg1p, method = "simple", fun = min, bind = TRUE)
Cmax <- terra::extract(Ccalc, esg1p, method = "simple", fun = max, bind = TRUE)

Nmean <- terra::extract(Ncalc, esg1p, method = "simple", fun = mean, bind = TRUE)
Nstd <- terra::extract(Ncalc, esg1p, method = "simple", fun = sd, bind = TRUE)

PHmean <- terra::extract(PHcalc, esg1p, method = "simple", fun = mean, bind = TRUE)
PHstd <- terra::extract(PHcalc, esg1p, method = "simple", fun = sd, bind = TRUE)

SOCmean <- terra::extract(SOCstack, esg1p, method = "simple", fun = mean, bind = TRUE)
SOCstd <- terra::extract(SOCstack, esg1p, method = "simple", fun = sd, bind = TRUE)


Cmean <- Cmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 1p",
                Unit = "dg/kg",
                Property = "Soil organic carbon content",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Cstd <- Cstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Cmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 
Cmin <- Cmin %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, Min = mean) %>%
  dplyr::left_join(Cstd) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation', Min) %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 
Cmax <- Cmax %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, Max = mean) %>%
  dplyr::left_join(Cmin) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation', Min, Max) %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



Nmean <- Nmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100,
                Bagh = "Bagh 1p",
                Unit = "g/kg",
                Property = "Total nitrogen",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Nstd <- Nstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Nmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


PHmean <- PHmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 1p",
                Unit = "pH",
                Property = "Soil pH",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
PHstd <- PHstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(PHmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



SOCmean <- SOCmean %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 1p",
                Unit = "t/ha",
                Property = "Soil organic carbon stocks",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = SOCstock0to30) 
SOCstd <- SOCstd %>%
  as.data.frame() %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = SOCstock0to30) %>%
  dplyr::left_join(SOCmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


bagh1p <- Cstd %>%
  rbind(Nstd) %>%
  rbind(PHstd) %>%
  rbind(SOCstd) %>%
  dplyr::filter(ESG != "Cropland" & ESG != "Forest") 



# Bagh 2
Cmean <- terra::extract(Ccalc, esg2p, method = "simple", fun = mean, bind = TRUE)
Cstd <- terra::extract(Ccalc, esg2p, method = "simple", fun = sd, bind = TRUE)

Nmean <- terra::extract(Ncalc, esg2p, method = "simple", fun = mean, bind = TRUE)
Nstd <- terra::extract(Ncalc, esg2p, method = "simple", fun = sd, bind = TRUE)

PHmean <- terra::extract(PHcalc, esg2p, method = "simple", fun = mean, bind = TRUE)
PHstd <- terra::extract(PHcalc, esg2p, method = "simple", fun = sd, bind = TRUE)

SOCmean <- terra::extract(SOCstack, esg2p, method = "simple", fun = mean, bind = TRUE)
SOCstd <- terra::extract(SOCstack, esg2p, method = "simple", fun = sd, bind = TRUE)


Cmean <- Cmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 2p",
                Unit = "dg/kg",
                Property = "Soil organic carbon content",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Cstd <- Cstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Cmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


Nmean <- Nmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100,
                Bagh = "Bagh 2p",
                Unit = "g/kg",
                Property = "Total nitrogen",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Nstd <- Nstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Nmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


PHmean <- PHmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 2p",
                Unit = "pH",
                Property = "Soil pH",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
PHstd <- PHstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(PHmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



SOCmean <- SOCmean %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 2p",
                Unit = "t/ha",
                Property = "Soil organic carbon stocks",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = SOCstock0to30) 
SOCstd <- SOCstd %>%
  as.data.frame() %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = SOCstock0to30) %>%
  dplyr::left_join(SOCmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


bagh2p <- Cstd %>%
  rbind(Nstd) %>%
  rbind(PHstd) %>%
  rbind(SOCstd) %>%
  dplyr::filter(ESG != "Cropland" & ESG != "Forest") 


# Bagh 3
Cmean <- terra::extract(Ccalc, esg3p, method = "simple", fun = mean, bind = TRUE)
Cstd <- terra::extract(Ccalc, esg3p, method = "simple", fun = sd, bind = TRUE)

Nmean <- terra::extract(Ncalc, esg3p, method = "simple", fun = mean, bind = TRUE)
Nstd <- terra::extract(Ncalc, esg3p, method = "simple", fun = sd, bind = TRUE)

PHmean <- terra::extract(PHcalc, esg3p, method = "simple", fun = mean, bind = TRUE)
PHstd <- terra::extract(PHcalc, esg3p, method = "simple", fun = sd, bind = TRUE)

SOCmean <- terra::extract(SOCstack, esg3p, method = "simple", fun = mean, bind = TRUE)
SOCstd <- terra::extract(SOCstack, esg3p, method = "simple", fun = sd, bind = TRUE)


Cmean <- Cmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 3p",
                Unit = "dg/kg",
                Property = "Soil organic carbon content",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Cstd <- Cstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Cmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


Nmean <- Nmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100,
                Bagh = "Bagh 3p",
                Unit = "g/kg",
                Property = "Total nitrogen",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Nstd <- Nstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Nmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


PHmean <- PHmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 3p",
                Unit = "pH",
                Property = "Soil pH",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
PHstd <- PHstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(PHmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



SOCmean <- SOCmean %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 3p",
                Unit = "t/ha",
                Property = "Soil organic carbon stocks",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = SOCstock0to30) 
SOCstd <- SOCstd %>%
  as.data.frame() %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = SOCstock0to30) %>%
  dplyr::left_join(SOCmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


bagh3p <- Cstd %>%
  rbind(Nstd) %>%
  rbind(PHstd) %>%
  rbind(SOCstd) %>%
  dplyr::filter(ESG != "Cropland" & ESG != "Forest") 





# Bagh 4
Cmean <- terra::extract(Ccalc, esg4p, method = "simple", fun = mean, bind = TRUE)
Cstd <- terra::extract(Ccalc, esg4p, method = "simple", fun = sd, bind = TRUE)

Nmean <- terra::extract(Ncalc, esg4p, method = "simple", fun = mean, bind = TRUE)
Nstd <- terra::extract(Ncalc, esg4p, method = "simple", fun = sd, bind = TRUE)

PHmean <- terra::extract(PHcalc, esg4p, method = "simple", fun = mean, bind = TRUE)
PHstd <- terra::extract(PHcalc, esg4p, method = "simple", fun = sd, bind = TRUE)

SOCmean <- terra::extract(SOCstack, esg4p, method = "simple", fun = mean, bind = TRUE)
SOCstd <- terra::extract(SOCstack, esg4p, method = "simple", fun = sd, bind = TRUE)


Cmean <- Cmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 4p",
                Unit = "dg/kg",
                Property = "Soil organic carbon content",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Cstd <- Cstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Cmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


Nmean <- Nmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100,
                Bagh = "Bagh 4p",
                Unit = "g/kg",
                Property = "Total nitrogen",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
Nstd <- Nstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/100) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(Nmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


PHmean <- PHmean %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10,
                Bagh = "Bagh 4p",
                Unit = "pH",
                Property = "Soil pH",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = mean) 
PHstd <- PHstd %>%
  as.data.frame() %>%
  dplyr::mutate(mean = mean/10) %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = mean) %>%
  dplyr::left_join(PHmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 



SOCmean <- SOCmean %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 4p",
                Unit = "t/ha",
                Property = "Soil organic carbon stocks",
                'Soil depth' = "0-30 cm") %>%
  dplyr::select(Bagh, ESG = Name_ENG, Property, 'Soil depth', Unit, Mean = SOCstock0to30) 
SOCstd <- SOCstd %>%
  as.data.frame() %>%
  dplyr::select(ESG = Name_ENG, 'Standard deviation' = SOCstock0to30) %>%
  dplyr::left_join(SOCmean) %>%
  dplyr::select(Bagh, ESG, Property, 'Soil depth', Unit, Mean, 'Standard deviation') %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) 


bagh4p <- Cstd %>%
  rbind(Nstd) %>%
  rbind(PHstd) %>%
  rbind(SOCstd) %>%
  dplyr::filter(ESG != "Cropland" & ESG != "Forest") 




# Join bagh level tables together
Bornuur0to30 <- bagh1p %>%
  rbind(bagh2p) %>%
  rbind(bagh3p) %>%
  rbind(bagh4p)



# Boxplot
bp <- Bornuur0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(x = Bagh, y = Mean, fill = Bagh)) +
  geom_boxplot() + 
  ggtitle("Mean soil organic carbon stocks across baghs, Bornuur soum, Tov") +
  theme(legend.position = "none") +
  ylab("Soil organic carbon stocks (t/ha)") +
  xlab("Bagh")

bp 

ggsave(plot = bp, filename = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\BornuurBoxplot.png", width = 7, height = 4)

# Change ESG names
unique(Bornuur0to30$ESG)
Bornuur0to30 <- dplyr::mutate(Bornuur0to30, ESG = ifelse(ESG == "1. Festuca-Forbs mountain steppe rangeland in Gravelly hills  and fan ESG, Forest steppe", 
                                                               "ESG 1. Festuca-Forbs mountain steppe rangeland",
                                                               ifelse(ESG == "2. Small bunch grass-Forbs  mountain steppe rangeland in Loamy fan ESG, Forest steppe",
                                                                      "ESG 2. Small bunch grass-Forbs  mountain steppe rangeland",
                                                                      ifelse(ESG == "4. Stipa baicalensis-Forbs meadow steppe rangeland in Mountain valley ESG, Forest steppe",
                                                                             "ESG 4. Stipa baicalensis-Forbs meadow steppe rangeland",
                                                                             ifelse(ESG == "5. Grass-Forbs riparian rangeland in High water table ESG, Forest steppe",
                                                                                    "ESG 5. Grass-Forbs riparian rangeland",
                                                                                    ifelse(ESG == "3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland in Loamy fan ESG, Forest steppe",
                                                                                           "ESG 3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland", ESG))))))




names(Bornuur0to30)
Bornuur0to30 <- Bornuur0to30 %>%
  dplyr::mutate(Aimag = "Tov", Soum = "Bornuur") %>%
  dplyr::select(Aimag, Soum, Bagh:`Standard deviation`)


# Write to csv
write.csv(Bornuur0to30, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\Bornuur0to30recalc.csv", row.names = FALSE)




# Faceted ESG barplot
Bornuur0to30$ESG <- stringr::str_remove(Bornuur0to30$ESG, "\\.[^.]*$")

barplot <- Bornuur0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(x = ESG, y = Mean, fill = ESG)) +
  geom_bar(stat = "identity") + 
  ggtitle("Mean soil organic carbon stocks across ESGs and Baghs, Bornuur soum, Tov aimag") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Soil organic carbon stocks (t/ha)") +
  xlab("ESG") +
  facet_wrap(~Bagh)


ggsave(plot = barplot, filename = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\BornuurBarplot.png", width = 8, height = 6)



# Calculate area table
names(esg1p)
esg1p <- esg1p %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 1p") %>%
  dplyr::select(Bagh, ESG = Name_ENG, "Area, ha" = AreaHA)

esg2p <- esg2p %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 2p") %>%
  dplyr::select(Bagh, ESG = Name_ENG, "Area, ha" = AreaHA)

esg3p <- esg3p %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 3p") %>%
  dplyr::select(Bagh, ESG = Name_ENG, "Area, ha" = AreaHA)

esg4p <- esg4p %>%
  as.data.frame() %>%
  dplyr::mutate(Bagh = "Bagh 4p") %>%
  dplyr::select(Bagh, ESG = Name_ENG, "Area, ha" = AreaHA)


# Rbind 
areas <- esg1p %>%
  rbind(esg2p) %>%
  rbind(esg3p) %>%
  rbind(esg4p) %>%
  dplyr::mutate_if(is.numeric, round, digits = 0) %>%
  dplyr::mutate(ESG = ifelse(ESG == "1. Festuca-Forbs mountain steppe rangeland in Gravelly hills  and fan ESG, Forest steppe", 
                                                           "ESG 1. Festuca-Forbs mountain steppe rangeland",
                                                           ifelse(ESG == "2. Small bunch grass-Forbs  mountain steppe rangeland in Loamy fan ESG, Forest steppe",
                                                                  "ESG 2. Small bunch grass-Forbs  mountain steppe rangeland",
                                                                  ifelse(ESG == "4. Stipa baicalensis-Forbs meadow steppe rangeland in Mountain valley ESG, Forest steppe",
                                                                         "ESG 4. Stipa baicalensis-Forbs meadow steppe rangeland",
                                                                         ifelse(ESG == "5. Grass-Forbs riparian rangeland in High water table ESG, Forest steppe",
                                                                                "ESG 5. Grass-Forbs riparian rangeland",
                                                                                ifelse(ESG == "3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland in Loamy fan ESG, Forest steppe",
                                                                                       "ESG 3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland", ESG)))))) %>%
  dplyr::filter(ESG != "Forest" & ESG != "Cropland")



areas <- dplyr::arrange(areas, ESG)


areasbornuur <- areas


tab <- gt(areas, rowname_col = "ESG",
          groupname_col = "Bagh")

tab

tab <- tab_style(tab, style = cell_text(weight = "bold"),
                 locations = cells_column_labels())
tab <- tab_style(tab, style = cell_text(weight = "bold"),
                 locations = cells_row_groups())
tab <- tab_header(tab, title = md("**ESG areas by bagh, Bornuur soum, Tov aimag**"))


tab


gt::gtsave(tab, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\", filename = "BornuurAreaTable.png", vwidth = 1500, vheight = 1000)


write.csv(areas,  "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\BornurrAreaData.csv", row.names = FALSE)
