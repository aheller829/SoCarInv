# Clean NAMEM soil pit data for analysis

# Load libraries
library(tidyverse)
library(sf)
library(aqp)
library(ggplot2)

# Read in soil data
soilpits <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\soilpits.csv")
horizons <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\soilhorizons.csv")

# Join profile data with horizon data
soiljoin <- dplyr::left_join(horizons, soilpits, by = c("Site", "Plot"))
names(soiljoin)
soiljoin <- dplyr::select(soiljoin, Site, Plot, Upper.Depth, Lower.Depth,
                          Horizon, Horizon.mod, Hue, Value, Chroma, Color, Texture,
                          TotalFragments = Total.Rock.Fragments..vol., Effervescence,
                          Grade, Structure, Size, ClayPct, Ecol.Site, Slope, Aspect, SlopeShape,
                          Landform, Hillslope.Profile, Latitude, Longitude)


# Combine Plot and Site names for single unique identifier
soiljoin <- tidyr::unite(soiljoin, "PK", Site:Plot, sep = "_", remove = FALSE)

# Add horizon number
soiljoin <- soiljoin %>% 
  dplyr::group_by(PK) %>% 
  dplyr::mutate(HorizonNumber = row_number()) %>%
  dplyr::ungroup()

# Add CIELAB colors to dataframe
lab <- munsell2rgb(
  soiljoin$Hue,
  soiljoin$Value,
  soiljoin$Chroma,
  alpha = 1,
  maxColorValue = 1,
  return_triplets = FALSE,
  returnLAB = TRUE
)

soiljoin <- cbind(soiljoin, lab)

# Remove any plots/rows where color is NA
soiljoin <- dplyr::filter(soiljoin, !is.na(L) | !is.na(A) | !is.na(B))

# Standardize landform names
soiljoin$Landform <- tolower(soiljoin$Landform)
unique(soiljoin$Landform)
soiljoin <- dplyr::mutate(soiljoin, Landform = ifelse(Landform == "plain (also landscape)", "plain", 
                                            ifelse(Landform == "peidmont slope", "piedmont slope", Landform)))

# Remove whitespace
soiljoin$Landform <- gsub('\\s+', '', soiljoin$Landform)


# Add AWC
awcto50 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\awcto50.csv")
awc <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\awc.csv")

names(awcto50)
names(awc)

awc <- tidyr::unite(awc, "PK", Site:Plot, sep = "_", remove = FALSE)
awcto50 <- tidyr::unite(awcto50, "PK", Site:Plot, sep = "_", remove = FALSE)

awcto50 <- dplyr::select(awcto50, PK, AWC_to_50cm = AWC_to_50cm_cm)
awc <- dplyr::select(awc, PK, Upper.Depth, Lower.Depth, AWC_horizon)

awcto50 <- na.omit(awcto50)

str(awc)
str(awcto50)
awc$Lower.Depth <- as.integer(awc$Lower.Depth)

awccomb <- dplyr::full_join(awc, awcto50)

# Add horizon number
awccomb <- awccomb %>% 
  dplyr::group_by(PK) %>% 
  dplyr::mutate(HorizonNumber = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-Upper.Depth, -Lower.Depth)



soiljoin <- dplyr::left_join(soiljoin, awccomb)


# Add numerical variable for effervescence class
unique(soiljoin$Effervescence)
soiljoin <- dplyr::mutate(soiljoin, EffNum = ifelse(Effervescence == "NE", 1,
                                                    ifelse(Effervescence == "VS", 2,
                                                           ifelse(Effervescence == "SL", 3,
                                            ifelse(Effervescence == "ST", 4, 
                                                   ifelse(Effervescence == "VE", 5, NA))))))


write.csv(soiljoin, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\cleandatajoin.csv", row.names = FALSE)


