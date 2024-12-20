# Estimating carbon content from soil color at NAMEM points

# Load libraries
library(tidyverse)
library(sf)
library(aqp)
library(ggplot2)
library(terradactyl)


# Read in soil data
soiljoin <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\cleandatajoin.csv")

plots_stms <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\plots_stms.csv")

plots_esgs <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\ESDplots_550_IDs.csv")

s_plots <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\S_plots.csv")

s_plots_es <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\S_plots_es.csv")

fs_plots <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\FS_plots.csv")

fs_plots_ll <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\FS_latlong.csv")




# Join steppe and forest steppe plot tables
names(s_plots)
names(fs_plots)

s_plots <- dplyr::select(s_plots, SiteID, PlotID, Easting, Northing)
# Merge steppe tables to get siteid
s_plots <- dplyr::left_join(s_plots, s_plots_es)

# Get latlong for FS
fs_plots <- dplyr::select(fs_plots, SiteID, PlotID = tblPlots_PlotID, EcolSite)
fs_plots <- dplyr::left_join(fs_plots, fs_plots_ll)

fs_plots <- dplyr::select(fs_plots, SiteID, PlotID, Easting = Lon, Northing = Lat, EcolSite)


sfs_plots <- rbind(s_plots, fs_plots)
sfs_plots$PlotID <- trimws(sfs_plots$PlotID)


# All years are listed as 2016
names(plots_stms)
unique(plots_stms$YEAR)
# Clean plot_stms table
plots_stms <- dplyr::select(plots_stms, SiteID = SITEID, PlotID = PLOTID, Longitude, Latitude, Zone, 
                            STM_2012, STM_2014, STM_2016, STM_2020, RC_2012, RC_2014,
                            RC_2016, RC_2020, Community.name)
# STMs seem mostly consistent across years except for in 2014 for many plots - but will
# use the most recent (STM_2020)
plots_stms <- dplyr::select(plots_stms, SiteID, PlotID, Easting = Longitude, Northing = Latitude, Zone, STM_2020,
                            RC_2020, Community.name)
plots_stms$PlotID <- trimws(plots_stms$PlotID)


str(plots_stms)
str(sfs_plots)

# Try to join rc data with steppe, forest steppe plot data
plots <- dplyr::left_join(plots_stms, sfs_plots)

plots_stms$SiteID <- trimws(plots_stms$SiteID)
sfs_plots$SiteID <- trimws(sfs_plots$SiteID)


x <- subset(sfs_plots, sfs_plots$SiteID %in% plots_stms$SiteID)


plots <- dplyr::left_join(plots_stms, sfs_plots, by = "SiteID")

names(plots)
plots <- plots %>%
  dplyr::select(SiteID, plots_stms_PlotID = PlotID.x, sfs_plots_PlotID = PlotID.y) %>%
  dplyr::filter(!(is.na(plots_stms_PlotID)) & !is.na(sfs_plots_PlotID))

write.csv(plots, "Outputs//plotsnames.csv", row.names = FALSE)

# Try to crossreference plots between dataframes
unique(plots_stms$PLOTID)
unique(soiljoin$Site)

unique(plots_stms$SITEID)
unique(soiljoin$Plot)

plots_stms$PLOTID <- trimws(plots_stms$PLOTID)
soiljoin$Plot <- trimws(soiljoin$Plot)

match <- merge(soiljoin, plots_stms, by.x = "Plot", by.y = "PLOTID", all.x = TRUE)
match <- merge(soiljoin, plots_esgs, by.x = "Plot", by.y = "PlotID")
match <- merge(plots_stms, plots_esgs, by.x = "PLOTID", by.y = "PlotID")




# Read in shapefiles
sukhbaatar <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\SukhbaatarAimag.shp")
dornod <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\DornodAimag.shp")
# Join polygons
soums <- rbind(sukhbaatar, dornod)
str(soums)


# Add color hexes to soiljoin for plotting
hexes <- munsell2rgb(
  soiljoin$Hue,
  soiljoin$Value,
  soiljoin$Chroma,
  alpha = 1,
  maxColorValue = 1,
  return_triplets = FALSE,
  returnLAB = FALSE
)

soiljoin <- cbind(soiljoin, hexes)




# Convert soilpits to shapefile
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
soilshape <- sf::st_as_sf(x = soiljoin,                         
               coords = c("Longitude", "Latitude"),
               crs = projcrs)



# Select soil pits within Sukhbaatar and Dornod
soumsoils <- sf::st_filter(soilshape, soums)


# Filter for upper horizons
str(soumsoils)
uppersoils <- dplyr::filter(soumsoils, Lower.Depth < 40)
uppersoils$Lower.Depth <- as.numeric(uppersoils$Lower.Depth)
uppersoils$Upper.Depth <- as.numeric(uppersoils$Upper.Depth)

# Or, just pick first horizon
firsthorizon <- dplyr::filter(soumsoils, HorizonNumber == 1)

# Or, create a standard cutoff depth (30 cm)
names(soumsoils)
soumsoils$Upper.Depth <- as.numeric(soumsoils$Upper.Depth)
soumsoils$Lower.Depth <- as.numeric(soumsoils$Lower.Depth)
thirtydepth <- soumsoils %>%
  dplyr::group_by(PK) %>%
  dplyr::filter(Upper.Depth < 30) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(StDepth = ifelse(Lower.Depth < 30, Lower.Depth, 30)) %>%
  dplyr::select(PK, Site, Plot, Upper.Depth, 
                Lower.Depth, StDepth, HorizonNumber, Horizon, Hue, Value,
                Chroma, Color, Texture, AWC_horizon, AWC_to_50cm, ClayPct, TotalFragments, Effervescence, Landform, 
                Slope, Aspect, SlopeShape, hexes, L, A, B)


# Make groups based on landform and soil texture (will replace with ESG when data is available)
# Split into separate dataframes
setNames(uppersoils %>% 
           group_split(Landform), paste0(unique(uppersoils$Landform))) %>%
  list2env(envir = globalenv())

# Plot soil textures across landforms
textsum <- alluvialfan %>%
  dplyr::group_by(Texture) %>%
  dplyr::summarise(Count = n())

ggplot(textsum, aes(x = Texture, y = Count)) + 
  geom_bar(stat = "identity")

# Facetedplot
firsthorizon %>%
  dplyr::group_by(Landform, Texture) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot(aes(x = Texture, y = Count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Landform)



# Bar plot count of soil colors in first horizon
hexsum <- firsthorizon %>%
  dplyr::group_by(hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()

names(hexes) <- as.character(firsthorizon$hexes)
hexsum$L <- as.numeric(hexsum$L)

# Regular barplot
ggplot(hexsum, aes(x = hexes, y = Count, fill = hexes, reorder(L))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = unique(hexsum$hexes))
# Stacked barplot - stacks are landforms?
hexsumlf <- firsthorizon %>%
  dplyr::group_by(Landform, hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()

hexsumlf$LF <- as.factor(hexsumlf$L)

ggplot(hexsumlf, aes(fill = LF, y = Count, x  = Landform, fill = LF, reorder(L))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = unique(hexsum$hexes))
# Stacks as soil texture
textsum <- firsthorizon %>%
  dplyr::group_by(Texture, hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()
textsum$LF <- as.factor(textsum$L)

ggplot(textsum, aes(fill = LF, y = Count, x  = Texture, fill = LF, reorder(L))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = unique(hexsum$hexes))


# Stacked barplot for whole dataset
hexsumlf <- soiljoin %>%
  dplyr::group_by(Landform, hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()

hexsumlf$LF <- as.factor(hexsumlf$L)

ggplot(hexsumlf, aes(fill = LF, y = Count, x  = Landform, fill = LF, reorder(L))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = unique(hexsumlf$hexes)) +
  theme(legend.position = "none")
# Stacks as soil texture
textsum <- soiljoin %>%
  dplyr::group_by(Texture, hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()
textsum$LF <- as.factor(textsum$L)

ggplot(textsum, aes(fill = LF, y = Count, x  = Texture, fill = LF, reorder(L))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = unique(textsum$hexes)) +
  theme(legend.position = "none")




# Make scatter plot of color vs. pct clay, etc.
names(firsthorizon)
ggplot(firsthorizon, aes(x = ClayPct, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
        size = 0.5)) +
  scale_colour_identity() 


ggplot(firsthorizon, aes(x = AWC_horizon, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() 


ggplot(firsthorizon, aes(x = AWC_to_50cm, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() 



ggplot(firsthorizon, aes(x = EffNum, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() 



# Use all horizons
ggplot(soiljoin, aes(x = EffNum, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() +
  theme(legend.position = "none")

ggplot(soiljoin, aes(x = ClayPct, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() +
  theme(legend.position = "none")


ggplot(soiljoin, aes(x = AWC_horizon, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() +
  theme(legend.position = "none")



# Use all horizons, just within two soums
ggplot(soiljoin, aes(x = EffNum, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() +
  theme(legend.position = "none")

ggplot(soiljoin, aes(x = ClayPct, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() +
  theme(legend.position = "none")


ggplot(soiljoin, aes(x = AWC_horizon, y = L, colour = hexes)) + 
  geom_point(aes(colour = hexes, fill = hexes, 
                 size = 0.5)) +
  scale_colour_identity() +
  theme(legend.position = "none")

# Make map with Munsell colors as symbology
# Read in ESG map to add
esgs <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\FinalESGMerge.shp")

map <- ggplot2::ggplot() + geom_sf(data = soums) +
  geom_point(data = firsthorizon, aes(colour = hexes, fill = hexes, 
                                      geometry = geometry, size = 0.5),
             stat = "sf_coordinates",
             show.legend = FALSE) +
  scale_colour_identity() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  labs(title = "Munsell color of first horizon for soil pits in Sukhbaatar and Dornod soums")
map
  
# Save firsthorizon as shapefile for arcmap
sf::st_write(firsthorizon, "C:\\Users\\TRAVAH\\Documents\\GIS\\firsthorizonDornodSukhbaatar.shp", driver = 'ESRI Shapefile')





