# Estimating carbon content from soil color at NAMEM points, DIMA extraction

# Load libraries
library(tidyverse)
library(sf)
library(aqp)
library(ggplot2)
library(raster)


soildima1 <-  read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\soil_1.csv")
hzdima1 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\horizons_1.csv")


soildima2 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\soil_2.csv")
hzdima2 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\horizons_2.csv")


soildima3 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\soil_3.csv")
hzdima3 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\horizons_3.csv")

soildima4 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\soil_4.csv")
hzdima4 <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\horizons_4.csv")


# Join soil tables
names(soildima1)
names(soildima2)
names(soildima3)
names(soildima4)

soildima <- rbind(soildima1, soildima2)
soildima <- rbind(soildima, soildima3)
soildima <- rbind(soildima, soildima4)


# Standardize variables
table(soildima$Landform)
soildima$Landform <- tolower(soildima$Landform)

table(soildima$Ecol.Site)
soildima$Ecol.Site <- tolower(soildima$Ecol.Site)
unique(soildima$Ecol.Site)
soildima <- soildima %>%
  dplyr::mutate(Ecol.Site = trimws(Ecol.Site)) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic loamy", "calcic loam"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic sandy", "calcic sand"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic candy", "calcic sand"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "loamyclayey", "loamy/clayey"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "shallow gravelly loamy", "shallow gravelly loam"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calccic loamy/clayey", "calcic loamy/clayey"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic sandyy loam", "calcic sandy loam"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic loamyy/clayey", "calcic loamy/clayey"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic loamyy", "calcic loamy"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "calcic sandyy loam, bottom land", "calcic sandy loam, bottom land"))) 


# Add PK
soildima$PK <- seq.int(nrow(soildima))

soildima <- sf::st_as_sf(soildima, coords = c(5, 4))
st_crs(soildima) <- 4326


plot(soildima$geometry)

# Read in ESG poly
esgs <-  sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\ESGS_dissolved.shp")
# Transform
soildima <- sf::st_transform(soildima, crs = st_crs(esgs))

# Extract ESG to points
sf_use_s2(FALSE)
soildima <- sf::st_intersection(soildima, esgs)

table(soildima$Name_ENG)

soildima <- dplyr::mutate(soildima, ESG = as.integer(str_extract(Name_ENG, "[0-9]+")))

table(soildima$ESG)

names(soildima)
soildima <- dplyr::select(soildima, PK, Site, Plot, Ecol.Site, Slope, Aspect, SlopeShape,
                          Landform, Hillslope.Profile, Total.Depth = Total.Soil.Pedon.Depth,
                          NZ_ENG, ESG = Name_ENG, ESGcode = ESG)


unique(soildima$Aspect)
unique(soildima$Slope)
unique(soildima$SlopeShape)


soildima <- soildima %>%
  dplyr::mutate(Aspect = ifelse(Aspect == "" | Aspect == "N/A" | Aspect == "n/A" | 
                                  Aspect == "n/a" | Aspect == "na", NA, Aspect),
                SlopeShape = ifelse(SlopeShape == "", NA, SlopeShape))

soildima$Slope <- as.integer(soildima$Slope)
soildima$Aspect <- as.integer(soildima$Aspect)


# Save as shp
sf::st_write(soildima, "C:\\Users\\TRAVAH\\Documents\\GIS\\soildima.shp", driver = 'ESRI Shapefile', append = FALSE)

soildima <- sf::st_drop_geometry(soildima)

# Remove duplicates
soildima <- dplyr::filter(soildima, PK != 23)

write.csv(soildima, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\soildima_clean.csv", row.names = FALSE)


# Join horizon data
hzdima <- rbind(hzdima1, hzdima2)
hzdima <- rbind(hzdima, hzdima3)
hzdima <- rbind(hzdima, hzdima4)


names(hzdima)

# Select columns
hzdima <- hzdima %>%
  dplyr::select(Site, Plot, Upper.Depth, Lower.Depth, Hue, Value, Chroma, Color, Texture,
                Frag.Type.1, Frag.Vol.1, Frag.Type.2, Frag.Vol.2, Frag.Type.3, Frag.Vol.3,
                TotalFrags = Total.Rock.Fragments..vol., Effervescence, PctClay = X..Clay)




# Add PKS
PKS <- dplyr::select(soildima, PK, Site, Plot, Ecol.Site, NZ_ENG, ESG, ESGcode)

hzdima <- dplyr::left_join(hzdima, PKS)

unique(hzdima$Texture)

hzdima <- hzdima %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "L/FSL", "L"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SIL/L", "L"))) %>%
  dplyr::mutate_all(funs(stringr::str_replace(., "SL/SCL", "SL")))

unique(hzdima$Frag.Type.1)

# Add color hexes to soiljoin for plotting
hexes <- munsell2rgb(
  hzdima$Hue,
  hzdima$Value,
  hzdima$Chroma,
  alpha = 1,
  maxColorValue = 1,
  return_triplets = FALSE,
  returnLAB = FALSE
)

hzdima <- cbind(hzdima, hexes)

# Remove plots without color
hzdima <- dplyr::filter(hzdima, !(is.na(Hue) | !(is.na(Value) | !(is.na(Chroma)))))



write.csv(hzdima, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\DIMA\\CSV\\hzdima_clean.csv", row.names = FALSE)



stms <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\plots_stms.csv")




# Find similar names
# Join site/plot
soildima <- tidyr::unite(soildima, col = SITE.PLOT, Site:Plot, sep = " / ", remove = FALSE)

soilnames <- soildima$SITE.PLOT
stmnames <- stms$SITE.PLOT

strdist <- stringdist::stringsimmatrix(soilnames, stmnames, method = "lv", useNames = "strings") %>% 
  as.matrix() %>%
  as_tibble(rownames = "word1") %>% 
  pivot_longer(-word1, names_to = "word2", values_to = "distance")

strdist <- dplyr::distinct(strdist)
# Take top match for each site
strdist_slice <- strdist %>%
  dplyr::group_by(word1) %>%
  dplyr::slice(which.max(distance))

# Edit manually in excel
write.csv(strdist_slice, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\plotnames.csv")
# Read back in
strdist_slice <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\plotnames.csv")

# Join and compare lat/long
stm_l <- dplyr::select(stms, SITE.PLOT, Longitude, Latitude)
soildima_l <- dplyr::select(soildima, SITE.PLOT, Longitude, Latitude)
strdist_slice <- dplyr::rename(strdist_slice, SITE.PLOT = Plot)

soildima_l <- dplyr::left_join(soildima_l, strdist_slice)
soildima_l <- na.omit(soildima_l)
soildima_l <- dplyr::rename(soildima_l, Plot = SITE.PLOT, SITE.PLOT = PLOTID,
                            Dima.Lat = Latitude, Dima.Long = Longitude)

l <- dplyr::left_join(soildima_l, stm_l)

l <- dplyr::select(l, -distance)

l$LatDiff <- l$Dima.Lat - l$Latitude
l$LongDiff <- l$Dima.Long - l$Longitude

# Compare distances
library(geosphere)
distm <- l %>%
 dplyr::mutate(distance = pmap(list(a = Dima.Long, 
                              b = Dima.Lat, 
                              x = Longitude,
                              y = Latitude), 
                         ~ geosphere::distGeo( c(..1, ..2), c(..3, ..4))))


distm <- as.data.frame(distm)
distm$km <- distm$distance * 0.001
str(distm)
distm$d <- unlist(lapply(distm$distance, function(x) ifelse(is.null(x), NA, x)))
distm <- distm %>%
  dplyr::select(-distance) %>%
  dplyr::mutate(km = d*0.001)


# Join STMs and plotdata
strdist_slice <- dplyr::select(strdist_slice, -distance)
soildima <- dplyr::left_join(soildima, strdist_slice)
names(stms)
names(soildima)
stms <- dplyr::select(stms, PLOTID = SITE.PLOT, Zone, STM_2012, RC_2012, 
                      STM_2014, RC_2014, STM_2016, RC_2016)


soildima <- dplyr::left_join(soildima, stms)

soildima_stms <- dplyr::filter(soildima, !(is.na(STM_2012)))

soils <- dplyr::left_join(soils, soildima_stms)

soils_stms <- dplyr::filter(soils, !is.na(STM_2012))
unique(soils_stms$Plot)


write.csv(soils_stms, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\soils_stms_matched.csv", row.names = FALSE)


# Add some veg data from Sumjee's plots
esdplots <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\esdplots.csv")
esdplots <- tidyr::unite(esdplots, col = SITE.PLOT, SiteID, PlotID, sep = " / ", remove = FALSE)
names(esdplots)
esdplots <- dplyr::select(esdplots, SITE.PLOT, TotalFoliar = Foliar.cover, 
                          BareGround = Totalbare_grnd, Stipa = Stipasum)

study <- dplyr::left_join(study, esdplots)




# Add elevation data
library(elevatr)
library(raster)
stm_l <- na.omit(stm_l)
stm_l <- sf::st_as_sf(stm_l, coords = c(2:3))
st_crs(stm_l) <- 4326

elev <- elevatr::get_elev_raster(locations = stm_l, z = 5, clip = "locations")

plot(elev, main = "Mongolia",
     legend.args = list(text = "Elevation (m)")) 

stm_l$Elevation <- raster::extract(elev, stm_l)
stm_l <- sf::st_drop_geometry(stm_l)


write.csv(stm_l, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\stms_elev.csv", row.names = FALSE)

strdist_slice <- dplyr::rename(strdist_slice, SITE.PLOT = PLOTID)

stm_l <- dplyr::left_join(stm_l, strdist_slice)
stm_l <- na.omit(stm_l)
stm_l <- dplyr::select(stm_l, -SITE.PLOT)
stm_l <- dplyr::rename(stm_l, SITE.PLOT = Plot)
stm_l <- dplyr::select(stm_l, -distance)

study <- dplyr::select(study, -distance, -Elevation)

study <- dplyr::left_join(study, stm_l)

write.csv(study, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\NAMEM\\fs_s_fullsoils_esgmatch.csv", row.names = FALSE)


c# Calculate plot level covariates
max.frags <- soils %>%
  dplyr::group_by(PK) %>%
  dplyr::slice(which.max(TotalFragments)) %>%
  dplyr::select(PK, HZUpperMaxFrags = UpperDepth, MaxFrags = TotalFragments)

max.clay <- soils %>%
  dplyr::select(PK, UpperDepth, ClayPct)
max.clay <- max.clay %>%
  dplyr::group_by(PK) %>%
  dplyr::slice(which.max(ClayPct)) %>%
  dplyr::select(PK, HZUpperMaxClay = UpperDepth, MaxPercentClay = ClayPct)

min.eff <- soils %>%
  dplyr::group_by(PK) %>%
  dplyr::slice(which.min(EffNum)) %>%
  dplyr::select(PK, HZUpperMinEff = UpperDepth, MinEff = EffNum)
max.eff <- soils %>%
  dplyr::group_by(PK) %>%
  dplyr::arrange(EffNum) %>%
  dplyr::slice(which.max(EffNum)) %>%
  dplyr::select(PK, HZUpperMaxEff = UpperDepth, MaxEff = EffNum)
max.eff <- sf::st_drop_geometry(max.eff)
min.eff <- sf::st_drop_geometry(min.eff)
change.eff <- dplyr::left_join(min.eff, max.eff)
change.eff <- change.eff %>%
  mutate(EffChange = MaxEff - MinEff) %>%
  select(PK, EffChange)
