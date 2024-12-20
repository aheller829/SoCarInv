# Creating maps of SOC from SoilGrids data for World Bank project proposal

# First load libraries
library(sf)
library(raster)
library(ggplot2)
library(tidyverse)
library(terra)
library(gdalUtilities)
library(stars)
library(RColorBrewer)
library(tmap)
library(ggrepel)
library(gt)
library(gtable)

# Load shapfiles of aimags and soums
aimags <- sf::st_read("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\GIS\\mng_admbnda_adm1_nso_20201019.shp")
plot(aimags["ADM1_EN"]) # Make a map 
aimags # Check projection - it's WGS 84

soums <- sf::st_read("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\GIS\\mng_admbnda_adm2_nso_20201019.shp")
plot(soums["ADM2_EN"]) # Make a map 
soums #  Check projection - it's WGS 84


# Select aimags and soums in area of interest (AOI)
# All soums from Uvs, Khovd, and Bayan-Olgii aimags
# First select aimags 
# Check aimag names for spelling
unique(aimags$ADM1_EN)
wbaimags <- dplyr::filter(aimags, ADM1_EN %in% c("Uvs", "Khovd", "Bayan-Olgii")) # Select aimags
# Select all soums within these aimags
wbsoums <- dplyr::filter(soums, ADM1_EN %in% c("Uvs", "Khovd", "Bayan-Olgii"))
# Plot aimag and soum selection
ggplot() +
  geom_sf(data = wbsoums, fill = "antiquewhite1", color = "gray")  +
  geom_sf(data = wbaimags, color = "black", fill = NA) +
  geom_sf_label(data = wbaimags, aes(label = ADM1_EN),
                size = 3, fontface = "bold") 


# Download SoilGrids data
# Get bounding box (extent of AOI)
igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # Set projection of SoilGrids data
wbaimags <- sf::st_transform(wbaimags, igh) # Change projection of AOI to match soilgrids
bbox <- sf::st_bbox(wbaimags) # Get boundaries of AOI
# Define bounding box attributes 
ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

# Set SoilGrids URL 
sg_url <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
# Set variable of interest to soil organic carbon stock
datos <- 'ocs/ocs_0-30cm_mean.vrt'
# Set file name
file <-  "ocs_igh_0_30.tif"

# Download data
gdal_translate(paste0(sg_url,datos), file ,
               tr = c(250,250),
               projwin = bb,
               projwin_srs = igh)

# Write to raster file
soc <- raster(file)

# Convert to stars object for plotting
socstar <- stars::st_as_stars(soc)
names(socstar) <-  "soc"
# Set color palette
pal <- colorRampPalette(brewer.pal(9, "BrBG"))
# Create map
tm_shape(socstar) +
  tm_raster("soc", palette = pal(10), title = "SOC stocks", )  +  
  tm_shape(wbaimags) + tm_borders() +
  tm_layout(scale = .8, 
            legend.position = c("left","bottom"),
            legend.bg.color = "white", legend.bg.alpha = .2, 
            legend.frame = "gray50")


# Convert aimag polygons back to proper projection
wbaimags <- sf::st_transform(wbaimags, crs(wbsoums))
# Reproject SOC raster to match polygon shapefiles
soc <- raster::projectRaster(soc, crs = crs(wbsoums))
# Trim raster to shapefile 
socclip <- raster::crop(soc, extent(wbaimags)) 
socclip <- raster::mask(socclip, wbaimags)
# Convert SOC raster to a dataframe so it can be plotted with ggplot
soc_df <- as.data.frame(socclip, xy = TRUE, na.rm = TRUE)
names(soc_df) # Show column names
# Plot raster and polygons together
socgg <- ggplot() + 
  geom_raster(data = soc_df, aes(x = x, y = y, fill = ocs_igh_0_30)) +
  scale_fill_gradient(trans = 'reverse',
                      name = "SOC stocks (t/ha)",
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_sf(data = wbsoums, fill = NA, color = "gray")  +
  geom_sf(data = wbaimags, color = "black", fill = NA) +
  geom_sf_label(data = wbaimags, aes(label = ADM1_EN),
                size = 3, fontface = "bold") +
  theme_minimal() +
  ggtitle("SOC stocks in AOI")

socgg



# Calculate summary statistics by soums
SOCmeans <- raster::extract(socclip, wbsoums, fun = mean, na.rm = TRUE, df = TRUE)
SOCmeans <- dplyr::rename(SOCmeans, SOCmean = ocs_igh_0_30) # Rename summary stat

SOCmins <- raster::extract(socclip, wbsoums, fun = min, na.rm = TRUE, df = TRUE)
SOCmins <- dplyr::rename(SOCmins, SOCmin = ocs_igh_0_30) # Rename summary stat

SOCmax <- raster::extract(socclip, wbsoums, fun = max, na.rm = TRUE, df = TRUE)
SOCmax <- dplyr::rename(SOCmax, SOCmax = ocs_igh_0_30) # Rename summary stat

SOCsd <- raster::extract(socclip, wbsoums, fun = sd, na.rm = TRUE, df = TRUE)
SOCsd <- dplyr::rename(SOCsd, SOCsd = ocs_igh_0_30) # Rename summary stat

# Join summary statistics to soum shapefile
socsums <- wbsoums %>%
  cbind(SOCmeans) %>%
  cbind(SOCmins) %>%
  cbind(SOCmax) %>%
  cbind(SOCsd) %>%
  dplyr::select(-ID.1, -ID.2, -ID.3) # Remove redundant ID columns from joins


# Write summary stats to csv
socsumsdf <- sf::st_drop_geometry(socsums)
write.csv(socsumsdf, "Outputs\\WBsocsums.csv", row.names = FALSE)

# Write shapefile
sf::st_write(socsums, "C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\GIS\\WBsocsums.shp", driver = 'ESRI Shapefile', append = FALSE)



# Read in summary stats
socsumdf <- read.csv("Outputs\\WBsocsums.csv")
socsum <- sf::st_read("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\GIS\\WBsocsums.shp")


# Make final maps
mng <- sf::st_read("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\GIS\\mng_boundary.shp")

# Map of AOI within MNG
mngplot <- ggplot() +
  geom_sf(data = mng, fill = "cornsilk", color = "black") + # Country boundary
  geom_sf(data = wbsoums, fill = "deepskyblue1", color = "gray")  + # Soum boundaries
  geom_sf(data = wbaimags, color = "black", fill = NA) + # Aimag boundaries
  geom_sf_label(data = wbaimags, aes(label = ADM1_EN), # Aimag labels
                size = 3, fontface = "bold") +
  theme(legend.position="none", 
        panel.grid.major = element_blank(), # Removes grid, axis labels, axis ticks
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Target Aimags") # Adds title to plot
  
mngplot # Display plot
# Save as png
ggsave(plot = mngplot, filename = "Outputs\\WB_mng.png", width = 7, height = 4)




# Carbon stock estimates across whole study area
socgg <- ggplot() + 
  geom_sf(data = wbsoums, fill = "chocolate")  +
  geom_raster(data = soc_df, aes(x = x, y = y, fill = ocs_igh_0_30)) +
  scale_fill_gradient(trans = 'reverse',
                      name = "SOC stocks (t/ha)",
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_sf(data = wbsoums, fill = NA, color = "gray")  +
  geom_sf(data = wbaimags, color = "black", fill = NA) +
  geom_sf_label(data = wbaimags, aes(label = ADM1_EN),
                size = 3, fontface = "bold") +
  ggtitle("Soil organic carbon stock estimates in area of interest*") +
  theme(
        panel.grid.major = element_blank(), # Removes grid, axis labels, axis ticks
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "*Areas in orange are missing data in the SoilGrids SOC model") 

socgg
ggsave(plot = socgg, filename = "Outputs\\WB_AOI_soc.png", width = 7, height = 4)


# Aimag/soum specific estimates
uvs <- dplyr::filter(wbaimags, ADM1_EN == "Uvs") # Select Uvs from AOI aimags
uvssoums <- dplyr::filter(wbsoums, ADM1_EN == "Uvs") # Select Uvs soums from whole AOI
# Clip and mask raster to Uvs
socuvs <- raster::crop(soc, extent(uvs)) 
socuvs <- raster::mask(socuvs, uvs)
# Convert SOC raster to a dataframe so it can be plotted with ggplot
soc_df_uvs <- as.data.frame(socuvs, xy = TRUE, na.rm = TRUE)
# Plot raster and polygons together
uvsgg <- ggplot() + 
  geom_sf(data = uvssoums, fill = "chocolate")  +
  geom_raster(data = soc_df_uvs, aes(x = x, y = y, fill = ocs_igh_0_30)) +
  scale_fill_gradient(trans = 'reverse',
                      name = "SOC stocks (t/ha)",
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_sf(data = uvssoums, fill = NA, color = "gray")  +
  geom_sf(data = uvs, color = "black", fill = NA) +
  geom_sf_label(data = uvssoums, aes(label = ADM2_EN),
                size = 2, fontface = "bold") +
  ggtitle("Soil organic carbon stock estimates for soums in Uvs aimag*") +
  theme(
    panel.grid.major = element_blank(), # Removes grid, axis labels, axis ticks
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  labs(caption = "*Areas in orange are missing data in the SoilGrids SOC model") 

uvsgg
ggsave(plot = uvsgg, filename = "Outputs\\WB_Uvs_soc.png", width = 7, height = 4)


# Bayan-Olgii
bo <- dplyr::filter(wbaimags, ADM1_EN == "Bayan-Olgii") 
bosoums <- dplyr::filter(wbsoums, ADM1_EN == "Bayan-Olgii") 
# Clip and mask raster to Uvs
socbo <- raster::crop(soc, extent(bo)) 
socbo <- raster::mask(socbo, bo)
# Convert SOC raster to a dataframe so it can be plotted with ggplot
soc_df_bo <- as.data.frame(socbo, xy = TRUE, na.rm = TRUE)
# Plot raster and polygons together
bogg <- ggplot() + 
  geom_sf(data = bosoums, fill = "chocolate")  +
  geom_raster(data = soc_df_bo, aes(x = x, y = y, fill = ocs_igh_0_30)) +
  scale_fill_gradient(trans = 'reverse',
                      name = "SOC stocks (t/ha)",
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_sf(data = bosoums, fill = NA, color = "gray")  +
  geom_sf(data = bo, color = "black", fill = NA) +
  ggrepel::geom_label_repel(
    data = bosoums,
    aes(label = ADM2_EN, geometry = geometry),
    size = 2, fontface = "bold",
    stat = "sf_coordinates",
    min.segment.length = 0
  ) +
  ggtitle("Soil organic carbon stock estimates for soums in Bayan-Olgii aimag*") +
  theme(
    panel.grid.major = element_blank(), # Removes grid, axis labels, axis ticks
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  labs(caption = "*Areas in orange are missing data in the SoilGrids SOC model") 

bogg
ggsave(plot = bogg, filename = "Outputs\\WB_BO_soc.png", width = 7, height = 4)


# Khovd
khovd <- dplyr::filter(wbaimags, ADM1_EN == "Khovd") 
khovdsoums <- dplyr::filter(wbsoums, ADM1_EN == "Khovd") 
# Clip and mask raster to Uvs
sock <- raster::crop(soc, extent(khovd)) 
sock <- raster::mask(sock, khovd)
# Convert SOC raster to a dataframe so it can be plotted with ggplot
soc_df_k <- as.data.frame(sock, xy = TRUE, na.rm = TRUE)
# Plot
kgg <- ggplot() + 
  geom_sf(data = khovdsoums, fill = "chocolate")  +
  geom_raster(data = soc_df_k, aes(x = x, y = y, fill = ocs_igh_0_30)) +
  scale_fill_gradient(trans = 'reverse',
                      name = "SOC stocks (t/ha)",
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  geom_sf(data = khovdsoums, fill = NA, color = "gray")  +
  geom_sf(data = khovd, color = "black", fill = NA) +
  ggrepel::geom_label_repel(
    data = khovdsoums,
    aes(label = ADM2_EN, geometry = geometry),
    size = 2, fontface = "bold",
    stat = "sf_coordinates",
    min.segment.length = 0
  ) +
  ggtitle("Soil organic carbon stock estimates for soums in Khovd aimag*") +
  theme(
    panel.grid.major = element_blank(), # Removes grid, axis labels, axis ticks
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  labs(caption = "*Areas in orange are missing data in the SoilGrids SOC model") 

kgg
ggsave(plot = kgg, filename = "Outputs\\WB_Khovd_soc.png", width = 7, height = 4)




# Make tables of mean and sd
names(socsumdf)
# Tables by aimag
ksum <- socsumdf %>%
  dplyr::filter(ADM1_EN == "Khovd") %>%
  dplyr::select(Soum = ADM2_EN, "Mean SOC stock" = SOCmean,
                "SOC stock SD" = SOCsd)
# Shorten decimal places
ksum$`Mean SOC stock` <- round(ksum$`Mean SOC stock`, 1)
ksum$`SOC stock SD` <- round(ksum$`SOC stock SD`, 1)
# gtable
tab <- gt(ksum)
tab
tab <- tab_style(tab, style = cell_text(weight = "bold"),
                 locations = cells_column_labels())
tab <- tab_header(tab, title = md("**SOC stocks (t/ha) by soum, Khovd aimag**"))
tab <- cols_align(tab, align = "center", columns = everything())
tab <- tab_footnote(tab,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))

tab

gt::gtsave(tab, path = "Outputs\\", filename = "WB_Khovd_SOC_tab.png", vwidth = 1500, vheight = 1000)


