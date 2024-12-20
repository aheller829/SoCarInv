# Get plot slopes and elevation from elevatr package

library(sf)
library(raster)
library(ggplot2)
library(rworldmap)
library(rworldxtra)
library(elevatr)
library(ncdf4)
library(ncdf4.helpers)

# Read in world data
world <- rworldmap::getMap(resolution = "low")
class(world)

# Convert world data to sf object
world <- sf::st_as_sf(world)
class(world)

# Map Mongolia
mng <- subset(world, ADMIN == "Mongolia")
(mngmap <- ggplot2::ggplot(data = mng) +
    geom_sf(fill = "cornsilk")) 

# Save MNG boundary as shapefile
sf::st_write(mng, "C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\GIS\\mng_boundary.shp", driver = 'ESRI Shapefile', append = FALSE)



# Get elevation data
elevation <- elevatr::get_elev_raster(locations = mng, z = 9, clip = "locations")
elevation_data <- as.data.frame(elevation, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"
# Remove rows of data frame with one or more NAs, using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

ggplot() +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = mng, color = "white", fill = NA) +
  coord_sf() +
  scale_fill_viridis_c() +
  labs(title = "Elevation in Mongolia", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")



# Get climate data from WorldClim download
# BIO1 = annual mean temperature
# BIO12 = annual precipitation 

# CCKP World Bank NetCDF downloaded data
# timeseries-pr-annual-mean_cru-x0.5_cru-ts4.07-historical_timeseries_mean_1901-2022.nc
fn <- ("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\Climate data\\pr-annual-mean_1901-2022.nc")
pr <- ncdf4::nc_open(fn)

print(pr)

# Get longitude and latitude
long <- ncdf4::ncvar_get(pr, "lon")
lat <- ncdf4::ncvar_get(pr, "lat")

# Get pr
precip <- ncdf4::ncvar_get(pr, "timeseries-pr-annual-mean")
# Check units
units <- ncdf4::ncatt_get(pr, "timeseries-pr-annual-mean")
units
# Precip is saved in a 3 dimensional file, with lat, long, and precip in the 3rd dimension
# To encompass time periods
dim(precip)
# The rightmost number 122 is the number of time points (years?)
# Check time units
tunits <- ncdf4::ncatt_get(pr, "time", "units")
tunits
# Units are days since Jan 1 1850


# Map a slice...a slice is one time period (should be year because I downloaded annual data)
slice <- precip[,,122] # The ,, indicates all lats and longs
# 122 should be most recent time slice 
image(long,lat,slice)


# Turn into a raster brick
raspr <- raster::brick(pr)




# Alternative workflow
# Define general variables
# First, file path
fn <- "C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\Climate data\\pr-annual-mean_1901-2022.nc"
# Name of variable of interest from the file
pr <- "timeseries-pr-annual-mean"
# Long, lat
x <- "lon"
y <- "lat"

# Open the file 
fn <- ncdf4::nc_open(fn)
# Load data
dat <- ncdf4::ncvar_get(fn, pr)
# Store data in a matrix
dat[] <- dat

# Get the range of longitude and latitude values
rlon <- ncdf4::ncvar_get(fn, varid = x) %>% range()  # Longitude range
rlat <- ncdf4::ncvar_get(fn, varid = y) %>% range()  # Latitude range

# Determine the dimensions of the data (X = number of longitude points, Y = number of latitude points)
X <- dim(dat)[1]
Y <- dim(dat)[2]

# Extract and format the time dimension as a Date vector
tt <- ncdf4.helpers::nc.get.time.series(fn, v = "time", time.dim.name = "time")
tt <- as.POSIXct(tt) %>% as.Date()  # Convert to Date format

# Close the netCDF file to free up resources
ncdf4::nc_close(fn)

# Create a raster template with the appropriate dimensions and extent
rs <- terra::rast(nrow = Y, ncol = X, extent = terra::ext(c(rlon, rlat)))

# Fix the orientation of the data to match the raster orientation
drs <- terra::xyFromCell(rs, 1:terra::ncell(rs)) %>% as_tibble()

# Initialize an empty list to store the raster layers
rs_list <- list()  
st <- terra::rast()  # Initialize an empty raster stack

# Loop through each time step, creating a raster layer for each
for (i in 1:length(tt)) {  
  dt1 <- dplyr::bind_cols(drs, as.vector(dat[, , i])) %>%
    magrittr::set_colnames(c("x", "y", pr))  # Bind coordinates and data into a single table
  
  dt1 <- terra::rast(dt1, type = "xyz")  # Create a raster from the table
  names(dt1) <- tt[i]  # Name the raster layer based on the date
  
  st <- c(st, terra::flip(dt1))  # Add the raster layer to the stack after flipping orientation if necessary
  
  print(paste0(tt[i], " of ", length(tt)))  # Print progress information
}



















# Extract DEM values to plot locations
plotcharsf <- dplyr::filter(plotchar, !is.na(Latitude))
plotcharsf <- sf::st_as_sf(plotcharsf, coords = c("Longitude", "Latitude"))
elev <- raster::extract(elevation, plotcharsf)

plotcharsf$DEMelev <- elev

# Not all plots have an elevation from DEM - some are missing lat/long
# Write to plotchar csv
plotcharsf <- sf::st_drop_geometry(plotcharsf)
plotchar <- dplyr::left_join(plotchar, plotcharsf)
write.csv(plotchar, "C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\Field Data\\CSV\\PlotChar.csv", row.names = FALSE)



