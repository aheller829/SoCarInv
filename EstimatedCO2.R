# Working with CO2 estimates for aimags, soums, ESGs
# The CO2 estimates are for grams of CO2 per kilogram of soil (g/kg) from 0-30 cm
# They come from the ESRI hosted raster layer of Soil Grids
# Pixel/cell size is 250 m

library(tidyverse)
library(sf)
library(stringr)
library(gt)
library(gtable)

# Calculated using ESGs clipped to soums of interest using zonal stats as table in ArcGIS pro

stats <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\COzonalstats.csv")

# Remove lake, cropland, forest
stats <- dplyr::filter(stats, NAME_ENG != "Lake" & NAME_ENG != "Cropland")
names(stats)
stats <- dplyr::select(stats, NAME_ENG, AREA, MIN, MAX, RANGE, MEAN, STD, MEDIAN)


# Add english natural zone column to esg shapefile
esgs <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\+20231005_ESG_By_aimag_New_STM\\ESG_22\\ESG_state.shp")

names(esgs)

unique(esgs$nat_zone)
esgs$nat_zone <- stringr::str_remove(esgs$nat_zone, "\r\n")
esgs$nat_zone <- trimws(esgs$nat_zone)

unique(esgs$nat_zone)

nz <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\NZ_single\\NZ_single.shp")
nz <- sf::st_drop_geometry(nz)
names(nz)
nz <- dplyr::select(nz, Name_ENG, Name_MON)
nz <- dplyr::distinct(nz)
nz <- dplyr::rename(nz, nat_zone = Name_MON, nat_zone_ENG = Name_ENG)

esgs <- dplyr::left_join(esgs, nz)
head(esgs)
names(esgs)

unique(esgs$Name_ENG)
esgs$Name_ENG <- stringr::str_remove(esgs$Name_ENG, "\r\n")
unique(esgs$Name_ENG)
esgs$Name_ENG <- trimws(esgs$Name_ENG)

esgs <- dplyr::mutate(esgs, Name_ENG = ifelse(Name_ENG == "2. Шавранцар хөрстэй уулын хажуу, бэлийн Алаг өвс-жижиг дэгнүүлт үетэнт уулын хээрийн бэлчээр",
                                              "2. Small bunch grass-Forbs  mountain steppe rangeland in Loamy fan ESG, Forest steppe", Name_ENG))
unique(esgs$Name_ENG)
unique(esgs$nat_zone)
names(esgs)

esgs <- dplyr::select(esgs, OBJECTID, nat_zone_ENG, Name_ENG, Shape_Leng, Shape_Area, geometry)


esgs <- esgs %>%
  dplyr::mutate(
    nat_zone_ENG = case_when(
      str_sub(Name_ENG, 1, 2) == '10' ~ 'Steppe',
      str_sub(Name_ENG, 1, 1) == '9' ~ 'Steppe',
      str_sub(Name_ENG, 1, 1) == '8' ~ 'Steppe',
      str_sub(Name_ENG, 1, 1) == '7' ~ 'Steppe',
      str_sub(Name_ENG, 1, 1) == '6' ~ 'Steppe',
      TRUE ~ nat_zone_ENG
    )
  )

unique(esgs$nat_zone_ENG)

esgs <- dplyr::mutate(esgs, nat_zone_ENG = ifelse(is.na(nat_zone_ENG), Name_ENG, nat_zone_ENG))

names(esgs)
esgs <- dplyr::rename(esgs, NZ_ENG = nat_zone_ENG, Sh_Length = Shape_Leng)

esgs <- dplyr::mutate(esgs, NZ_ENG = ifelse(NZ_ENG == "Desert stepe", "Desert steppe", NZ_ENG))

sf::st_write(esgs, "C:\\Users\\TRAVAH\\Documents\\GIS\\ESGS_dissolved.shp", driver = 'ESRI Shapefile', append = FALSE)





# Read in calculated zonal stats
esgstats <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\cozonalstats_ESGs.csv")

nzstats <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\cozonalstats_nzs.csv")

esgs <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\ESGS_dissolved.shp")

# Change decimal places
esgstats <- dplyr::mutate(esgstats, across(where(is.numeric), ~ round(., 2)))
nzstats <- dplyr::mutate(nzstats, across(where(is.numeric), ~ round(., 2)))


# Add natural zones to esgstats
nz_esg <- dplyr::select(esgs, NZ_ENG, NAME_ENG = Name_ENG)
nz_esg <- sf::st_drop_geometry(nz_esg)

esgstats <- dplyr::left_join(esgstats, nz_esg)

# NZ table
names(nzstats)
nzstats <- dplyr::select(nzstats, NZ_ENG, MIN, MAX, RANGE, MEAN, STD, MEDIAN)
nzstats <- dplyr::filter(nzstats, NZ_ENG != "Cropland" & NZ_ENG != "Lake" & NZ_ENG != "Forest")

nzstats <- dplyr::mutate(nzstats, NZ_ENG = ifelse(NZ_ENG == "Desert stepe", "Desert steppe", NZ_ENG))

nzstats <- dplyr::rename(nzstats, "Natural Zone" = NZ_ENG)

nz <- gt(nzstats)
nz

nz <- cols_align(nz, align = "center", columns = everything())
nz <- tab_header(nz, title = md("**Estimated Soil Organic Carbon Stocks, t/ha (0-30 cm)**"))
nz <- tab_style(nz, style = cell_text(weight = "bold"),
    locations = cells_column_labels())
nz <- tab_footnote(nz,
    footnote = "From SoilGrids SOC raster, Poggio et al. 2021",
    locations = cells_title(groups = "title"))
    
nz
    

nz <- tab_style(nz, cell_fill("green3"), locations = cells_body(rows = 4))
nz <- tab_style(nz, cell_fill("darkorange"), locations = cells_body(rows = 1))
nz <- tab_style(nz, cell_fill("magenta3"), locations = cells_body(rows = 2))
nz <- tab_style(nz, cell_fill("orangered2"), locations = cells_body(rows = 3))
nz <- tab_style(nz, cell_fill("royalblue3"), locations = cells_body(rows = 5))

nz
nz <- tab_style(nz, style = cell_text(weight = "bold"),
                locations = cells_body(columns = vars("MEAN")))

nz

# ESG level table
names(esgstats)
str(esgstats)
esgstats$"ESG Number" <- as.numeric(gsub("\\D", "", esgstats$NAME_ENG))

esgstats <- dplyr::select(esgstats, "Natural Zone" = Natural_Zone, "ESG Name" = NAME_ENG, MIN:STD, MEDIAN)

esgstats <- dplyr::arrange(esgstats, desc('Natural Zone'))

esgstats <- dplyr::select(esgstats, "Natural Zone", "ESG Name", MEAN, MEDIAN, STD, MIN, MAX, RANGE)

es <- gt(esgstats, rowname_col = "ESG Name",
         groupname_col = "Natural Zone")

es

es <- cols_align(es, align = "center", columns = everything())
es <- tab_header(es, title = md("**Estimated Soil Organic Carbon Stocks, t/ha (0-30 cm)**"))
es <- tab_style(es, style = cell_text(weight = "bold"),
                locations = cells_column_labels())
es <- tab_footnote(es,
                   footnote = "From SoilGrids SOC raster, Poggio et al. 2021",
                   locations = cells_title(groups = "title"))

es

colors <- c("green3", "darkorange", "magenta3", "orangered2", "royalblue3")

es <- tab_style(es, cell_fill("green3"), locations = cells_row_groups("Forest steppe"))
es <- tab_style(es, cell_fill("darkorange"), locations = cells_row_groups("Steppe"))
es <- tab_style(es, cell_fill("magenta3"), locations = cells_row_groups("Desert steppe"))
es <- tab_style(es, cell_fill("orangered2"), locations = cells_row_groups("Desert"))
es <- tab_style(es, cell_fill("royalblue3"), locations = cells_row_groups("High mountain"))


es

es <- tab_style(es, style = cell_text(weight = "bold"),
                locations = cells_body(columns = vars("MEAN")))


es

gt::gtsave(es, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "WholeMNGCO2est.png", vwidth = 1500, vheight = 1000)

gt::gtsave(nz, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "NZWholeMNGCO2est.png", vwidth = 1500, vheight = 1000)


# Calculate variance and CV
names(esgstats)
esgstats <- dplyr::mutate(esgstats, CV = STD/MEAN)
esgstats$CVP <- esgstats$CV * 100
# Read in ESG areas
esgareas <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\esgareas.csv")
esgareas$"ESG Number" <- as.numeric(gsub("\\D", "", esgareas$Name_ENG))
esgareas <- dplyr::filter(esgareas, !is.na("ESG Number"))
esgstats <- dplyr::filter(esgstats, !is.na('ESG Number'))

esgstats <- dplyr::left_join(esgstats, esgareas, by = "ESG Number")

names(esgstats)

esgstats <- dplyr::select(esgstats, NAME_ENG, Name_ENG, "ESG Number", MIN:PCT90, CV, 
                          CVP, AreaHct)

esgstats <- dplyr::filter(esgstats, Name_ENG != "Forest" & Name_ENG != "Cropland" &
                            Name_ENG != "Lake")

write.csv(esgstats, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\ESGstats.csv", row.names = FALSE)

# ESGs by aimags
steppeaimags <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\SteppeAimags.shp")
fsaimags <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\ForestSteppeAimags.shp")

esgs <- sf::st_read("C:\\Users\\TRAVAH\\Documents\\GIS\\ESGS_dissolved.shp")

unique(esgs$Name_ENG)

FSesgs <- dplyr::filter(esgs, Name_ENG == "1. Festuca-Forbs mountain steppe rangeland in Gravelly hills  and fan ESG, Forest steppe" |
                        Name_ENG == "2. Small bunch grass-Forbs  mountain steppe rangeland in Loamy fan ESG, Forest steppe" |
                        Name_ENG == "3. Forbs-Grass-Carex with Dasiphora fruticosa meadow steppe rangeland in Loamy fan ESG, Forest steppe" |
                        Name_ENG == "4. Stipa baicalensis-Forbs meadow steppe rangeland in Mountain valley ESG, Forest steppe" |
                        Name_ENG == "5. Grass-Forbs riparian rangeland in High water table ESG, Forest steppe")

Sesgs <- dplyr::filter(esgs, Name_ENG == "6. Stipa Krylovii-Small bunch grass-Forbs dry steppe rangeland in Gravelly hills and fan ESG, Steppe" |
                         Name_ENG == "7. Stipa krylovii-grass dry steppe rangeland in Sandy loam alluvial fan and plain ESG, Steppe" |
                         Name_ENG == "8. Stipa krylovii-grass with Caragana steppe rangeland in Deep sandy alluvial plain ESG, Steppe" |
                         Name_ENG == "9. Stipa grandis-Elymus chinensis-Forbs dry steppe rangeland in Sandy loam ESG, Steppe")

fsaimags <- sf::st_transform(fsaimags, st_crs(esgs))
steppeaimags <- sf::st_transform(steppeaimags, st_crs(esgs))

steppeaimags_esgs <- sf::st_join(steppeaimags, Sesgs)
plot(steppeaimags_esgs)





# Make table of aimag level sampling requirements
ests <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Sample design\\NABUsamplerequirements.csv")

es <- gt(ests)

es

es <- cols_align(es, align = "center", columns = everything())
es <- tab_header(es, title = md("**Sampling Requirements at the Aimag Level for 90% CI and 95% CI**"))
es <- tab_style(es, style = cell_text(weight = "bold"),
                locations = cells_column_labels())

es <- cols_label(es,
   ESGarea = html("ESG Area (ha)"),
   MeanSOC = html("Mean SOC (t C/ha)"),
   CV = html("CV (%)"),
   SD = html ("SD (%)"),
   PlotSize = html("Plot Size (ha)"),
   SitesPerStrata_90 = html("Sites/composites per strata, 90% CI"),
   HaPerPlot_90 = html("Ha/plot, 90% CI"),
   SitesPerStrata_95 = html("Sites/composites per strata, 95% CI"),
   HaPerPlot_95 = html("Ha/plot, 95% CI")
)

es

es <- tab_footnote(es,
                   footnote = "From SoilGrids SOC raster, Poggio et al. 2021",
                   locations = cells_column_labels(columns = MeanSOC))
es <- tab_footnote(es,
                   footnote = "Number of sites and composite samples (based on a composite of 10) needed for 90% CI, 10% level of error",
                   locations = cells_column_labels(columns = c(SitesPerStrata_90, HaPerPlot_90)))
es <- tab_footnote(es,
                   footnote = "Number of sites and composite samples (based on a composite of 10) needed for 95% CI, 5% level of error",
                   locations = cells_column_labels(columns = c(SitesPerStrata_95, HaPerPlot_95)))

es


es <- tab_style(es, 
                cell_fill("yellow"), 
                locations = cells_body(columns = SitesPerStrata_90))
             

es

gt::gtsave(es, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\MongoliaSoilCarbon\\Outputs", filename = "NABUsamplerequirements.png", vwidth = 1500, vheight = 1000)

