# Exploring soil characteristics across ESGs


# Load libraries
library(tidyverse)
library(sf)
library(aqp)
library(ggplot2)
library(gt)
library(vegan)
library(ggordiplots)

# Read in soil data
# NAMEM data from DIMA, ESG assigned by extracting raster value from ESG map
soils <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\DIMA\\CSV\\hzdima_clean.csv")

# Plot level data from DIMA, ESG assigned by extracting raster value from ESG map
soildima <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\DIMA\\CSV\\soildima_clean.csv")

# These plots are ones that could be joined with some confidence to Sumjee's data and to her assigned ESG/RC
stms <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\NAMEM\\plots_stms.csv")

# STM matches joined to horizon level data
soils_stms <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\NAMEM\\soils_stms_matched.csv")

# soils_stms subset to steppe and forest steppe sites
study <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\NAMEM\\fs_s_fullsoils_esgmatch.csv")

table(study$ESG)



# Calculate and add plot level soil variables
# Extract clay at standard depths
names(soils)
unique(soils$Effervescence)
# Rename variables to match existing esd-dev code, add ordinal eff column
soil.standards <- soils %>%
  dplyr::rename(HorizonDepthUpper = Upper.Depth,
                                HorizonDepthLower = Lower.Depth,
                TotalFragments = TotalFrags,
                PercentClay = PctClay) %>%
  dplyr::mutate(OrdinalEffervescence = ifelse(Effervescence == "NE", 1, 
                                        ifelse(Effervescence == "VS", 2,
                                        ifelse(Effervescence == "SL", 3,
                                        ifelse(Effervescence == "ST", 4,
                                        ifelse(Effervescence == "VS", 5, NA))))))
soil.standards <- soil.standards %>%
  mutate(Clay0 = ifelse(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, PercentClay, NA_integer_),
         Clay15 = ifelse(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, PercentClay, NA_integer_),
         Clay30 = ifelse(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, PercentClay, NA_integer_),
         Clay60 = ifelse(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, PercentClay, NA_integer_))
# Eff
soil.standards$OrdinalEffervescence <- as.integer(soil.standards$OrdinalEffervescence)
soil.standards <- soil.standards %>%
  mutate(Eff0 = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, OrdinalEffervescence, NA_integer_),
         Eff15 = if_else(HorizonDepthUpper < 15 & 16 < HorizonDepthLower, OrdinalEffervescence, NA_integer_),
         Eff30 = if_else(HorizonDepthUpper < 30 & 31 < HorizonDepthLower, OrdinalEffervescence, NA_integer_),
         Eff60 = if_else(HorizonDepthUpper < 60 & 61 < HorizonDepthLower, OrdinalEffervescence, NA_integer_))
# Fragments
soil.standards$TotalFragments <- as.integer(soil.standards$TotalFragments)
soil.standards <- soil.standards %>%
  mutate(Frags0 = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, TotalFragments, NA_integer_),
         Frags15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, TotalFragments, NA_integer_),
         Frags30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, TotalFragments, NA_integer_),
         Frags60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, TotalFragments, NA_integer_))
# Extract depth of each soil pit
depth.restrict <- soil.standards %>%
  dplyr::group_by(PK) %>%
  dplyr::slice(which.max(HorizonDepthLower)) %>%
  dplyr::ungroup() %>%
  dplyr::select(PK, Depth = HorizonDepthLower)
# Separate each variable type and collapse rows so there is a single row for each soil pit
clays <- soil.standards %>%
  dplyr::left_join(depth.restrict) %>%
  dplyr::select(PK, Clay0:Clay60, Depth) %>%
  dplyr::group_by(PK) %>%
  dplyr::summarise_each(funs(first(na.omit(.))))
effs <- soil.standards %>%
  dplyr::left_join(depth.restrict) %>%
  dplyr::select(PK, Eff0:Eff60, Depth) %>%
  dplyr:: group_by(PK) %>%
  dplyr:: summarise_each(funs(first(na.omit(.))))
frags <- soil.standards %>%
  dplyr::left_join(depth.restrict) %>%
  dplyr::select(PK, Frags0:Frags60, Depth) %>%
  dplyr::group_by(PK) %>%
  dplyr:: summarise_each(funs(first(na.omit(.))))
# Rejoin collapsed variables
soil.standards <- clays %>%
  left_join(effs) %>%
  left_join(frags)



# Create a new dataframe of the horizon with the highest effervescence class for each soil pit
max.eff <- effs %>%
  dplyr::select(-Depth) %>%
  tidyr::gather(key = Depth, value = Eff, 2:5) %>%
  dplyr::group_by(PK) %>%
  dplyr::arrange(Eff) %>%
  dplyr::slice(which.max(Eff)) %>%
  dplyr::select(PK, DepthMaxEff = Depth, MaxEff = Eff)
max.eff$DepthMaxEff <- as.numeric(stringr::str_remove(max.eff$DepthMaxEff, "Eff"))


# Extract horizon with greatest fragment content for each soil pit
max.frags <- frags %>%
  dplyr::select(-Depth) %>%
  tidyr::gather(key = Depth, value = MaxFrags, 2:5) %>%
  dplyr::group_by(PK) %>%
  dplyr::arrange(MaxFrags) %>%
  dplyr::slice(which.max(MaxFrags)) %>%
  dplyr::select(PK, DepthMaxFrags = Depth, MaxFrags)
max.frags$DepthMaxFrags <- as.numeric(stringr::str_remove(max.frags$DepthMaxFrags, "Frags"))

# Extract horizon with maximum clay for each plot
max.clays <- clays %>%
  dplyr::select(-Depth) %>%
  tidyr::gather(key = Depth, value = MaxClay, 2:5) %>%
  dplyr::group_by(PK) %>%
  dplyr::arrange(MaxClay) %>%
  dplyr::slice(which.max(MaxClay)) %>%
  dplyr::select(PK, DepthMaxClay = Depth, MaxClay)
max.clays$DepthMaxClay <- as.numeric(stringr::str_remove(max.clays$DepthMaxClay, "Clay"))


# Extract surface texture
surface.texture <- soils %>%
  filter(Upper.Depth == 0) %>%
  select(PK, SurfaceTexture = Texture)



# Combine desired variables
soilcovars <- soil.standards %>%
  dplyr::left_join(max.clays) %>%
  dplyr::left_join(max.eff) %>%
  dplyr::left_join(max.frags) %>%
  dplyr::left_join(surface.texture)


soils <- dplyr::left_join(soils, soilcovars, by = "PK")

names(soils)

soildima <- dplyr::left_join(soildima, soilcovars, by = "PK")


# Ordination
# Add row number
soils <- dplyr::mutate(soils, ID = rownames(soils))
soildima <- dplyr::mutate(soildima, ID = rownames(soildima))


names(soildima)

soildima.matrix <- dplyr::select(soildima, PK, NZ_ENG, ESGcode, Slope, Depth, Clay0,
                          DepthMaxClay, MaxClay, DepthMaxEff, MaxEff, 
                          DepthMaxFrags, MaxFrags)

soildima.matrix <- na.omit(soildima.matrix)


soildima.dist <- dplyr::select(soildima.matrix, Slope, Depth, Clay0,
                          DepthMaxClay, MaxClay, DepthMaxEff, MaxEff, 
                          DepthMaxFrags, MaxFrags)


# Ordination with PCOA
soildima.dist <- vegdist(soildima.dist, method = "gower", binary = FALSE)
# Run a PCoA on functional groups and structural indicators
soilPCA <- cmdscale(soildima.dist, k = 2)

# Plot with hulls
ordiplot(soilPCA, main = "PCoA - soil attributes")
fit <- envfit(soilPCA , soildima.matrix, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "red", cex = 0.7)

unique(soildima$ESGcode)
ordispider(soilPCA , soildima.matrix$ESG, col= "black", label = T)

ordiplot(soilPCA, main = "PCoA - soil attributes")
ordispider(soilPCA , soildima.matrix$NZ_ENG, col= "black", label = T)
plot(fit, p.max = 0.05, col = "red", cex = 0.7)







# Repeat with steppe and forest steppe only
unique(soildima$NZ_ENG)
soildima.matrix <- soildima %>%
  dplyr::select(PK, NZ_ENG, ESGcode, Slope, Depth, Clay0,
                                 DepthMaxClay, MaxClay, DepthMaxEff, MaxEff, 
                                 DepthMaxFrags, MaxFrags) %>%
  dplyr::filter(NZ_ENG == "Forest steppe")

soildima.matrix <- na.omit(soildima.matrix)


soildima.dist <- dplyr::select(soildima.matrix, Slope, Depth, Clay0,
                               DepthMaxClay, MaxClay, DepthMaxEff, MaxEff, 
                               DepthMaxFrags, MaxFrags)


# Ordination with PCOA
soildima.dist.m <- vegdist(soildima.dist, method = "gower", binary = FALSE)
# Run a PCoA on functional groups and structural indicators
soilPCA <- cmdscale(soildima.dist.m, k = 2)

# Plot with hulls
ordiplot(soilPCA, main = "PCoA - soil attributes")
fit <- envfit(soilPCA , soildima.matrix, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "red", cex = 0.7)

unique(soildima$ESGcode)
ordispider(soilPCA , soildima.matrix$ESG, col= "black", label = T)

ordiplot(soilPCA, main = "PCoA - soil attributes")
ordispider(soilPCA , soildima.matrix$NZ_ENG, col= "black", label = T)
plot(fit, p.max = 0.05, col = "red", cex = 0.7)



ordiplot(soilPCA, main = "PCoA - soil attributes")
unique(soildima.matrix$NZ_ENG)
ordihull(soilPCA, soildima.matrix$ESGcode, draw = "polygon", alpha = 0.3)
fit <- envfit(soilPCA , soildima.dist, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "red", cex = 0.7)
ordispider(soilPCA , soildima.matrix$ESGcode, col= "black", label = T)


# There is better differentiation when looking at just ESGs within forest steppe 
# Not well differentiated when looking at all natural zones, steppe and forest steppe, or steppe ESGs


# Try with Sumjee's data matched
names(soils_stms)
names(study)


pks <- dplyr::select(soils_stms, SITE.PLOT, PK)
pks <- dplyr::distinct(pks)

stms_study <- dplyr::select(study, SITE.PLOT, ESG_RC, ESG, RC, Elevation, L, A, B, MappedESG, TotalFoliar, BareGround)

# Extract standard L value
lval <- dplyr::select(study, SITE.PLOT, L, HorizonDepthUpper = Upper.Depth, HorizonDepthLower = Lower.Depth)
lval <- lval %>%
  mutate(L0 = if_else(HorizonDepthUpper == 0 | HorizonDepthUpper == 1, L, NA_integer_),
         L15 = if_else(HorizonDepthUpper < 15 & 14 < HorizonDepthLower, L, NA_integer_),
         L30 = if_else(HorizonDepthUpper < 30 & 29 < HorizonDepthLower, L, NA_integer_),
         L60 = if_else(HorizonDepthUpper < 60 & 59 < HorizonDepthLower, L, NA_integer_))
lval <- dplyr::select(lval, SITE.PLOT, L0:L60)
lval <- lval %>%
  tidyr::gather(key = Depth, value = MaxClay, 2:5) %>%
  dplyr::group_by(PK) %>%
  dplyr::arrange(MaxClay) %>%
  dplyr::slice(which.max(MaxClay)) %>%
  dplyr::select(PK, DepthMaxClay = Depth, MaxClay)
max.clays$DepthMaxClay <- as.numeric(stringr::str_remove(max.clays$DepthMaxClay, "Clay"))



stms_study <- dplyr::left_join(stms_study, pks)
