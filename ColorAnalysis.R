# Estimating carbon content from soil color at NAMEM points

# Load libraries
library(tidyverse)
library(sf)
library(aqp)
library(ggplot2)
library(gt)

# Read in soil data
soils <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\DIMA\\CSV\\hzdima_clean.csv")

soildima <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\DIMA\\CSV\\soildima_clean.csv")

stms <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\NAMEM\\plots_stms.csv")
                     
soils_stms <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\NAMEM\\soils_stms_matched.csv")

study <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Monitoring Data\\NAMEM\\fs_s_fullsoils_esgmatch.csv")


# Add color hexes to soiljoin for plotting
soildima <- dplyr::left_join(soildima, soils)

hexes <- munsell2rgb(
  soildima$Hue,
  soildima$Value,
  soildima$Chroma,
  alpha = 1,
  maxColorValue = 1,
  return_triplets = FALSE,
  returnLAB = TRUE
)

soiljoin <- cbind(soildima, hexes)

# Remove those missing color
soiljoin <- dplyr::filter(soiljoin, !(is.na(L)))

unique(soiljoin$SITE.PLOT)
table(soiljoin$STM_2012)

  
# Set colors 

colors <- c( "springgreen1", "springgreen2",
             "springgreen3", "forestgreen", "springgreen4", 
             "gold1", "gold2", "gold3", "goldenrod", "gold4",
             "maroon1",
             "maroon2", "maroon3", "maroon4",
             "orangered", "orangered1", "orangered2", "orangered3",
             "dodgerblue2", "dodgerblue3",
             "dodgerblue4")






# Dataframe of first horizons
h1 <- dplyr::filter(study, Upper.Depth == 0)

h1 <- tidyr::unite(h1, ESG_RC, ESG, RC, sep = "_", remove = FALSE)


x <- h1 %>%
  dplyr::group_by(ESG_RC) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::filter(Count > 1)

study <- tidyr::unite(study, ESG_RC, ESG, RC, sep = "_", remove = FALSE)
study <- subset(study, study$ESG_RC %in% x$ESG_RC)

h1 <- dplyr::filter(study, Upper.Depth == 0)


# Bar plot count of ss in first horizon
hexsum <- h1 %>%
  subset(h1$ESG_RC %in% x$ESG_RC) %>%
  dplyr::group_by(hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()

hexsum$L <- as.numeric(hexsum$L)

# Regular barplot
ggplot(hexsum, aes(x = hexes, y = Count, fill = hexes, reorder(L))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = unique(hexsum$hexes))


# Stacked barplot - stacks are ESGs in forest steppe
hexsumlf <- h1 %>%
  dplyr::group_by(ESG, RC, hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()

hexsumlf$LF <- as.factor(hexsumlf$L)
hexsumlf$ESG <- as.factor(hexsumlf$ESG)

ggplot(hexsumlf, aes(fill = LF, y = Count, x  = ESG, fill = LF, reorder(L))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = unique(hexsum$hexes))

table(h1$ESG)

h1_2 <- dplyr::filter(h1, ESG == 7)
table(h1_2$RC)


hexsumlf <- h1_2 %>%
  dplyr::group_by(RC, hexes, L) %>%
  dplyr::summarise(Count = n()) %>%
  dplyr::ungroup()
hexsumlf$LF <- as.factor(hexsumlf$L)


ggplot(hexsumlf, aes(fill = LF, y = Count, x  = RC, fill = LF, reorder(L))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = unique(hexsum$hexes))



# Boxplots



h1 <- dplyr::filter(soildima, )
h1_2 <- dplyr::filter(h1, ESG == 9)

names(study)
unique(h1$Texture)
table(h1$Texture)

s <- h1 %>%
  dplyr::filter(ESG %in% c(1, 2, 3, 4, 5) & Texture == "SL")

ggplot(s, aes(x = RC, y = L, reorder(L))) +
  geom_boxplot() 


ggplot(h1_2, aes(x = RC, y = L, reorder(L))) +
  geom_boxplot() 



ggplot(h1, aes(x = RC, y = A, reorder(L))) +
  geom_boxplot() 

table(h1$RC)


h2 <- dplyr::filter(study, HZnum == 2)


ggplot(h2, aes(x = RC, y = A, reorder(L))) +
  geom_boxplot() 

h2_2 <- dplyr::filter(h1, ESG == 9)

ggplot(h2_2, aes(x = RC, y = L, reorder(L))) +
  geom_boxplot() 


h3 <- dplyr::filter(study, HZnum == 3)


ggplot(h3, aes(x = RC, y = L, reorder(A))) +
  geom_boxplot() 

# Ridge plot
library(ggridges)
h1$ESG <- as.factor(h1$ESG)
h1 <- dplyr::ungroup(h1)

ggplot(h1, aes(x = A, y = ESG, fill = RC)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("L value in Horizon 1")


ggplot(h2, aes(x = A, y = ESG, fill = RC)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("L value in Horizon 2")

ggplot(h3, aes(x = L, y = ESG, fill = RC)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("L value in Horizon 2")


# Correlation matrix
library(PerformanceAnalytics)
library(corrplot)

names(h1)
str(soiljoin)
soiljoin$Aspect <- as.numeric(soiljoin$Aspect)
soiljoin$Slope <- as.numeric(soiljoin$Slope)
cor <- dplyr::select(soiljoin, Slope, Aspect, L, A, PctClay)


chart.Correlation(cor, histogram=TRUE, pch=19)
       
corm <- cor(cor, use = "complete.obs")

corrplot(corm, type = "upper", 
         tl.col = "black", tl.srt = 45)
       
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




# Model
names(study)
model <- lm(L ~ Slope + Aspect + PctClay, data = soiljoin)

hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

summary(model)


# Make RC numneric
soiljoin <- dplyr::mutate(soiljoin, RCnum = ifelse(RC_2012 == "I", 1,
                                                   ifelse(RC_2012 == "II", 2,
                                            ifelse(RC_2012 == "III", 3, 
                                            ifelse(RC_2012 == "IV", 4,
                                            ifelse(RC_2012 == "V", 5, NA))))))



model <- lm(L ~ Slope + Aspect + PctClay + RCnum, data = soiljoin)
hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

summary(model)


# ANOVA
table(study$ESG)
x <- study %>%
  dplyr::group_by(ESG, RC) %>%
  dplyr::summarise(Count = n())
x <- h1 %>%
  dplyr::group_by(ESG, RC) %>%
  dplyr::summarise(Count = n())


# Explore within ESGs
library(ggpubr)
esg <- dplyr::filter(study, ESG == 2)
esg <- dplyr::filter(esg, HZnum == 2)

ggplot(esg) +
  aes(x = RC, y = L, color = RC) +
  geom_jitter() +
  theme(legend.position = "none")

hist(esg$L)
plot(density(esg$L))
ggqqplot(esg$L)


res_aov <- aov(L ~ RC,
               data = esg
)
summary(res_aov)

# ESG 8 (not filtered by horizon) has significant p-value (not significant for first horizon only)


# Whole dataset
g <- dplyr::filter(study, ESG %in% c(7, 8, 9))


ggplot(g) +
  aes(x = RC, y = L, color = RC) +
  geom_jitter() +
  theme(legend.position = "none")

hist(esg$L)
plot(density(esg$L))
ggqqplot(esg$L)


res_aov <- aov(L ~ RC,
               data = esg
)
summary(res_aov)
