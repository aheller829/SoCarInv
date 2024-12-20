# Libraries
library(tidyverse)
library(ggplot2)

# Read in CSV
data <- read.csv("C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\Data\\Burmaas_carbon_results.csv")

# Add unique plot name
data <- tidyr::unite(data, PlotName, Aimag:Plot, sep = "_", remove = FALSE)

  
data$PlotNameShort <- gsub("[a-z]", "", data$PlotName)
  
# Add horizon number
data <- data %>% 
  dplyr::group_by(PlotName) %>% 
  dplyr::mutate(HorizonNumber = row_number()) %>%
  dplyr::ungroup()

# Plot SOC by horizon by plot
ggplot(data, aes(x = PlotNameShort, y = Total_SOC_per_year_kg.m2, color = PlotNameShort))+
  geom_point()+
  facet_wrap(.~HorizonNumber, scales = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = PlotNameShort, y = t_c.ha, color = PlotNameShort))+
  geom_point()+
  facet_wrap(.~HorizonNumber, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Line
ggplot(data, aes(x = HorizonNumber, y = t_c.ha, color = PlotNameShort)) +
  geom_point() +
  geom_line()

# Box
ggplot(data, aes(x = PlotNameShort, y = t_c.ha)) +
  geom_boxplot()
