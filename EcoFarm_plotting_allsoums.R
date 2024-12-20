
# Plotting soil properties across all EcoFarm soums

library(tidyverse)
library(stringr)
library(gt)
library(gtable)

# Read in csvs
Xushaat0to30 <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\EcoFarm\\Xushaat0to30recalc.csv")

Bornuur0to30 <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\EcoFarm\\Bornuur0to30recalc.csv")

IkhUul0to30 <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\EcoFarm\\IkhUul0to30recalc.csv")

XutagOndor0to30 <- read.csv("C:\\Users\\alexheller\\Documents\\Soil Carbon Mongolia\\EcoFarm\\XutagOndor0to30recalc.csv")


unique(Xushaat0to30$ESG)


topc <- IkhUul0to30 %>%
  dplyr::group_by(Bagh, Property, ESG) %>%
  dplyr::summarise(Max = max(Mean))



# Combine into single table for database
names(Bornuur0to30)
Ecofarm0to30 <- Bornuur0to30 %>%
  rbind(IkhUul0to30) %>%
  rbind(Xushaat0to30) %>%
  rbind(XutagOndor0to30) %>%
  dplyr::select(Aimag, Soum, Bagh:Standard.deviation)


# Add cv 
names(Ecofarm0to30)
Ecofarm0to30 <- dplyr::mutate(Ecofarm0to30, CV = Standard.deviation/Mean)
Ecofarm0to30$CVP <- Ecofarm0to30$CV * 100
Ecofarm0to30$CV <- round(Ecofarm0to30$CV, 1)
Ecofarm0to30$CVP <- round(Ecofarm0to30$CVP, 1)


write.csv(Ecofarm0to30, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\ECCOFarmData.csv", row.names = FALSE)



# Built GT tables
# Bornuur
names(Bornuur0to30)
bornuur <- dplyr::filter(Bornuur0to30, Property == "Soil organic carbon stocks")
bornuur <- dplyr::select(bornuur, Bagh, ESG, Mean, 'Standard deviation' = Standard.deviation, Unit)



tabp1 <- gt(bornuur, groupname_col = "Bagh")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil organic carbon stocks by ESG and bagh, Bornuur soum, Tov aimag**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))


tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\", filename = "BornuurSOC.png", vwidth = 1500, vheight = 1000)



# Xushaat
xushaat <- dplyr::filter(Xushaat0to30, Property == "Soil organic carbon stocks")
xushaat<- dplyr::select(xushaat, Bagh, ESG, Mean, 'Standard deviation' = Standard.deviation, Unit)



tabp1 <- gt(bornuur, groupname_col = "Bagh")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil organic carbon stocks by ESG and bagh, Xushaat soum, Selenge aimag**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))


tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\", filename = "XushaatSOC.png", vwidth = 1500, vheight = 1000)





# Xutag-Ondor
xutag <- dplyr::filter(XutagOndor0to30, Property == "Soil organic carbon stocks")
xutag <- dplyr::select(xutag, Bagh, ESG, Mean, 'Standard deviation' = Standard.deviation, Unit)



tabp1 <- gt(bornuur, groupname_col = "Bagh")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil organic carbon stocks by ESG and bagh, Xutag-Ondor soum, Bulgan aimag**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))


tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\", filename = "XutagOndorSOC.png", vwidth = 1500, vheight = 1000)



# Ikh-Uul
ikh <- dplyr::filter(IkhUul0to30, Property == "Soil organic carbon stocks")
ikh <- dplyr::select(ikh, Bagh, ESG, Mean, 'Standard deviation' = Standard.deviation, Unit)



tabp1 <- gt(bornuur, groupname_col = "Bagh")

tabp1 

tabp1 <- cols_align(tabp1, align = "center", columns = everything())
tabp1  <- tab_header(tabp1 , title = md("**Soil organic carbon stocks by ESG and bagh, Ikh-Uul soum, Khovsgol aimag**"))
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_row_groups())
tabp1 <- tab_style(tabp1, style = cell_text(weight = "bold"),
                   locations = cells_column_labels())

tabp1 <- tab_footnote(tabp1,
                      footnote = "From SoilGrids rasterized data, Poggio et al. 2021",
                      locations = cells_title(groups = "title"))


tabp1 


gt::gtsave(tabp1, path = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\", filename = "IkhUulSOC.png", vwidth = 1500, vheight = 1000)





# Paired boxplot
# Bornuur
Bornuur0to30$ESG <- stringr::str_remove(Bornuur0to30$ESG, "\\.[^.]*$")

bar <- Bornuur0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(fill = ESG, y = Mean, x = Bagh)) +
  geom_col(position = position_dodge2(preserve = "single"), stat = "identity") +
  labs(title = "Mean soil organic carbon stocks across ESGs and Baghs, Bornuur soum, Tov aimag",
       y = "Soil organic carbon stock (t/ha)") 
  
bar

ggsave(plot = bar, filename = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\BornuurGroupedBar.png", width = 8, height = 6)


bar <- Bornuur0to30 %>%
  dplyr::filter(Property == "Total nitrogen") %>%
  ggplot(aes(fill = ESG, y = Mean, x = Bagh)) +
  geom_col(position = position_dodge2(preserve = "single"), stat = "identity") +
  labs(title = "Mean soil organic carbon stocks across ESGs and Baghs, Bornuur soum, Tov aimag",
       y = "Soil organic carbon stock (t/ha)") 
bar


box <- Bornuur0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(fill = ESG, y = Mean)) +
  geom_boxplot() +  
  ggtitle("Mean soil organic carbon stocks across ESGs and baghs, Bornuur soum, Tov") +
  theme(legend.position = "none") +
  ylab("Soil organic carbon stocks (t/ha)") +
  xlab("ESG") + 
  facet_wrap(~Bagh)


box




IkhUul0to30$ESG <- stringr::str_remove(IkhUul0to30$ESG, "\\.[^.]*$")

bar <- IkhUul0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(fill = ESG, y = Mean, x = Bagh)) +
  geom_col(position = position_dodge2(preserve = "single"), stat = "identity") +
  labs(title = "Mean soil organic carbon stocks across ESGs and Baghs, Ikh-Uul soum, Khovsgol aimag",
       y = "Soil organic carbon stock (t/ha)") 

bar

ggsave(plot = bar, filename = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\IkhUulGroupedBar.png", width = 8, height = 6)




Xushaat0to30$ESG <- stringr::str_remove(Xushaat0to30$ESG, "\\.[^.]*$")

bar <- Xushaat0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(fill = ESG, y = Mean, x = Bagh)) +
  geom_col(position = position_dodge2(preserve = "single"), stat = "identity") +
  labs(title = "Mean soil organic carbon stocks across ESGs and Baghs, Xushaat soum, Selenge aimag",
       y = "Soil organic carbon stock (t/ha)") 

bar

ggsave(plot = bar, filename = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\XushaatGroupedBar.png", width = 8, height = 6)



XutagOndor0to30$ESG <- stringr::str_remove(XutagOndor0to30$ESG, "\\.[^.]*$")

bar <- XutagOndor0to30 %>%
  dplyr::filter(Property == "Soil organic carbon stocks") %>%
  ggplot(aes(fill = ESG, y = Mean, x = Bagh)) +
  geom_col(position = position_dodge2(preserve = "single"), stat = "identity") +
  labs(title = "Mean soil organic carbon stocks across ESGs and Baghs, Xutag-Ondor soum, Bulgan aimag",
       y = "Soil organic carbon stock (t/ha)") 

bar

ggsave(plot = bar, filename = "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\XutagGroupedBar.png", width = 8, height = 6)









# Save areas to csv
areasbornuur$Soum <- "Bornuur"
areasbornuur$Aimag <- "Tov"
areasikhuul$Soum <- "Ikh-Uul"
areasikhuul$Aimag <- "Khovsgol"
areasxushaat$Soum <- "Xushaat"
areasxushaat$Aimag <- "Selenge"
areasxutag$Soum <- "Xutag-Ondor"
areasxutag$Aimag <- "Bulgan"

areas <- areasbornuur %>%
  rbind(areasikhuul) %>%
  rbind(areasxushaat) %>%
  rbind(areasxutag)

str(areas)
areasum <- areas %>%
  dplyr::group_by(Soum, ESG) %>%
  dplyr::rename(Area = 'Area, ha') %>%
  dplyr::summarise(TotalArea = sum(Area))



write.csv(areas, "C:\\Users\\TRAVAH\\Desktop\\Soil Carbon Mongolia\\EcoFarm\\Areas.csv", row.names = FALSE)

