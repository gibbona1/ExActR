# Francesco
# April 13, 2023

## Create the accounts of extent at the site level
# Example site: HAZELWOOD 



# Libraries
library(terra)
library(ggplot2)
library(cowplot)
library(tidyverse) # NOTE: this package conflict with "terra", so might need to specify which package is used for some functions
library(tidyterra)
library(openxlsx)

# Enter Ireland level data

## Coillte properties (includes polygons of all Coillte sites)
Cproperty <- vect("data/01_GIS_Coillte/01_Coillte_GISData/Property.shp")
## Corine land use map from 2018
CLC18 <- vect("data/02_GIS_shapefiles/CLC2018/CLC18_IE_ITM_Terrestrial.shp") 
## Corine land use map from 2000
CLC00 <- vect("data/02_GIS_shapefiles/CLC2000/CORINELandcover_2000Rev_Terrestrial.shp") 

# make sure all vectors have same coordinate system WGS84
Cproperty <- project(Cproperty, "EPSG:4326")
CLC18 <- project(CLC18, "EPSG:4326")
CLC00 <- project(CLC00, "EPSG:4326")


## Select example property: Hazelwood
hazelwood <- subset(Cproperty, Cproperty$PROPERTY_N == "HAZELWOOD") 

# Crop CORINE for selected site
hazelwood_CLC00 <- terra::crop(CLC00, hazelwood)
hazelwood_CLC18 <- terra::crop(CLC18, hazelwood)


## Dataframe with "Class_Desc" and "CODE_18" columns
CLC18_Codes_df <- as.data.frame(CLC18) %>% select(CODE_18, Class_Desc) %>% unique()
CLC00_Codes_df <- as.data.frame(CLC00) %>% select(CODE_00) %>% unique()
# Create dataframe with 2000 and 2018 codes to have matching class
CLC00_Codes_df$CODE_18 <- CLC00_Codes_df$CODE_00
CLCall_Codes_df <- dplyr::full_join(CLC00_Codes_df, CLC18_Codes_df, by="CODE_18")


# Calculate area of land use type from Corine land cover
# 2000
hazelwood_CLC00_df <- as.data.frame(hazelwood_CLC00)
hazelwood_CLC00_df$Area_ha<- expanse(hazelwood_CLC00, unit="ha")
# 2018
hazelwood_CLC18_df <- as.data.frame(hazelwood_CLC18)
hazelwood_CLC18_df$Area_ha<- expanse(hazelwood_CLC18, unit="ha")


# Rename to have matching class (Class_Desc)
hazelwood_CLC00_df <- dplyr::inner_join(hazelwood_CLC00_df, CLCall_Codes_df[,c(1,3)], by="CODE_00")


# Total area of Hazelwood (ha) - 2018 CLC
hazelwood_CLC18_df %>% summarize(TotArea = round(sum(Area_ha),2)) # 128.77 ha
# Total area of Hazelwood (ha) - 2000 CLC
hazelwood_CLC00_df %>% summarize(TotArea = round(sum(Area_ha),2)) # 128.77 ha - to check they match


# Area of Hazelwood by land use (ha) for 2000 and 2018
# 2000
hazelwood_CLC00_df_type <- hazelwood_CLC00_df %>% group_by(Class_Desc) %>%
  summarize(Area_2000 = round(sum(Area_ha),2))
# 2018
hazelwood_CLC18_df_type <- hazelwood_CLC18_df %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))

hazelwood_CLC_Table <- Reduce(function(...) merge(..., by='Class_Desc', all=TRUE), list(hazelwood_CLC00_df_type, hazelwood_CLC18_df_type))

hazelwood_CLC_Table <- hazelwood_CLC_Table %>% replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2))



## Calculate individual Reductions in CLC between 2000 and 2018 for each type

# Water courses (511) REDUCTIONS
hazelwood_CLC00_Code511 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "511")    # Water courses in 2000
hazelwood_CLC18_Area511from00 <- terra::crop(hazelwood_CLC18, hazelwood_CLC00_Code511)  # Water courses from 2000 unchanged in 2018

# 2000 area
hazelwood_CLC00_Code511_df <- hazelwood_CLC00_Code511 %>% as.data.frame() %>% mutate(Area_ha = expanse(hazelwood_CLC00_Code511, unit="ha")) %>%
  inner_join(CLCall_Codes_df[,c(1,3)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2))
# 2018 area
hazelwood_CLC18_Area511from00_df <- hazelwood_CLC18_Area511from00 %>% as.data.frame() %>% 
  mutate(Area_ha = expanse(hazelwood_CLC18_Area511from00, unit="ha")) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))

# Reductions in ha and % and changes to what other Land Use
hazelwood_CLC_Code511_Table <- full_join(hazelwood_CLC00_Code511_df, hazelwood_CLC18_Area511from00_df, by = "Class_Desc") %>%
  rename("Class_Desc2018" = "Class_Desc") %>%
  mutate(ClassDesc2000 = "Water courses") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  relocate(ClassDesc2000) %>% as.data.frame()


# Water bodies (512) REDUCTIONS
hazelwood_CLC00_Code512 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "512")     # Water bodies in 2000
hazelwood_CLC18_Area512from00 <- terra::crop(hazelwood_CLC18, hazelwood_CLC00_Code512)   # Water bodies from 2000 unchanged in 2018

# 2000
hazelwood_CLC00_Code512_df <- hazelwood_CLC00_Code512 %>% as.data.frame() %>% mutate(Area_ha = expanse(hazelwood_CLC00_Code512, unit="ha")) %>%
  inner_join(CLCall_Codes_df[,c(1,3)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2))
# 2018
hazelwood_CLC18_Area512from00_df <- hazelwood_CLC18_Area512from00 %>% as.data.frame() %>% 
  mutate(Area_ha = expanse(hazelwood_CLC18_Area512from00, unit="ha")) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))

hazelwood_CLC_Code512_Table <- full_join(hazelwood_CLC00_Code512_df, hazelwood_CLC18_Area512from00_df, by = "Class_Desc") %>%
  rename("Class_Desc2018" = "Class_Desc") %>%
  mutate(ClassDesc2000 = "Water bodies") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  relocate(ClassDesc2000) %>% as.data.frame()


# Broad-leaved forest (311) REDUCTIONS
hazelwood_CLC00_Code311 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "311")      # Broad-leaved forest from 2000
hazelwood_CLC18_Area311from00 <- terra::crop(hazelwood_CLC18, hazelwood_CLC00_Code311)    # Broad-leaved forest from 2000 unchanged in 2018

# 2000
hazelwood_CLC00_Code311_df <- hazelwood_CLC00_Code311 %>% as.data.frame() %>% mutate(Area_ha = expanse(hazelwood_CLC00_Code311, unit="ha")) %>%
  inner_join(CLCall_Codes_df[,c(1,3)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2))
# 2018
hazelwood_CLC18_Area311from00_df <- hazelwood_CLC18_Area311from00 %>% as.data.frame() %>% 
  mutate(Area_ha = expanse(hazelwood_CLC18_Area311from00, unit="ha")) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))


hazelwood_CLC_Code311_Table <- full_join(hazelwood_CLC00_Code311_df, hazelwood_CLC18_Area311from00_df, by = "Class_Desc") %>%
  rename("Class_Desc2018" = "Class_Desc") %>%
  mutate(ClassDesc2000 = "Broad-leaved forest") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  relocate(ClassDesc2000) %>% as.data.frame()

# Pastures (231) REDUCTIONS
hazelwood_CLC00_Code231 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "231")       # Pastures from 2000
hazelwood_CLC18_Area231from00 <- terra::crop(hazelwood_CLC18, hazelwood_CLC00_Code231)     # Pastures from 2000 unchanged in 2018

# 2000
hazelwood_CLC00_Code231_df <- hazelwood_CLC00_Code231 %>% as.data.frame() %>% mutate(Area_ha = expanse(hazelwood_CLC00_Code231, unit="ha")) %>%
  inner_join(CLCall_Codes_df[,c(1,3)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2))
# 2018
hazelwood_CLC18_Area231from00_df <- hazelwood_CLC18_Area231from00 %>% as.data.frame() %>% 
  mutate(Area_ha = expanse(hazelwood_CLC18_Area231from00, unit="ha")) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))

hazelwood_CLC_Code231_Table <- full_join(hazelwood_CLC00_Code231_df, hazelwood_CLC18_Area231from00_df, by = "Class_Desc") %>%
  rename("Class_Desc2018" = "Class_Desc") %>%
  mutate(ClassDesc2000 = "Pastures") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  relocate(ClassDesc2000) %>% as.data.frame()


## Combine all reductions, calculate additions and net change by land use type
hazelwood_CLC_ExtentAccout <- rbind(hazelwood_CLC_Code511_Table, hazelwood_CLC_Code512_Table,
                               hazelwood_CLC_Code311_Table, hazelwood_CLC_Code231_Table) %>%
  rename("Class_Desc" = "ClassDesc2000") %>%
  group_by(Class_Desc) %>% 
  filter(Change_00_18_ha > 0) %>% 
  summarize(Reductions = sum(Change_00_18_ha)*-1) %>%
  left_join(hazelwood_CLC_Table[,c(1:3)], by="Class_Desc") %>%
  mutate(NetChange = round(Area_2018 - Area_2000,2),
         Additions = round(NetChange - Reductions,2)) %>% 
  mutate(NetChange_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2),
         Additions_Perc = round(Additions/Area_2000*100,2),
         Reductions_Perc = round(Reductions/Area_2000*100,2)) %>% as.data.frame()



## Table of individual changes
hazelwood_CLC_ChangeMatrix <- rbind(hazelwood_CLC_Code511_Table, hazelwood_CLC_Code512_Table,
                                    hazelwood_CLC_Code311_Table, hazelwood_CLC_Code231_Table) %>%
  filter(Change_00_18_ha >0) %>% select(1,2,5)




## Plot map and  bar plots ------------

# Set colors (for all CLC in our sites)
CLC_colors <- c("Mixed forest" = "#336600",
                "Coniferous forest"="#ea6218",
                "Broad-leaved forest" = "#00CC33",
                "Peat bogs" = "#1360ed",
                "Water bodies" = "#00CCFF",
                "Water courses" = "#0099FF",
                "Transitional woodland-shrub" = "#00FF99",
                "Burned areas" = "#666000",
                "Pastures" = "#e8fa5bff",
                "Bare rocks" = "#CCCCCC",
                "Sparsely vegetated areas" = "#009966",
                "Inland marshes" = "#66CCCC",
                "Land principally occupied by agriculture, with significant areas of natural vegetation" = "#FF9933",
                "Moors and heathland" = "#FF3399",
                "Natural grasslands" = "#FFFF00",
                "Discontinuous urban fabric" = "#000000",
                "Estuaries" = "#00CCCC",
                "Intertidal flats" = "#0099FF",
                "Non-irrigated arable land" = "#CCFFCC",
                "Fruit trees and berry plantations" = "#00CC99")


# Add Class_desc from CLC2018 to calculate changes from 2000 to 2018 and plot with full name of land use
hazelwood_CLC18$CODE_00 <- hazelwood_CLC18$CODE_18
hazelwood_CLC00 <- merge(hazelwood_CLC00, hazelwood_CLC18, by="CODE_00", all.x = T) %>%
  mutate(Class_Desc = case_when(CODE_00 == 511 ~ "Water courses",
                                TRUE ~ as.character(Class_Desc)))


x11()
# MAP hazelwood CLC 2018
HAZEL_CLC18_plot <- ggplot() +
  geom_spatvector(data = hazelwood_CLC18, aes(fill=Class_Desc), color=NA) +
  labs(title = 'Hazelwood - 2018') + 
  theme_bw() + scale_fill_manual(values=CLC_colors) + 
  theme(legend.position = "none") +
  coord_sf(crs = "EPSG:4326")

# MAP hazelwood CLC 2000
HAZEL_CLC00_plot <- ggplot() +
  geom_spatvector(data = hazelwood_CLC00, aes(fill=Class_Desc), color=NA) +
  labs(title = 'Hazelwood - 2000') + 
  theme_bw() + scale_fill_manual(values=CLC_colors) + 
  theme(legend.position = "none") +
  coord_sf(crs = "EPSG:4326")

mapHAZEL00_legend <- get_legend(
  HAZEL_CLC00_plot + theme(legend.position = "right") +
    guides(fill=guide_legend(title="Corine Land Cover")))

plot_grid(HAZEL_CLC00_plot, HAZEL_CLC18_plot,mapHAZEL00_legend, nrow = 1, rel_widths = c(1,1,0.8), rel_heights = c(1,1,0.3))




# Plot bar plots of land use in 2000 and 2018 in hectares
hazelwood_CLC_ExtentAccout %>% select(1,3,4) %>%  
  gather(Year, Area, Area_2000:Area_2018) %>% 
  mutate(Year = recode(Year, "Area_2000" = "2000",
                            "Area_2018" = "2018")) %>%
ggplot(aes(x=Year, y = Area,fill=Class_Desc)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(y = "Area (ha)", title = "Hazelwood") +
  scale_fill_manual(values=CLC_colors) +
  theme_classic(base_size=15) +
  guides(fill = guide_legend(title = ""))

# Plot bar plots of land use in 2000 and 2018 in %
hazelwood_CLC_ExtentAccout %>% select(1,3,4) %>%  
  gather(Year, Area, Area_2000:Area_2018) %>% 
  mutate(Year = recode(Year, "Area_2000" = "2000",
                       "Area_2018" = "2018")) %>%
  mutate(Area_Perc = case_when(Year == "2000" ~ Area/(sum(Area)/2),
                               Year == "2018" ~ Area/(sum(Area)/2))) %>%
  ggplot(aes(x=Year, y = Area_Perc,fill=Class_Desc)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(y = "Area (%)", title = "Hazelwood") +
  scale_fill_manual(values=CLC_colors) +
  theme_classic(base_size=15) +
  scale_y_continuous(labels=scales::percent) +
  guides(fill = guide_legend(title = ""))


# Plot bar plots of land use change from 2000 to 2018
ggplot(hazelwood_CLC_ExtentAccout, aes(Class_Desc, NetChange, fill=Class_Desc)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=CLC_colors) +
  theme_classic(base_size=15) +
  labs(x="", y="Area Change (ha)", title = "Hazelwood") +
  theme(legend.position = "none") + 
  coord_flip() 



