# Francesco
# April 13, 2023

## Create the accounts of extent at the site level
# Example site: HAZELWOOD 



# Libraries
library(sf)
library(ggplot2)
library(cowplot)
library(tidyverse) # NOTE: this package conflict with "terra", so might need to specify which package is used for some functions
library(openxlsx)
library(ggspatial)

sf_use_s2(FALSE)

##-------------------------------------------------
# Prepare Corine data for site (if already ready, go at L60 or L75)
##-------------------------------------------------
# Enter Ireland level data

## Coillte properties (includes polygons of all Coillte sites)
Cproperty <- st_read("Hazelwood/Property.shp")
## Corine land use map from 2000
CLC00 <- st_read("CORINE_data/CORINELandcover_2000Rev_Terrestrial.shp") 
## Corine land use map from 2006
CLC06 <- st_read("CORINE_data/clc06_IE_v3.shp")
## Corine land use map from 2012
CLC12 <- st_read("CORINE_data/CLC12Rev_IE_ITM.shp")
## Corine land use map from 2018
CLC18 <- st_read("CORINE_data/CLC18_IE_ITM_Terrestrial.shp") 


# make sure all vectors have same coordinate system WGS84
Cproperty <- st_transform(Cproperty, "EPSG:4326")
CLC00 <- st_transform(CLC00, "EPSG:4326")
CLC06 <- st_transform(CLC06, "EPSG:4326")
CLC12 <- st_transform(CLC12, "EPSG:4326")
CLC18 <- st_transform(CLC18, "EPSG:4326")



## Select example property: Hazelwood
hazelwood <- subset(Cproperty, Cproperty$PROPERTY_N == "HAZELWOOD") 

# Crop CORINE for selected site
hazelwood_CLC00 <- st_intersection(CLC00, hazelwood)
hazelwood_CLC06 <- st_intersection(CLC06, hazelwood)
hazelwood_CLC12 <- st_intersection(CLC12, hazelwood)
hazelwood_CLC18 <- st_intersection(CLC18, hazelwood)

#st_write(hazelwood_CLC06, "hazelwood_CLC2006.shp")
#st_write(hazelwood_CLC12, "hazelwood_CLC2012.shp")

##-------------------------------------------------
# Data already cropped
##-------------------------------------------------
## Dataframe with "Class_Desc" and "CODE_00" to "CODE_18" columns
CLC18_Codes_df <- as.data.frame(CLC18) %>% select(CODE_18, Class_Desc) %>% unique()
CLC12_Codes_df <- as.data.frame(CLC12) %>% select(CODE_12, Class_Desc) %>% unique()
CLC06_Codes_df <- as.data.frame(CLC06) %>% select(CODE_06) %>% unique()
CLC00_Codes_df <- as.data.frame(CLC00) %>% select(CODE_00) %>% unique()
# Rename to have matching class
CLCall_Codes_df <- dplyr::full_join(CLC18_Codes_df, CLC12_Codes_df,by="Class_Desc")
CLC06_Codes_df$CODE_18 <- CLC06_Codes_df$CODE_06
CLCall_Codes_df <- dplyr::full_join(CLC06_Codes_df, CLCall_Codes_df, by="CODE_18")
CLC00_Codes_df$CODE_18 <- CLC00_Codes_df$CODE_00
CLCall_Codes_df <- dplyr::full_join(CLC00_Codes_df, CLCall_Codes_df, by="CODE_18") %>%
  relocate(CODE_00, CODE_06, CODE_12, CODE_18, Class_Desc) 


# Site CORINE layers
hazelwood_CLC00 <- st_read("Hazelwood/hazelwood_CLC2000.shp")
hazelwood_CLC06 <- st_read("Hazelwood/hazelwood_CLC2006.shp")
hazelwood_CLC12 <- st_read("Hazelwood/hazelwood_CLC2012.shp")
hazelwood_CLC18 <- st_read("Hazelwood/hazelwood_CLC2018.shp")


# Calculate area of land cover type from Corine land cover
# 2000
hazelwood_CLC00$Area_ha<- st_area(hazelwood_CLC00$geometry)/10000
# 2006
hazelwood_CLC06$Area_ha<- st_area(hazelwood_CLC06$geometry)/10000
# 2012
hazelwood_CLC12$Area_ha<- st_area(hazelwood_CLC12$geometry)/10000
# 2018
hazelwood_CLC18$Area_ha<- st_area(hazelwood_CLC18$geometry)/10000


# Rename to have matching class (Class_Desc)
hazelwood_CLC00 <- dplyr::inner_join(hazelwood_CLC00, CLCall_Codes_df[,c(1,5)], by="CODE_00")
hazelwood_CLC06 <- dplyr::inner_join(hazelwood_CLC06, CLCall_Codes_df[,c(2,5)], by="CODE_06")


# Total area of Hazelwood (ha) - 2000 CLC
hazelwood_CLC00 %>% summarize(TotArea = round(sum(Area_ha),2)) # 128.77 ha 
# Total area of Hazelwood (ha) - 2006 CLC
hazelwood_CLC06 %>% summarize(TotArea = round(sum(Area_ha),2)) # 128.77 ha 
# Total area of Hazelwood (ha) - 2012 CLC
hazelwood_CLC12 %>% summarize(TotArea = round(sum(Area_ha),2)) # 128.77 ha 
# Total area of Hazelwood (ha) - 2018 CLC
hazelwood_CLC18 %>% summarize(TotArea = round(sum(Area_ha),2)) # 128.77 ha - to check they all match



# Area of HAZELWOOD by land use (ha) for 2000, 2006, 2012, 2018
# 2000
hazelwood_CLC00_df_type <- hazelwood_CLC00 %>% as.data.frame() %>% group_by(Class_Desc) %>%
  summarize(Area_2000 = round(sum(Area_ha),2))
# 2006
hazelwood_CLC06_df_type <- hazelwood_CLC06 %>% as.data.frame() %>%group_by(Class_Desc) %>%
  summarize(Area_2006 = round(sum(Area_ha),2))
# 2012
hazelwood_CLC12_df_type <- hazelwood_CLC12 %>% as.data.frame() %>% group_by(Class_Desc) %>%
  summarize(Area_2012 = round(sum(Area_ha),2))
# 2018
hazelwood_CLC18_df_type <- hazelwood_CLC18 %>% as.data.frame() %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))


hazelwood_CLC_Table <- Reduce(function(...) merge(..., by='Class_Desc', all=TRUE), list(hazelwood_CLC00_df_type, hazelwood_CLC06_df_type,
                                                                                    hazelwood_CLC12_df_type, 
                                                                                    hazelwood_CLC18_df_type))

hazelwood_CLC_Table <- hazelwood_CLC_Table %>% replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2),
         Change_00_06_ha = round(Area_2006 - Area_2000, 2),
         Change_00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Change_06_12_ha = round(Area_2012 - Area_2006, 2),
         Change_06_12_Perc = round((Area_2012 - Area_2006)/Area_2006*100, 2),
         Change_12_18_ha = round(Area_2018 - Area_2012, 2),
         Change_12_18_Perc = round((Area_2018 - Area_2012)/Area_2012*100, 2)) %>%
  mutate_at(c('Area_2000', 'Area_2006', 'Area_2012', 'Area_2018', 
              'Change_00_18_ha', 'Change_00_18_Perc',
              'Change_00_06_ha', 'Change_00_06_Perc',
              'Change_06_12_ha', 'Change_06_12_Perc',
              'Change_12_18_ha', 'Change_12_18_Perc'), as.numeric)

hazelwood_CLC_Table %>% select(2:5) %>% colSums() # to check areas are correct





## Calculate individual Reductions in CLC between 2000 and 2018 for each type

# Water courses (511) REDUCTIONS
hazelwood_CLC00_Code511 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "511") %>% select(6)
totArea2000_code511 <- sum(st_area(hazelwood_CLC00_Code511$geometry)/10000) %>% as.numeric()# 5.45909
hazelwood_CLC06_Area511from00 <- st_intersection(hazelwood_CLC06, hazelwood_CLC00_Code511)
hazelwood_CLC06_Code511 <- subset(hazelwood_CLC06, hazelwood_CLC06$CODE_06 == "511") %>% select(6)
totArea2006_code511 <- sum(st_area(hazelwood_CLC06_Code511$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC12_Area511from06 <- st_intersection(hazelwood_CLC12, hazelwood_CLC06_Code511)
hazelwood_CLC12_Code511 <- subset(hazelwood_CLC12, hazelwood_CLC12$CODE_12 == "511") %>% select(6)
totArea2012_code511 <- sum(st_area(hazelwood_CLC12_Code511$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC18_Area511from12 <- st_intersection(hazelwood_CLC18, hazelwood_CLC12_Code511)
hazelwood_CLC18_Area511from00 <- st_intersection(hazelwood_CLC18, hazelwood_CLC00_Code511)

# 2000
hazelwood_CLC00_Code511_df <- hazelwood_CLC00_Code511 %>% as.data.frame() %>% mutate(Area_ha = st_area(hazelwood_CLC00_Code511$geometry)/10000) %>%
  inner_join(CLCall_Codes_df[,c(1,5)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2)) %>% mutate_at('Area_2000', as.numeric)
# 2006
hazelwood_CLC06_Area511from00_df <- hazelwood_CLC06_Area511from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC06_Area511from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2006 = round(sum(Area_ha),2))%>% mutate_at('Area_2006', as.numeric)
# 2012
hazelwood_CLC12_Area511from06_df <- hazelwood_CLC12_Area511from06 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC12_Area511from06$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2012 = round(sum(Area_ha),2))%>% mutate_at('Area_2012', as.numeric)
# 2018
hazelwood_CLC18_Area511from12_df <- hazelwood_CLC18_Area511from12 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area511from12$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)
# 2000-2018
hazelwood_CLC18_Area511from00_df <- hazelwood_CLC18_Area511from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area511from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)

hazelwood_CLC_Code511_Table_1 <- full_join(hazelwood_CLC00_Code511_df, full_join(hazelwood_CLC06_Area511from00_df, full_join(hazelwood_CLC12_Area511from06_df, 
                                                                                                                 hazelwood_CLC18_Area511from12_df)), by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_06_ha = round(Area_2006 - Area_2000, 2),
         Change_00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Change_06_12_ha = round(Area_2012 - totArea2006_code511, 2),
         Change_06_12_Perc = round((Area_2012 - totArea2006_code511)/totArea2006_code511*100, 2),
         Change_12_18_ha = round(Area_2018 - totArea2012_code511, 2),
         Change_12_18_Perc = round((Area_2018 - totArea2012_code511)/totArea2012_code511*100, 2)) %>% as.data.frame()

hazelwood_CLC_Code511_Table_2 <- full_join(hazelwood_CLC00_Code511_df, hazelwood_CLC18_Area511from00_df, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  select(1,4,5) %>% as.data.frame() 

hazelwood_CLC_Code511_Table <- left_join(hazelwood_CLC_Code511_Table_1, hazelwood_CLC_Code511_Table_2, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% as.data.frame() 


# Water bodies (512) REDUCTIONS
hazelwood_CLC00_Code512 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "512") %>% select(6)
totArea2000_code512 <- sum(st_area(hazelwood_CLC00_Code512$geometry)/10000) %>% as.numeric()# 5.45909
hazelwood_CLC06_Area512from00 <- st_intersection(hazelwood_CLC06, hazelwood_CLC00_Code512)
hazelwood_CLC06_Code512 <- subset(hazelwood_CLC06, hazelwood_CLC06$CODE_06 == "512") %>% select(6)
totArea2006_code512 <- sum(st_area(hazelwood_CLC06_Code512$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC12_Area512from06 <- st_intersection(hazelwood_CLC12, hazelwood_CLC06_Code512)
hazelwood_CLC12_Code512 <- subset(hazelwood_CLC12, hazelwood_CLC12$CODE_12 == "512") %>% select(6)
totArea2012_code512 <- sum(st_area(hazelwood_CLC12_Code512$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC18_Area512from12 <- st_intersection(hazelwood_CLC18, hazelwood_CLC12_Code512)
hazelwood_CLC18_Area512from00 <- st_intersection(hazelwood_CLC18, hazelwood_CLC00_Code512)

# 2000
hazelwood_CLC00_Code512_df <- hazelwood_CLC00_Code512 %>% as.data.frame() %>% mutate(Area_ha = st_area(hazelwood_CLC00_Code512$geometry)/10000) %>%
  inner_join(CLCall_Codes_df[,c(1,5)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2)) %>% mutate_at('Area_2000', as.numeric)
# 2006
hazelwood_CLC06_Area512from00_df <- hazelwood_CLC06_Area512from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC06_Area512from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2006 = round(sum(Area_ha),2))%>% mutate_at('Area_2006', as.numeric)
# 2012
hazelwood_CLC12_Area512from06_df <- hazelwood_CLC12_Area512from06 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC12_Area512from06$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2012 = round(sum(Area_ha),2))%>% mutate_at('Area_2012', as.numeric)
# 2018
hazelwood_CLC18_Area512from12_df <- hazelwood_CLC18_Area512from12 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area512from12$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)
# 2000-2018
hazelwood_CLC18_Area512from00_df <- hazelwood_CLC18_Area512from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area512from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)

hazelwood_CLC_Code512_Table_1 <- full_join(hazelwood_CLC00_Code512_df, full_join(hazelwood_CLC06_Area512from00_df, full_join(hazelwood_CLC12_Area512from06_df, 
                                                                                                                             hazelwood_CLC18_Area512from12_df)), by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_06_ha = round(Area_2006 - Area_2000, 2),
         Change_00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Change_06_12_ha = round(Area_2012 - totArea2006_code512, 2),
         Change_06_12_Perc = round((Area_2012 - totArea2006_code512)/totArea2006_code512*100, 2),
         Change_12_18_ha = round(Area_2018 - totArea2012_code512, 2),
         Change_12_18_Perc = round((Area_2018 - totArea2012_code512)/totArea2012_code512*100, 2)) %>% as.data.frame()

hazelwood_CLC_Code512_Table_2 <- full_join(hazelwood_CLC00_Code512_df, hazelwood_CLC18_Area512from00_df, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  select(1,4,5) %>% as.data.frame() 

hazelwood_CLC_Code512_Table <- left_join(hazelwood_CLC_Code512_Table_1, hazelwood_CLC_Code512_Table_2, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% as.data.frame() 


# Broad-leaved forest (311) REDUCTIONS
hazelwood_CLC00_Code311 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "311") %>% select(6)
totArea2000_code311 <- sum(st_area(hazelwood_CLC00_Code311$geometry)/10000) %>% as.numeric()# 5.45909
hazelwood_CLC06_Area311from00 <- st_intersection(hazelwood_CLC06, hazelwood_CLC00_Code311)
hazelwood_CLC06_Code311 <- subset(hazelwood_CLC06, hazelwood_CLC06$CODE_06 == "311") %>% select(6)
totArea2006_code311 <- sum(st_area(hazelwood_CLC06_Code311$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC12_Area311from06 <- st_intersection(hazelwood_CLC12, hazelwood_CLC06_Code311)
hazelwood_CLC12_Code311 <- subset(hazelwood_CLC12, hazelwood_CLC12$CODE_12 == "311") %>% select(6)
totArea2012_code311 <- sum(st_area(hazelwood_CLC12_Code311$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC18_Area311from12 <- st_intersection(hazelwood_CLC18, hazelwood_CLC12_Code311)
hazelwood_CLC18_Area311from00 <- st_intersection(hazelwood_CLC18, hazelwood_CLC00_Code311)

# 2000
hazelwood_CLC00_Code311_df <- hazelwood_CLC00_Code311 %>% as.data.frame() %>% mutate(Area_ha = st_area(hazelwood_CLC00_Code311$geometry)/10000) %>%
  inner_join(CLCall_Codes_df[,c(1,5)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2)) %>% mutate_at('Area_2000', as.numeric)
# 2006
hazelwood_CLC06_Area311from00_df <- hazelwood_CLC06_Area311from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC06_Area311from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2006 = round(sum(Area_ha),2))%>% mutate_at('Area_2006', as.numeric)
# 2012
hazelwood_CLC12_Area311from06_df <- hazelwood_CLC12_Area311from06 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC12_Area311from06$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2012 = round(sum(Area_ha),2))%>% mutate_at('Area_2012', as.numeric)
# 2018
hazelwood_CLC18_Area311from12_df <- hazelwood_CLC18_Area311from12 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area311from12$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)
# 2000-2018
hazelwood_CLC18_Area311from00_df <- hazelwood_CLC18_Area311from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area311from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)

hazelwood_CLC_Code311_Table_1 <- full_join(hazelwood_CLC00_Code311_df, full_join(hazelwood_CLC06_Area311from00_df, full_join(hazelwood_CLC12_Area311from06_df, 
                                                                                                                             hazelwood_CLC18_Area311from12_df)), by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_06_ha = round(Area_2006 - Area_2000, 2),
         Change_00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Change_06_12_ha = round(Area_2012 - totArea2006_code311, 2),
         Change_06_12_Perc = round((Area_2012 - totArea2006_code311)/totArea2006_code311*100, 2),
         Change_12_18_ha = round(Area_2018 - totArea2012_code311, 2),
         Change_12_18_Perc = round((Area_2018 - totArea2012_code311)/totArea2012_code311*100, 2)) %>% as.data.frame()

hazelwood_CLC_Code311_Table_2 <- full_join(hazelwood_CLC00_Code311_df, hazelwood_CLC18_Area311from00_df, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  select(1,4,5) %>% as.data.frame() 

hazelwood_CLC_Code311_Table <- left_join(hazelwood_CLC_Code311_Table_1, hazelwood_CLC_Code311_Table_2, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% as.data.frame() 



# Mixed forest (313)
hazelwood_CLC00_Code313 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "313") %>% select(6)
totArea2000_code313 <- sum(st_area(hazelwood_CLC00_Code313$geometry)/10000) %>% as.numeric()# 5.45909
hazelwood_CLC06_Area313from00 <- st_intersection(hazelwood_CLC06, hazelwood_CLC00_Code313)
hazelwood_CLC06_Code313 <- subset(hazelwood_CLC06, hazelwood_CLC06$CODE_06 == "313") %>% select(6)
totArea2006_code313 <- sum(st_area(hazelwood_CLC06_Code313$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC12_Area313from06 <- st_intersection(hazelwood_CLC12, hazelwood_CLC06_Code313)
hazelwood_CLC12_Code313 <- subset(hazelwood_CLC12, hazelwood_CLC12$CODE_12 == "313") %>% select(6)
totArea2012_code313 <- sum(st_area(hazelwood_CLC12_Code313$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC18_Area313from12 <- st_intersection(hazelwood_CLC18, hazelwood_CLC12_Code313)
hazelwood_CLC18_Area313from00 <- st_intersection(hazelwood_CLC18, hazelwood_CLC00_Code313)

# 2000
hazelwood_CLC00_Code313_df <- hazelwood_CLC00_Code313 %>% as.data.frame() %>% mutate(Area_ha = st_area(hazelwood_CLC00_Code313$geometry)/10000) %>%
  inner_join(CLCall_Codes_df[,c(1,5)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2)) %>% mutate_at('Area_2000', as.numeric)
# 2006
hazelwood_CLC06_Area313from00_df <- hazelwood_CLC06_Area313from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC06_Area313from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2006 = round(sum(Area_ha),2))%>% mutate_at('Area_2006', as.numeric)
# 2012
hazelwood_CLC12_Area313from06_df <- hazelwood_CLC12_Area313from06 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC12_Area313from06$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2012 = round(sum(Area_ha),2))%>% mutate_at('Area_2012', as.numeric)
# 2018
hazelwood_CLC18_Area313from12_df <- hazelwood_CLC18_Area313from12 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area313from12$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)
# 2000-2018
hazelwood_CLC18_Area313from00_df <- hazelwood_CLC18_Area313from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area313from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)

hazelwood_CLC_Code313_Table_1 <- full_join(hazelwood_CLC00_Code313_df, full_join(hazelwood_CLC06_Area313from00_df, full_join(hazelwood_CLC12_Area313from06_df, 
                                                                                                                             hazelwood_CLC18_Area313from12_df)), by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_06_ha = round(Area_2006 - Area_2000, 2),
         Change_00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Change_06_12_ha = round(Area_2012 - totArea2006_code313, 2),
         Change_06_12_Perc = round((Area_2012 - totArea2006_code313)/totArea2006_code313*100, 2),
         Change_12_18_ha = round(Area_2018 - totArea2012_code313, 2),
         Change_12_18_Perc = round((Area_2018 - totArea2012_code313)/totArea2012_code313*100, 2)) %>% as.data.frame()

hazelwood_CLC_Code313_Table_2 <- full_join(hazelwood_CLC00_Code313_df, hazelwood_CLC18_Area313from00_df, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  select(1,4,5) %>% as.data.frame() 

hazelwood_CLC_Code313_Table <- left_join(hazelwood_CLC_Code313_Table_1, hazelwood_CLC_Code313_Table_2, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% as.data.frame() 

# Pastures (231) REDUCTIONS
hazelwood_CLC00_Code231 <- subset(hazelwood_CLC00, hazelwood_CLC00$CODE_00 == "231") %>% select(6)
totArea2000_code231 <- sum(st_area(hazelwood_CLC00_Code231$geometry)/10000) %>% as.numeric()# 5.45909
hazelwood_CLC06_Area231from00 <- st_intersection(hazelwood_CLC06, hazelwood_CLC00_Code231)
hazelwood_CLC06_Code231 <- subset(hazelwood_CLC06, hazelwood_CLC06$CODE_06 == "231") %>% select(6)
totArea2006_code231 <- sum(st_area(hazelwood_CLC06_Code231$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC12_Area231from06 <- st_intersection(hazelwood_CLC12, hazelwood_CLC06_Code231)
hazelwood_CLC12_Code231 <- subset(hazelwood_CLC12, hazelwood_CLC12$CODE_12 == "231") %>% select(6)
totArea2012_code231 <- sum(st_area(hazelwood_CLC12_Code231$geometry)/10000) %>% as.numeric() # 0
hazelwood_CLC18_Area231from12 <- st_intersection(hazelwood_CLC18, hazelwood_CLC12_Code231)
hazelwood_CLC18_Area231from00 <- st_intersection(hazelwood_CLC18, hazelwood_CLC00_Code231)

# 2000
hazelwood_CLC00_Code231_df <- hazelwood_CLC00_Code231 %>% as.data.frame() %>% mutate(Area_ha = st_area(hazelwood_CLC00_Code231$geometry)/10000) %>%
  inner_join(CLCall_Codes_df[,c(1,5)], by="CODE_00") %>% # Rename to have matching class 
  group_by(Class_Desc) %>% summarize(Area_2000 = round(sum(Area_ha),2)) %>% mutate_at('Area_2000', as.numeric)
# 2006
hazelwood_CLC06_Area231from00_df <- hazelwood_CLC06_Area231from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC06_Area231from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2006 = round(sum(Area_ha),2))%>% mutate_at('Area_2006', as.numeric)
# 2012
hazelwood_CLC12_Area231from06_df <- hazelwood_CLC12_Area231from06 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC12_Area231from06$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2012 = round(sum(Area_ha),2))%>% mutate_at('Area_2012', as.numeric)
# 2018
hazelwood_CLC18_Area231from12_df <- hazelwood_CLC18_Area231from12 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area231from12$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)
# 2000-2018
hazelwood_CLC18_Area231from00_df <- hazelwood_CLC18_Area231from00 %>% as.data.frame() %>% 
  mutate(Area_ha = st_area(hazelwood_CLC18_Area231from00$geometry)/10000) %>% group_by(Class_Desc) %>%
  summarize(Area_2018 = round(sum(Area_ha),2))%>% mutate_at('Area_2018', as.numeric)

hazelwood_CLC_Code231_Table_1 <- full_join(hazelwood_CLC00_Code231_df, full_join(hazelwood_CLC06_Area231from00_df, full_join(hazelwood_CLC12_Area231from06_df, 
                                                                                                                             hazelwood_CLC18_Area231from12_df)), by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_06_ha = round(Area_2006 - Area_2000, 2),
         Change_00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Change_06_12_ha = round(Area_2012 - totArea2006_code231, 2),
         Change_06_12_Perc = round((Area_2012 - totArea2006_code231)/totArea2006_code231*100, 2),
         Change_12_18_ha = round(Area_2018 - totArea2012_code231, 2),
         Change_12_18_Perc = round((Area_2018 - totArea2012_code231)/totArea2012_code231*100, 2)) %>% as.data.frame()

hazelwood_CLC_Code231_Table_2 <- full_join(hazelwood_CLC00_Code231_df, hazelwood_CLC18_Area231from00_df, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% 
  mutate(Change_00_18_ha = round(Area_2018 - Area_2000, 2),
         Change_00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2)) %>% 
  select(1,4,5) %>% as.data.frame() 

hazelwood_CLC_Code231_Table <- left_join(hazelwood_CLC_Code231_Table_1, hazelwood_CLC_Code231_Table_2, by = "Class_Desc") %>%
  replace(is.na(.), 0) %>% as.data.frame() 


## Combine all reductions, calculate additions and net change by land use type
hazelwood_CLC_ExtentAccout <- rbind(hazelwood_CLC_Code511_Table, hazelwood_CLC_Code512_Table, hazelwood_CLC_Code311_Table, hazelwood_CLC_Code313_Table,
                                    hazelwood_CLC_Code231_Table) %>%
  group_by(Class_Desc) %>% 
  filter(Area_2000 > 0) %>% 
  mutate(across(c(Change_00_06_ha:Change_00_18_Perc), ~ ifelse(.x > 0, 0, .x))) %>%
  group_by(Class_Desc) %>% 
  summarize(Reductions00_06 = sum(Change_00_06_ha)*-1,
            Reductions06_12 = sum(Change_06_12_ha)*-1,
            Reductions12_18 = sum(Change_12_18_ha)*-1,
            Reductions00_18 = sum(Change_00_18_ha)*-1) %>%
  full_join(hazelwood_CLC_Table[,c(1:5)], by="Class_Desc") %>%
  mutate(NetChange00_06 = round(Area_2006 - Area_2000,2),
         Additions00_06 = round(NetChange00_06 + Reductions00_06,2),
         NetChange06_12 = round(Area_2012 - Area_2006,2),
         Additions06_12 = round(NetChange06_12 + Reductions06_12,2),
         NetChange12_18 = round(Area_2018 - Area_2012,2),
         Additions12_18 = round(NetChange12_18 + Reductions12_18,2),
         NetChange00_18 = round(Area_2018 - Area_2000,2),
         Additions00_18 = round(NetChange00_18 + Reductions00_18,2)) %>% 
  mutate(NetChange00_06_Perc = round((Area_2006 - Area_2000)/Area_2000*100, 2),
         Additions00_06_Perc = round(Additions00_06/Area_2000*100,2),
         Reductions00_06_Perc = round(Reductions00_06/Area_2000*100,2),
         NetChange06_12_Perc = round((Area_2012 - Area_2006)/Area_2006*100, 2),
         Additions06_12_Perc = round(Additions06_12/Area_2006*100,2),
         Reductions06_12_Perc = round(Reductions06_12/Area_2006*100,2),
         NetChange12_18_Perc = round((Area_2018 - Area_2012)/Area_2012*100, 2),
         Additions12_18_Perc = round(Additions12_18/Area_2012*100,2),
         Reductions12_18_Perc = round(Reductions12_18/Area_2012*100,2),
         NetChange00_18_Perc = round((Area_2018 - Area_2000)/Area_2000*100, 2),
         Additions00_18_Perc = round(Additions00_18/Area_2000*100,2),
         Reductions00_18_Perc = round(Reductions00_18/Area_2000*100,2)) %>% 
  mutate_at(c('Reductions00_06','Reductions06_12','Reductions12_18','Reductions00_18', 'Area_2000', 'Area_2006', 'Area_2012','Area_2018', 
              'NetChange00_06', 'Additions00_06', 'NetChange06_12', 'Additions06_12', 
              'NetChange12_18', 'Additions12_18', 'NetChange00_18', 'Additions00_18',
              'NetChange00_06_Perc', 'Additions00_06_Perc', 'Reductions00_06_Perc',
              'NetChange06_12_Perc', 'Additions06_12_Perc', 'Reductions06_12_Perc',
              'NetChange12_18_Perc', 'Additions12_18_Perc', 'Reductions12_18_Perc',
              'NetChange00_18_Perc', 'Additions00_18_Perc', 'Reductions00_18_Perc'), as.numeric) %>%
  relocate(Class_Desc, Area_2000, Area_2006, Area_2012, Area_2018, Reductions00_06, Reductions06_12, Reductions12_18, Reductions00_18, 
           Additions00_06, Additions06_12, Additions12_18, Additions00_18, NetChange00_06, NetChange06_12, NetChange12_18, NetChange00_18, 
           Reductions00_06_Perc, Reductions06_12_Perc, Reductions12_18_Perc, Reductions00_18_Perc,
           Additions00_06_Perc, Additions06_12_Perc, Additions12_18_Perc, Additions00_18_Perc, 
           NetChange00_06_Perc, NetChange06_12_Perc, NetChange12_18_Perc, NetChange00_18_Perc) %>% as.data.frame()





## Plot map and  bar plots ------------

# Set colors (for all CLC)
CLC_colors <- c("Mixed forest" = "#336600",
                "Coniferous forest"="#ea6218",
                "Broad-leaved forest" = "#00CC33",
                "Peat bogs" = "#1360ed",
                "Water bodies" = "#00CCFF",
                "Water courses" = "#0099FF",
                "Transitional woodland-shrub" = "#00FF99",
                "Burnt areas" = "#666000",
                "Pastures" = "#e8fa5bff",
                "Bare rocks" = "#CCCCCC",
                "Industrial or commercial units" = "#111555",
                "Sparsely vegetated areas" = "#009966",
                "Inland marshes" = "#66CCCC",
                "Land principally occupied by agriculture" = "#FF9933",
                "Moors and heathland" = "#FF3399",
                "Natural grasslands" = "#FFFF00",
                "Complex cultivation patterns" = "#FFFFCC",
                "Discontinuous urban fabric" = "#000000",
                "Sport and leisure facilities" = "#663300",
                "Mineral extraction sites" = "#CC0033",
                "Dump sites" = "#333333",
                "Road and rail networks and associated land" = "#CCCFFF",
                "Airports" = "#FF0000",
                "Sea and ocean" = "#000066",
                "Estuaries" = "#00CCCC",
                "Intertidal flats" = "#0099FF",
                "Non-irrigated arable land" = "#CCFFCC",
                "Fruit trees and berry plantations" = "#00CC99",
                "Beaches, dunes, sands" = "#CCCC00",
                "Salt marshes" = "#9999CC")



x11()
# MAP hazelwood CLC 2000-2006-2012-2018
# MAP hazelwood CLC 2000
HAZEL_CLC00_plot <- ggplot() +
  geom_sf(data = hazelwood_CLC00, aes(fill=Class_Desc), color=NA) +
  labs(title = 'Hazelwood - 2000') + 
  theme_bw() + scale_fill_manual(values=CLC_colors) + 
  theme(legend.position = "none") +
  coord_sf(crs = "EPSG:4326")+
  # library(ggspatial)
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    pad_x = unit(6, "cm")
  )  +
  #--- add north arrow ---#
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(8, "cm"),
    pad_y = unit(0.6, "cm"),
    style = north_arrow_fancy_orienteering
  )

# MAP hazelwood CLC 2006
HAZEL_CLC06_plot <- ggplot() +
  geom_sf(data = hazelwood_CLC06, aes(fill=Class_Desc), color=NA) +
  labs(title = 'Hazelwood - 2006') + 
  theme_bw() + scale_fill_manual(values=CLC_colors) + 
  theme(legend.position = "none") +
  coord_sf(crs = "EPSG:4326")

# MAP hazelwood CLC 2012
HAZEL_CLC12_plot <- ggplot() +
  geom_sf(data = hazelwood_CLC12, aes(fill=Class_Desc), color=NA) +
  labs(title = 'Hazelwood - 2012') + 
  theme_bw() + scale_fill_manual(values=CLC_colors) + 
  theme(legend.position = "none") +
  coord_sf(crs = "EPSG:4326")

# MAP hazelwood CLC 2018
HAZEL_CLC18_plot <- ggplot() +
  geom_sf(data = hazelwood_CLC18, aes(fill=Class_Desc), color=NA) +
  labs(title = 'Hazelwood - 2018') + 
  theme_bw() + scale_fill_manual(values=CLC_colors) + 
  theme(legend.position = "none") +
  coord_sf(crs = "EPSG:4326")


mapHAZEL_legend <- get_legend(
  CLC18 %>% filter(CODE_18 == 231 | CODE_18 == 311 | CODE_18 == 313 | CODE_18 == 511 | CODE_18 == 512) %>% 
    # use CLC18 dataset to select w3hat I need for the legend
    ggplot() +
    geom_sf(aes(fill=Class_Desc), color=NA) +
    scale_fill_manual(values = CLC_colors,
                      labels=c("Broad-leaved forest",
                               "Mixed forest",
                               "Pastures",
                               "Water bodies",
                               "Water courses")) +
    coord_sf(crs = "EPSG:4326") + labs(fill='Corine Land Cover') +
    theme(legend.position = "right")
)

CLC_Plot <- plot_grid(HAZEL_CLC00_plot, HAZEL_CLC06_plot,HAZEL_CLC12_plot, HAZEL_CLC18_plot, ncol=2)
plot_grid(CLC_Plot,  mapHAZEL_legend, rel_widths = c(1,0.1), rel_heights = c(1,0.2))




# Plot histograms of land use Area for CLC in 2000-2006-2012-2018
reshape2::melt(hazelwood_CLC_Table[,c(1:5)], "Class_Desc") %>% 
  mutate(Class_Desc = case_when(Class_Desc == "Land principally occupied by agriculture, with significant areas of natural vegetation" ~ 
                                  "Land principally occupied by agriculture",
                                TRUE ~ as.character(Class_Desc)),
         variable = case_when(variable == "Area_2000" ~ "CORINE 2000",
                              variable == "Area_2006" ~ "CORINE 2006",
                              variable == "Area_2012" ~ "CORINE 2012",
                              variable == "Area_2018" ~ "CORINE 2018")) %>% 
  ggplot(aes(Class_Desc, value, fill=Class_Desc)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=CLC_colors) +
  theme_bw(base_size = 13) + labs(x="", y="Area (ha)", title = 'Business Area Unit 1') +
  geom_text(aes(x=Class_Desc,y=as.numeric(value+5),fontface = "bold",label=format(value, digits = 3)),
            position = position_dodge(width = .75),size = 2) +
  coord_flip() + theme(legend.position = "none") +
  facet_wrap(~variable, ncol=4)


# Plot histograms of land use change from 2000 to 2018, for each interval, in Hazelwood
reshape2::melt(hazelwood_CLC_Table[,c(1,6,8,10,12)], "Class_Desc") %>% 
  mutate(Class_Desc = case_when(Class_Desc == "Land principally occupied by agriculture, with significant areas of natural vegetation" ~ 
                                  "Land principally occupied by agriculture",
                                TRUE ~ as.character(Class_Desc)),
         variable = case_when(variable == "Change_00_06_ha" ~ "CORINE change 2000-2006",
                              variable == "Change_06_12_ha" ~ "CORINE change 2006-2012",
                              variable == "Change_12_18_ha" ~ "CORINE change 2012-2018",
                              variable == "Change_00_18_ha" ~ "CORINE change 2000-2018")) %>%
  ggplot(aes(Class_Desc, value, fill=Class_Desc)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=CLC_colors)+
  theme_bw() + labs(x="", y="Area Change (ha)", title = 'Business Area Unit 1') +
  theme(legend.position = "none") + coord_flip() +
  guides(fill = guide_legend(title = ""))+
  facet_grid(~factor(variable, levels=c("CORINE change 2000-2006","CORINE change 2006-2012",
                                        "CORINE change 2012-2018","CORINE change 2000-2018")))


