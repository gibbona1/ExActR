library(sf)
library(terra)
library(dplyr)

sf_folder <- "inst/extent_app/ext_data/Dargle"
f1 <- "dargle_CLC2012.shp"
f2 <- "dargle_CLC2018.shp"

sf::sf_use_s2(FALSE)

#read data
sf1 <- st_read(file.path(sf_folder, f1)) %>% st_make_valid()
sf2 <- st_read(file.path(sf_folder, f2)) %>% st_make_valid()

terra1 <- vect(sf1)
terra2 <- vect(sf2)

df_int <- terra::intersect(terra1, terra2) %>% st_as_sf()

df_int %>%
  arrange(CODE_18) %>%
  select(geometry, CODE_18) %>%
  plot()

df_int2 <- st_intersection(df1, df2)

df_int2 %>%
  arrange(CODE_18) %>%
  select(geometry, CODE_18) %>%
  plot()
