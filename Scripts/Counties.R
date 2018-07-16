#-------------------------------------------------------------------

# Creating county layer for filtering CWPT quarter-section data.

# Load packages
library(tidyverse)
library(sf)
library(tmaptools)

# Import data
ab_counties <- read_shape("./SpatialData/AB_Counties.shp", as.sf = TRUE, stringsAsFactors = FALSE)

# Set projection
ab_counties <- st_transform(ab_counties, "+init=epsg:4326")

# Cleanup
ab_counties <- ab_counties %>%
  mutate(Rural_MD_County = ifelse(!is.na(SPMUN_NAME), SPMUN_NAME, MD_NAME)) %>% # Create county name variable 
  mutate(County_MD_SA = ifelse(!is.na(SPAREA_NAM), SPAREA_NAM, Rural_MD_County)) %>%
  select(OBJECTID_1, County_MD_SA, SPMUN_CODE, MD_CODE, SPAREA_COD, Shape_Leng, Shape_Area)

# Save sf object as shapefile in the 'Clean' folder of SpatialData
st_write(obj = ab_counties, "./SpatialData/Clean/ab_counties.shp")
