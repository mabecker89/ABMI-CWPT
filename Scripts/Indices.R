#--------------------------------------------------------------------------------------

# Calculating indices for each QS in Alberta, by county/MD/special area. 

# Load packages
library(tidyverse)
library(sf)
library(tmaptools)
library(Hmisc)

# Import data
ab_cwpt_all <- read_shape("./SpatialData/Alberta_AllCounties/AB_CWPT_LakeTownCity_Erase.shp", 
                        as.sf = TRUE, stringsAsFactors = FALSE)

# Set projection
ab_cwpt_all <- st_transform(ab_cwpt_all, "+init=epsg:4326")

# Prepare data
ab_cwpt_indices <- ab_cwpt_all %>%
  mutate(Meridian = "W") %>%
  mutate(LLD = paste0(Meridian, LinkID)) %>% # Create LLD variable
  select(-c(Meridian, LinkID)) %>%
  mutate(Rural_MD_County = ifelse(!is.na(SPMUN_NAME), SPMUN_NAME, MD_NAME)) %>% # Create county name variable 
  mutate(County_MD_SA = ifelse(!is.na(SPAREA_NAM), SPAREA_NAM, Rural_MD_County)) %>%
  filter(!is.na(County_MD_SA)) %>%
  group_by(County_MD_SA) %>%
  mutate(up_ac_bin = as.numeric(cut2(upslope_ac, g = 10)), # Calculate indices
         self_bin = as.numeric(cut2(p_load, g = 10)),
         down_ret_bin = 11L - as.numeric(cut2(downslope_, g = 10))) %>%
  ungroup() %>%
  mutate(land_imp = (up_ac_bin + down_ret_bin) / 2) %>%
  select(OBJECTID, LLD, AREA, County_MD_SA, up_ac_bin, down_ret_bin, land_imp, self_bin, geometry)

# Write sf object to shapefile in 'Clean' folder of SpatialData
st_write(obj = ab_cwpt_indices, "./SpatialData/Clean/ab_cwpt_indices.shp")

# Write csv
st_geometry(ab_cwpt_indices) <- NULL
write_csv(cwpt_indices, "./IndexData/QS_IndexValues.csv")









