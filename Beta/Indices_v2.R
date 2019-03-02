#-------------------------------------------------------------------------------

# For CWPT 2.0

# Calculating upslope accumulation, downslope retention, and on-pixel 
# contribution indices for each QS in Alberta, using the new and improved (!)
# 15m DEM-derived data. Indices will be calculated by:

# 1. County/MD/Special Area
# 2. HUC 2/8 watershed
# 3. LUF planning region

# In the future, indices can be calculated at any spatial scale. For e.g., the
# catchment scale for an individual lake (as produced by Jerome). 

#-------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(sf)
library(Hmisc)
library(rmapshaper)
library(classInt)

#-------------------------------------------------------------------------------

# Import data

ab_cwpt2_qs <- 
  st_read("./SpatialData/webapp_v1/Raw/cwpt2_qs_AB_final.shp", 
                       stringsAsFactors = FALSE, quiet = TRUE)

# Use rmapshaper::ms_simplify to improve performance; sys = TRUE requires having
# the system mapshaper library installed and on PATH.
# ab_cwpt2_qs <- ms_simplify(ab_cwpt2_qs, sys = TRUE)

# Counties
ab_counties <- st_read("./SpatialData/webapp_v1/Raw/AB_Counties.shp",
                       stringsAsFactors = FALSE, quiet = TRUE)

ab_counties <- ms_simplify(ab_counties, sys = TRUE)

# Hydologic Unit Code (HUC) Watersheds

ab_huc8 <- st_read("./SpatialData/webapp_v1/Raw/")



# Set projection

ab_cwpt2_qs <- st_transform(ab_cwpt2_qs, "+init=epsg:4326")
ab_counties <- st_transform(ab_counties, "+init=epsg:4326")


# Clean up qs polygon attribute info and calculate indices

ab_cwpt2_qs_1 <- ab_cwpt2_qs %>%
  # Create one variable for County/MD/SA name (henceforth 'municipality')
  mutate(Rural_MD_County = ifelse(!is.na(SPMUN_NAME), SPMUN_NAME, MD_NAME)) %>% 
  mutate(County_MD_SA = ifelse(!is.na(SPAREA_NAM), 
                               SPAREA_NAM, Rural_MD_County))
  
  # Calculate % of white zone in each municipality
  
  county_w <- ab_cwpt2_qs_1 %>%
    st_set_geometry(NULL) %>%
    filter(!is.na(County_MD_SA)) %>%
    group_by(County_MD_SA, GWA_NAME) %>%
    count() %>%
    filter(!is.na(GWA_NAME)) %>%
    spread(GWA_NAME, n, fill = 0) %>%
    rename(Green = 'Green Area', White = 'White Area') %>%
    mutate(total_qs = Green + White,
           prop_w = White / total_qs) %>%
    filter(prop_w >= 0.15) %>%
    select(County_MD_SA)
  
ab_cwpt2_qs_2 <- ab_cwpt2_qs_1 %>%
  # Remove municipalities that are primarily Green Area
  filter(County_MD_SA %in% county_w$County_MD_SA) %>%
  select(LinkID, up_15__min:down_15__2, qs_p_load, GWA_NAME, HUC_2:LUF_CODE,
         County_MD_SA) %>%
  # Create LLD variable
  mutate(Meridian = "W") %>%
  mutate(LLD = paste0(Meridian, LinkID)) %>%
  separate(LLD, c("Meridian", "Range", "Township", "Section", "Quarter"),
           sep = "-") %>%
  unite(LLD, c("Quarter", "Section", "Township", "Range", "Meridian"),
        sep = "-", remove = FALSE)

# Create df of lld & geometries (stashed away for later)
ab_lld_geo <- ab_cwpt2_qs_2 %>%
  select(LLD)

ab_cwpt2_qs_3 <- ab_cwpt2_qs_2 %>%
  # Remove geometry to improve speed
  st_set_geometry(NULL) %>%
  # Calculate percentiles in order to remove outliers later
  group_by(County_MD_SA) %>%
  # County is '_1'
  mutate(pt_upmean_1 = percent_rank(up_15__mea),
         pt_upmax_1 = percent_rank(up_15__max),
         pt_dmean_1 = percent_rank(down_15__2),
         pt_pix_1 = percent_rank(qs_p_load)) %>%
  ungroup() %>%
  group_by(HUC_2) %>%
  # HUC 2 watersheds are '_2'
  mutate(pt_upmean_2 = percent_rank(up_15__mea),
         pt_upmax_2 = percent_rank(up_15__max),
         pt_dmean_2 = percent_rank(down_15__2),
         pt_pix_2 = percent_rank(qs_p_load)) %>%
  ungroup() %>%
  group_by(HUC_8) %>%
  # HUC 8 watersheds are '_3'
  mutate(pt_upmean_3 = percent_rank(up_15__mea),
         pt_upmax_3 = percent_rank(up_15__max),
         pt_dmean_3 = percent_rank(down_15__2),
         pt_pix_3 = percent_rank(qs_p_load)) %>%
  ungroup() %>%
  group_by(LUF_CODE) %>%
  # LUF regions are '_4'
  mutate(pt_upmean_4 = percent_rank(up_15__mea),
         pt_upmax_4 = percent_rank(up_15__max),
         pt_dmean_4 = percent_rank(down_15__2),
         pt_pix_4 = percent_rank(qs_p_load)) %>%
  ungroup()

upmean_1 <- ab_cwpt2_qs_3 %>%
  filter(pt_upmean_1 <= 0.99)
upmean_1 <- classIntervals(upmean_1$up_15__mea, style = "fisher", n = 10)



  
  
  
  

lsac <- ab_cwpt2_qs_2 %>%
  filter(County_MD_SA == "Lac Ste. Anne County")

plot <- ab_cwpt2_qs_3 %>%
  ggplot(aes(x = down_15__2)) +
  geom_histogram(bins = 100) 

plot
  
test <- ab_cwpt2_qs_3 %>% 
  filter(County_MD_SA == "Lac Ste. Anne County") %>%
  filter(pt_upmean_1 >= 0.99)


test 

try <- ab_cwpt2_qs_indices %>%
  # filter(County_MD_SA == "Lac Ste. Anne County") %>%
  st_set_geometry(NULL) %>%
  # filter(down_15__2 < 250000)

test <- classIntervals(lsac$down_15__2, style = "jenks", n = 10)
lsac$jenks <- cut(lsac$down_15__2, breaks = test$brks,
                  labels = as.character(1:10))

hello <- lsac %>%
  group_by(jenks) %>%
  count() %>%
  View()

hmm <- lsac %>%
  filter(is.na(jenks))

plot <- test %>%
  # filter(up_15__mea < 500) %>%
  ggplot(aes(x = pct)) +
  geom_histogram(bins = 100) 

plot

plot <- ab_cwpt2_qs_2 %>%
  st_set_geometry(NULL) %>%
  top_n(-(row_number() * 0.99), up_15__mea) %>%
  # filter(up_15__mea < 750) %>%
  ggplot(aes(x = up_15__mea)) +
  geom_histogram(bins = 100) 

plot

nrow(ab_cwpt2_qs_2) 

percent_rank()




ab_counties_simp <- ab_counties %>%
  # Remove excess columns
  select(OBJECTID_1, SPMUN_NAME, MD_NAME, SPAREA_NAM) %>%
  # Remove fragment (ghost) polygons
  filter(OBJECTID_1 <= 74) %>%
  # Create one variable for County/MD/SA name
  mutate(Rural_MD_County = ifelse(!is.na(SPMUN_NAME), SPMUN_NAME, MD_NAME)) %>% 
  mutate(County_MD_SA = ifelse(!is.na(SPAREA_NAM), 
                               SPAREA_NAM, Rural_MD_County)) %>%
  select(County_MD_SA)

ab_gw_simp <- ab_gw %>%
  select(GWA_NAME)

# Spatial Join (intersect)
#   - Want county information appended to each quarter-section.

# 0-buffer trick

ab_cwpt2_qs_buff = st_buffer(ab_cwpt2_qs, dist = 0)

ab_cwpt2_qs_gw <- ab_cwpt2_qs_buff %>%
  st_join(ab_gw_simp, join = st_intersects, largest = TRUE)
  
  st_join(ab_counties_simp, join = st_intersects, largest = TRUE) %>%
  filter(!is.na(County_MD_SA)) %>%
  st_join(ab_gw, join = st_intersects, largest = TRUE)
  
  


#   - append other spatial features as well (e.g. HUC, LUF)

# Want to start thinking about how to cut down on the number of features (qs's)
# embedded within this app. Alberta is a big freaking province. Maybe start by:
#   a) getting rid of qs's that are in the national parks (duh)
#   b) eliminating the green zone (really, this tool only applies to Ag)




















