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
  # Filter out municipalities that contain greater than 85% green zone.
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

#-------------------------------------------------------------------------------

# Ugh, tedious process here. Oh well. We'll figure it out down the road when you
# are an R ninja ;) See JenksCalc.R for the calculation process of jenks at each
# level. 

# Read in csv files generated - this is Municipality level. 
all_umax <- read_csv("./Beta/StagingData/all_umax_j1.csv")
all_umea <- read_csv("./Beta/StagingData/all_umea_j1.csv")
all_dmean <- read_csv("./Beta/StagingData/all_dmean_j1.csv")
all_pix <- read_csv("./Beta/StagingData/all_pix_j1.csv")
all_wain <- read_csv("./Beta/StagingData/all_wain_j1.csv")
all_nl <- read_csv("./Beta/StagingData/all_nl_j1.csv")

# Combine all

all_1 <- all_umea %>%
  full_join(all_umax, by = "LLD") %>%
  full_join(all_pix, by = "LLD") %>%
  full_join(all_dmean, by = "LLD") %>%
  bind_rows(all_wain,
            all_nl) %>%
  select(LLD, j_upmean_1, j_upmax_1, j_pix_1, j_dmean_1) %>%
  # Replace all NAs (which are the outliers) with a '10'.
  # The reason for doing this was that extreme outliers (here defined as the
  # top 1%) will skew the jenks breaks calculation, and thus were taken out.
  mutate(j_upmean_1 = replace_na(j_upmean_1, 10),
         j_upmax_1 = replace_na(j_upmax_1, 10),
         j_pix_1 = replace_na(j_pix_1, 10),
         j_dmean_1 = replace_na(j_dmean_1, 10)) %>%
  mutate_if(is.factor, as.character) %>%
  # Invert downslope retention score - i.e., 1 becomes a 10, 2 becomes a 9,
  # etc. This is because a qs with low retention potential (a '1' originally) 
  # is actually the most important, technically speaking, for prioritization.
  mutate(j_dmean_11 = 11L-as.numeric(j_dmean_1)) %>%
  mutate_at(vars(j_upmean_1:j_dmean_1), as.numeric)

# Join back to qs-level data - let's track Municipalities separately here.

ab_cwpt2_qs_m <- ab_cwpt2_qs_3 %>%
  select(LLD, County_MD_SA, up_15__max, up_15__mea, down_15__2, qs_p_load,
         pt_upmean_1:pt_pix_1, Meridian:Quarter) %>%
  left_join(all_1, by = "LLD") %>%
  # Calculate quantile indices
  group_by(County_MD_SA) %>%
  mutate(q_upmean_1 = as.numeric(cut2(up_15__mea, g = 10)),
         q_upmax_1 = as.numeric(cut2(up_15__max, g = 10)),
         q_dmean_1 = as.numeric(cut2(down_15__2, g = 10)),
         q_pix_1 = as.numeric(cut2(qs_p_load, g = 10))) %>%
  ungroup() %>%
  mutate(q_dmean_11 = 11L-as.numeric(q_dmean_1))
  
# Let's do some plots - e.g., plot Jenks vs. Quantiles

plot1 <- ab_cwpt2_qs_m %>%
  filter(pt_upmean_1 <= 0.98,
         County_MD_SA == "Starland County") %>%
  ggplot(aes(x = up_15__mea)) +
  geom_histogram(aes(fill = as.character(j_upmean_1)), bins = 100)

plot1

#-------------------------------------------------------------------------------

# Now let's do at LUF planning regions '_4'

# Read in csv files generated - this is LUF level. 
all_upmax_4 <- read_csv("./Beta/StagingData/all_upmax_j4.csv")
all_upmean_4 <- read_csv("./Beta/StagingData/all_upmean_j4.csv")
all_dmean_4 <- read_csv("./Beta/StagingData/all_dmean_j4.csv")
all_pix_4 <- read_csv("./Beta/StagingData/all_pix_j4.csv")

all_4 <- all_upmean_4 %>%
  full_join(all_upmax_4, by = "LLD") %>%
  full_join(all_pix_4, by = "LLD") %>%
  full_join(all_dmean_4, by = "LLD") %>%
  # Missing two qs's - why? They're in Lloydminster, and weren't assigned a LUF
  # code. 
  select(LLD, j_upmean_4, j_upmax_4, j_pix_4, j_dmean_4) %>%
  # Replace NAs with '10'
  mutate(j_upmean_4 = replace_na(j_upmean_4, 10),
         j_upmax_4 = replace_na(j_upmax_4, 10),
         j_pix_4 = replace_na(j_pix_4, 10),
         j_dmean_4 = replace_na(j_dmean_4, 10)) %>%
  mutate_if(is.factor, as.character) %>%
  # Invest downslope retention score
  mutate(j_dmean_41 = 11L-as.numeric(j_dmean_4)) %>%
  mutate_at(vars(j_upmean_4:j_dmean_4), as.numeric)

# Join back to qs-level data - let's track LUF planning regions separately here.

ab_cwpt2_qs_l <- ab_cwpt2_qs_3 %>%
  select(LLD, LUF_CODE, up_15__max, up_15__mea, down_15__2, qs_p_load,
         pt_upmean_4:pt_pix_4, Meridian:Quarter) %>%
  left_join(all_4, by = "LLD") %>%
  filter(!is.na(LUF_CODE)) %>%
  # Calculate quantile indices
  group_by(LUF_CODE) %>%
  mutate(q_upmean_4 = as.numeric(cut2(up_15__mea, g = 10)),
         q_upmax_4 = as.numeric(cut2(up_15__max, g = 10)),
         q_dmean_4 = as.numeric(cut2(down_15__2, g = 10)),
         q_pix_4 = as.numeric(cut2(qs_p_load, g = 10))) %>%
  ungroup() %>%
  mutate(q_dmean_41 = 11L-as.numeric(q_dmean_4))

# Let's do some plots - e.g., plot Jenks vs. Quantiles

plot4 <- ab_cwpt2_qs_l %>%
  filter(
         LUF_CODE == "07") %>%
  ggplot(aes(x = pt_pix_4)) +
  geom_histogram(bins = 100)

plot4
  
#-------------------------------------------------------------------------------
  
# Now let's do HUC 2 watersheds '_2'

# We actually need to redo this step - calculate % of white zone in each HUC2
# watershed. 

huc2_w <- ab_cwpt2_qs_1 %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(HUC_2)) %>%
  group_by(HUC_2, GWA_NAME) %>%
  count() %>%
  filter(!is.na(GWA_NAME)) %>%
  spread(GWA_NAME, n, fill = 0) %>%
  rename(Green = 'Green Area', White = 'White Area') %>%
  mutate(total_qs = Green + White,
         prop_w = White / total_qs) %>%
  # Filter out watersheds that contain less than 15% white zone.
  filter(prop_w >= 0.15) %>%
  select(HUC_2)

ab_cwpt2_qs_2_2 <- ab_cwpt2_qs_1 %>%
  # Remove watersheds that are primarily Green Area
  filter(HUC_2 %in% huc2_w$HUC_2) %>%
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
ab_lld_geo_2 <- ab_cwpt2_qs_2_2 %>%
  select(LLD)

ab_cwpt2_qs_3_2 <- ab_cwpt2_qs_2_2 %>%
  # Remove geometry to improve speed
  st_set_geometry(NULL) %>%
  # Calculate percentiles in order to remove outliers later
  group_by(HUC_2) %>%
  # HUC 2 watersheds are '_2'
  mutate(pt_upmean_2 = percent_rank(up_15__mea),
         pt_upmax_2 = percent_rank(up_15__max),
         pt_dmean_2 = percent_rank(down_15__2),
         pt_pix_2 = percent_rank(qs_p_load)) %>%
  ungroup()

# OK, finished all the jenks calculations - now, an interesting problem.
# Different number of quarter sections in each 'all_' dataframe. Why would this 
# be? Probably something to do with the white zone filter that I put on. To 
# investigate on Saturday!










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




















