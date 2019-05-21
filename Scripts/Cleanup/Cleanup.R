#-------------------------------------------------------------------------------

# CWPT 2.0

# Joining ES information to QS Shapefile.

#-------------------------------------------------------------------------------

# Load packages

library(tidyverse)
library(sf)

# Import data

cwpt_qs_all <- st_read("./Data/Spatial/Raw/cwpt_qs_AB_final.shp",
                       stringsAsFactors = FALSE, quiet = TRUE)

p_up <- read_csv("./Data/Raw/cwpt_p_flowacc_qs_15m.csv")
n_up <- read_csv("./Data/Raw/cwpt_n_flowacc_qs_15m.csv")
tss_up <- read_csv("./Data/Raw/cwpt_tss_flowacc_qs_15m.csv")
p_pix <- read_csv("./Data/Raw/cwpt_p_pixel_qs_15m.csv")
n_pix <- read_csv("./Data/Raw/cwpt_n_pixel_qs_15m.csv")
tss_pix <- read_csv("./Data/Raw/cwpt_tss_pixel_qs_15m.csv")
down <- read_csv("./Data/Raw/cwpt_downret_qs_15m.csv")
qs_ag <- read_csv("./Data/Raw/cwpt_ag_qs_15m.csv")

# Do some magic!

cwpt_qs_all_1 <- cwpt_qs_all %>%
  # Remove geomtetry for faster processing
  st_set_geometry(NULL) %>%
  # Join in p, n, and tss raw data and clean
  left_join(p_up, by = "LinkID") %>%
  select(-c(OBJECTID:MIN, RANGE, STD, SUM)) %>%
  rename(up_p_max = MAX,
         up_p_mean = MEAN) %>%
  left_join(n_up, by = "LinkID") %>%
  select(-c(OBJECTID:MIN, RANGE, STD, SUM)) %>%
  rename(up_n_max = MAX,
         up_n_mean = MEAN) %>%
  left_join(tss_up, by = "LinkID") %>%
  select(-c(OBJECTID:MIN, RANGE, STD, SUM)) %>%
  rename(up_tss_max = MAX, 
         up_tss_mean = MEAN) %>%
  left_join(p_pix, by = "LinkID") %>%
  select(-c(OBJECTID:AREA)) %>%
  rename(pix_p_mean = MEAN) %>%
  left_join(n_pix, by = "LinkID") %>%
  select(-c(OBJECTID:AREA)) %>%
  rename(pix_n_mean = MEAN) %>%
  left_join(tss_pix, by = "LinkID") %>%
  select(-c(OBJECTID:AREA)) %>%
  rename(pix_tss_mean = MEAN) %>%
  left_join(down, by = "LinkID") %>%
  select(-c(OBJECTID:MAX)) %>%
  rename(down_mean = MEAN) %>%
  # Join ag hf information; calculate percentage of ag cover in each qs
  left_join(qs_ag, by = c("LinkID" = "LINKID")) %>%
  mutate(pct_rp = VALUE_1 / Shape_Area,
         pct_crop = VALUE_2 / Shape_Area,
         pct_tp = VALUE_3 / Shape_Area,
         pct_ma = VALUE_4 / Shape_Area,
         pct_ng = VALUE_5 / Shape_Area) %>%
  # Create one field for county/municipal district/special area
  mutate(Rural_MD_County = ifelse(!is.na(SPMUN_NAME), SPMUN_NAME, MD_NAME),
         County_MD_SA = ifelse(!is.na(SPAREA_NAM), 
                               SPAREA_NAM, Rural_MD_County)) %>%
  # Create LLD variable(s)
  mutate(Meridian = "W") %>%
  mutate(LLD = paste0(Meridian, LinkID)) %>%
  separate(LLD, c("Meridian", "Range", "Township", "Section", "Quarter"),
           sep = "-") %>%
  unite(LLD, c("Quarter", "Section", "Township", "Range", "Meridian"),
        sep = "-", remove = FALSE) %>%
  rename(HUC2_NAME = NAME) %>%
  select(LLD, LinkID, County_MD_SA, GWA_NAME, HUC2_NAME, LUF_NAME,
         up_p_max:down_mean, pct_rp:pct_ng, Meridian:Quarter)
  
# Write csv as intermediate data file
write_csv(cwpt_qs_all_1, "./Data/Intermediate/cwpt_qs_all_wPNTSS.csv")

# Create shape file of just LinkID and their respective geometries, to be put in
# 'Clean' spatial data folder.

cwpt_qs_all <- cwpt_qs_all %>%
  select(LinkID) # Keeps geometry automatically.

st_write(cwpt_qs_all, "./Data/Spatial/Clean/cwpt_qs_all.shp")















