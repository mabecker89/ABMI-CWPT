#-------------------------------------------------------------------------------

# CWPT 2.0

# Calculating Indices for Municipalities

#-------------------------------------------------------------------------------

# Load packages

library(tidyverse)
library(sf)
library(classInt)
library(Hmisc)

#-------------------------------------------------------------------------------

# Import data (from Cleanup script)
cwpt_qs_all_1 <- read_csv("./Data/Intermediate/cwpt_qs_all_wPNTSS.csv")

cwpt_qs_mun <- cwpt_qs_all_1 %>%
  select(LLD:GWA_NAME, up_p_max:Quarter)

# Calculate percentage of white zone in each municipality

county_w <- cwpt_qs_mun %>%
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

# Question is: do we want to consider green zone areas when calculating indices?
# I think for counties with > 50% white zone, use everything. 
# For counties with < 50% white zone, filter for white zone first.

cwpt_qs_mun_1 <- cwpt_qs_mun %>%
  filter(County_MD_SA %in% county_w$County_MD_SA) %>%
  group_by(County_MD_SA) %>%
  mutate(pct_up_p_mean = percent_rank(up_p_mean),
         pct_up_p_max = percent_rank(up_p_max),
         pct_up_n_mean = percent_rank(up_n_mean),
         pct_up_n_max = percent_rank(up_n_max),
         pct_up_tss_mean = percent_rank(up_tss_mean),
         pct_up_tss_max = percent_rank(up_tss_max),
         pct_pix_p_mean = percent_rank(pix_p_mean),
         pct_pix_n_mean = percent_rank(pix_n_mean),
         pct_pix_tss_mean = percent_rank(pix_tss_mean),
         pct_down_mean = percent_rank(down_mean)) %>%
  ungroup()

#-------------------------------------------------------------------------------

# Calculating Jenks scores

# Counties with > 50% white zone

county_w_50 <- county_w %>%
  filter(prop_w > 0.5)

county_g_50 <- county_w %>%
  filter(prop_w < 0.5)

cwpt_qs_mun_1_w50 <- cwpt_qs_mun_1 %>%
  filter(County_MD_SA %in% county_w_50$County_MD_SA)

# Upslope phosphorus max

up_pmax <- cwpt_qs_mun_1_w50 %>%
  # Filter out high-end outliers - to be assigned a '10' later
  filter(pct_up_p_max <= 0.97) %>%
  split(.$County_MD_SA) %>%
  map(~ classIntervals(.$up_p_max, style = "fisher", n = 10))

# Athabasca County
athabasca_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Athabasca County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Athabasca County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Beaver County
beaver_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Beaver County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Beaver County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Birch Hills County
birchhills_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Birch Hills County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Birch Hills County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Camrose County
camrose_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Camrose County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Camrose County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Cardston County
cardston_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cardston County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Cardston County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Barrhead No. 11
barrhead_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Barrhead No. 11",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Barrhead No. 11"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Forty Mile No. 8
fortymile_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Forty Mile No. 8",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Forty Mile No. 8"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Grande Prairie No. 1
gp_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Grande Prairie No. 1",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Grande Prairie No. 1"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Minburn No. 27
minburn_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Minburn No. 27",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Minburn No. 27"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Newell
newell_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Newell",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Newell"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Paintearth No. 18
paintearth_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Paintearth No. 18",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Paintearth No. 18"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of St. Paul No. 19
stpaul_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of St. Paul No. 19",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of St. Paul No. 19"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Stettler No. 6
stettler_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Stettler No. 6",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Stettler No. 6"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Two Hills No. 21
twohills_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Two Hills No. 21",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Two Hills No. 21"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Vermilion River
vermilion_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Vermilion River",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Vermilion River"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Warner No. 5
warner_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Warner No. 5",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Warner No. 5"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# County of Wetaskiwin No. 10
wetaskiwin_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Wetaskiwin No. 10",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["County of Wetaskiwin No. 10"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Cypress County
cypress_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cypress County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Cypress County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Flagstaff County
flagstaff_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Flagstaff County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Flagstaff County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Kneehill County
kneehill_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Kneehill County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Kneehill County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Lac Ste. Anne County
lsac_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lac Ste. Anne County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Lac Ste. Anne County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Lacombe County
lacombe_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lacombe County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Lacombe County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Lamont County
lamont_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lamont County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Lamont County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Leduc County
leduc_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Leduc County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Leduc County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Lethbridge County
leth_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lethbridge County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Lethbridge County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Acadia No. 34
acadia_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Acadia No. 34",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Acadia No. 34"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Bonnyville No. 87
bonny_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Bonnyville No. 87",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Bonnyville No. 87"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Fairview No. 136
fairview_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Fairview No. 136",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Fairview No. 136"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Foothills No. 31
foothills_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Foothills No. 31",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Foothills No. 31"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Peace No. 135
peace_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Peace No. 135",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Peace No. 135"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Pincher Creek No. 9
pincher_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Pincher Creek No. 9",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Pincher Creek No. 9"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Provost No. 52
provost_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Provost No. 52",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Provost No. 52"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Smoky River No. 130
smokyriver_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Smoky River No. 130",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Smoky River No. 130"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Spirit River No. 133
spiritriver_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Spirit River No. 133",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Spirit River No. 133"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Taber
taber_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Taber",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Taber"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Wainwright No. 61
wainright_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Wainwright No. 61",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Wainwright No. 61"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# M.D. of Willow Creek No. 26
willow_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Willow Creek No. 26",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["M.D. of Willow Creek No. 26"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Mountain View County
mountainv_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Mountain View County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Mountain View County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Parkland County
parkland_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Parkland County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Parkland County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Ponoka County
ponoka_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Ponoka County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Ponoka County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Red Deer County
reddeer_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Red Deer County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Red Deer County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Rocky View County
rockyview_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Rocky View County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Rocky View County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Smoky Lake County
smokylake_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Smoky Lake County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Smoky Lake County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Special Areas 2
sa2_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 2",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Special Areas 2"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Special Areas 3
sa3_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 3",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Special Areas 3"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Special Areas 4
sa4_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 4",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Special Areas 4"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Starland County
starland_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Starland County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Starland County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Strathcona County
strathcona_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Strathcona County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Strathcona County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Sturgeon County
sturgeon_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Sturgeon County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Sturgeon County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Thorhild County
thorhild_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Thorhild County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Thorhild County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Vulcan County
vulcan_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Vulcan County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Vulcan County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Westlock County
westlock_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Westlock County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Westlock County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

# Wheatland County
wheatland_up_pmax <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Wheatland County",
         pct_up_p_max <= 0.97) %>%
  select(LLD, up_p_max) %>%
  mutate(j_up_pmax = cut(up_p_max, 
                       breaks = up_pmax[["Wheatland County"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_up_pmax = replace_na(j_up_pmax, 1))

all_up_pmax <- bind_rows(
  acadia_up_pmax, athabasca_up_pmax, barrhead_up_pmax, beaver_up_pmax,
  birchhills_up_pmax, bonny_up_pmax, camrose_up_pmax,
  cardston_up_pmax, cypress_up_pmax, fairview_up_pmax,
  flagstaff_up_pmax, foothills_up_pmax, fortymile_up_pmax, gp_up_pmax, kneehill_up_pmax,
  lacombe_up_pmax, lamont_up_pmax, leduc_up_pmax, leth_up_pmax, lsac_up_pmax,
  minburn_up_pmax, mountainv_up_pmax, newell_up_pmax, paintearth_up_pmax, parkland_up_pmax,
  peace_up_pmax, pincher_up_pmax, ponoka_up_pmax, provost_up_pmax, 
  reddeer_up_pmax, rockyview_up_pmax, sa2_up_pmax, sa3_up_pmax, sa4_up_pmax,
  smokylake_up_pmax, smokyriver_up_pmax, spiritriver_up_pmax, starland_up_pmax, 
  stettler_up_pmax, stpaul_up_pmax, strathcona_up_pmax, sturgeon_up_pmax, taber_up_pmax,
  thorhild_up_pmax, twohills_up_pmax, vermilion_up_pmax, vulcan_up_pmax, warner_up_pmax,
  westlock_up_pmax, wetaskiwin_up_pmax, wheatland_up_pmax, willow_up_pmax,
  wainright_up_pmax
)
              
#-------------------------------------------------------------------------------

# Upslope phosphorus mean

up_pmean <- cwpt_qs_mun_1_w50 %>%
  # Filter out high-end outliers - to be assigned a '10' later
  filter(pct_up_p_mean <= 0.97) %>%
  split(.$County_MD_SA) %>%
  map(~ classIntervals(.$up_p_mean, style = "fisher", n = 10))

# Athabasca County
athabasca_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Athabasca County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Athabasca County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Beaver County
beaver_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Beaver County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Beaver County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Birch Hills County
birchhills_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Birch Hills County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Birch Hills County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Camrose County
camrose_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Camrose County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Camrose County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Cardston County
cardston_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cardston County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Cardston County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Barrhead No. 11
barrhead_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Barrhead No. 11",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Barrhead No. 11"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Forty Mile No. 8
fortymile_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Forty Mile No. 8",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Forty Mile No. 8"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Grande Prairie No. 1
gp_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Grande Prairie No. 1",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Grande Prairie No. 1"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Minburn No. 27
minburn_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Minburn No. 27",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Minburn No. 27"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Newell
newell_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Newell",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Newell"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Paintearth No. 18
paintearth_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Paintearth No. 18",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Paintearth No. 18"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of St. Paul No. 19
stpaul_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of St. Paul No. 19",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of St. Paul No. 19"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Stettler No. 6
stettler_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Stettler No. 6",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Stettler No. 6"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Two Hills No. 21
twohills_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Two Hills No. 21",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Two Hills No. 21"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Vermilion River
vermilion_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Vermilion River",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Vermilion River"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Warner No. 5
warner_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Warner No. 5",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Warner No. 5"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# County of Wetaskiwin No. 10
wetaskiwin_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Wetaskiwin No. 10",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["County of Wetaskiwin No. 10"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Cypress County
cypress_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cypress County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Cypress County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Flagstaff County
flagstaff_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Flagstaff County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Flagstaff County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Kneehill County
kneehill_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Kneehill County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Kneehill County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Lac Ste. Anne County
lsac_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lac Ste. Anne County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Lac Ste. Anne County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Lacombe County
lacombe_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lacombe County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Lacombe County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Lamont County
lamont_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lamont County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Lamont County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Leduc County
leduc_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Leduc County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Leduc County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Lethbridge County
leth_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lethbridge County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Lethbridge County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Acadia No. 34
acadia_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Acadia No. 34",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Acadia No. 34"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Bonnyville No. 87
bonny_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Bonnyville No. 87",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Bonnyville No. 87"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Fairview No. 136
fairview_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Fairview No. 136",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Fairview No. 136"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Foothills No. 31
foothills_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Foothills No. 31",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Foothills No. 31"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Peace No. 135
peace_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Peace No. 135",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Peace No. 135"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Pincher Creek No. 9
pincher_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Pincher Creek No. 9",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Pincher Creek No. 9"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Provost No. 52
provost_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Provost No. 52",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Provost No. 52"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Smoky River No. 130
smokyriver_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Smoky River No. 130",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Smoky River No. 130"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Spirit River No. 133
spiritriver_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Spirit River No. 133",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Spirit River No. 133"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Taber
taber_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Taber",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Taber"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Wainwright No. 61
wainright_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Wainwright No. 61",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Wainwright No. 61"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# M.D. of Willow Creek No. 26
willow_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Willow Creek No. 26",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["M.D. of Willow Creek No. 26"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Mountain View County
mountainv_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Mountain View County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Mountain View County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Parkland County
parkland_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Parkland County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Parkland County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Ponoka County
ponoka_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Ponoka County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Ponoka County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Red Deer County
reddeer_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Red Deer County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Red Deer County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Rocky View County
rockyview_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Rocky View County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Rocky View County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Smoky Lake County
smokylake_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Smoky Lake County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Smoky Lake County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Special Areas 2
sa2_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 2",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Special Areas 2"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Special Areas 3
sa3_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 3",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Special Areas 3"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Special Areas 4
sa4_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 4",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Special Areas 4"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Starland County
starland_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Starland County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Starland County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Strathcona County
strathcona_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Strathcona County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Strathcona County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Sturgeon County
sturgeon_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Sturgeon County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Sturgeon County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Thorhild County
thorhild_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Thorhild County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Thorhild County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Vulcan County
vulcan_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Vulcan County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Vulcan County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Westlock County
westlock_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Westlock County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Westlock County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

# Wheatland County
wheatland_up_pmean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Wheatland County",
         pct_up_p_mean <= 0.97) %>%
  select(LLD, up_p_mean) %>%
  mutate(j_up_pmean = cut(up_p_mean, 
                         breaks = up_pmean[["Wheatland County"]]$brks,
                         labels = as.numeric(1:10))) %>%
  mutate(j_up_pmean = replace_na(j_up_pmean, 1))

all_up_pmean <- bind_rows(
  acadia_up_pmean, athabasca_up_pmean, barrhead_up_pmean, beaver_up_pmean,
  birchhills_up_pmean, bonny_up_pmean, camrose_up_pmean,
  cardston_up_pmean, cypress_up_pmean, fairview_up_pmean,
  flagstaff_up_pmean, foothills_up_pmean, fortymile_up_pmean, gp_up_pmean, kneehill_up_pmean,
  lacombe_up_pmean, lamont_up_pmean, leduc_up_pmean, leth_up_pmean, lsac_up_pmean,
  minburn_up_pmean, mountainv_up_pmean, newell_up_pmean, paintearth_up_pmean, parkland_up_pmean,
  peace_up_pmean, pincher_up_pmean, ponoka_up_pmean, provost_up_pmean, 
  reddeer_up_pmean, rockyview_up_pmean, sa2_up_pmean, sa3_up_pmean, sa4_up_pmean,
  smokylake_up_pmean, smokyriver_up_pmean, spiritriver_up_pmean, starland_up_pmean, 
  stettler_up_pmean, stpaul_up_pmean, strathcona_up_pmean, sturgeon_up_pmean, taber_up_pmean,
  thorhild_up_pmean, twohills_up_pmean, vermilion_up_pmean, vulcan_up_pmean, warner_up_pmean,
  westlock_up_pmean, wetaskiwin_up_pmean, wheatland_up_pmean, willow_up_pmean,
  wainright_up_pmean
)

#-------------------------------------------------------------------------------

# Downslope retention

downmean <- cwpt_qs_mun_1_w50 %>%
  # Filter out high-end outliers - to be assigned a '10' later
  filter(pct_down_mean <= 0.97) %>%
  split(.$County_MD_SA) %>%
  map(~ classIntervals(.$down_mean, style = "fisher", n = 10))

# Athabasca County
athabasca_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Athabasca County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Athabasca County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Beaver County
beaver_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Beaver County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Beaver County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Birch Hills County
birchhills_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Birch Hills County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Birch Hills County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Camrose County
camrose_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Camrose County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Camrose County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Cardston County
cardston_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cardston County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Cardston County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Barrhead No. 11
barrhead_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Barrhead No. 11",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Barrhead No. 11"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Forty Mile No. 8
fortymile_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Forty Mile No. 8",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Forty Mile No. 8"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Grande Prairie No. 1
gp_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Grande Prairie No. 1",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Grande Prairie No. 1"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Minburn No. 27
minburn_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Minburn No. 27",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Minburn No. 27"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Newell
newell_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Newell",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Newell"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Paintearth No. 18
paintearth_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Paintearth No. 18",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Paintearth No. 18"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of St. Paul No. 19
stpaul_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of St. Paul No. 19",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of St. Paul No. 19"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Stettler No. 6
stettler_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Stettler No. 6",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Stettler No. 6"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Two Hills No. 21
twohills_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Two Hills No. 21",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Two Hills No. 21"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Vermilion River
vermilion_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Vermilion River",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Vermilion River"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Warner No. 5
warner_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Warner No. 5",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Warner No. 5"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# County of Wetaskiwin No. 10
wetaskiwin_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Wetaskiwin No. 10",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["County of Wetaskiwin No. 10"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Cypress County
cypress_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cypress County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Cypress County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Flagstaff County
flagstaff_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Flagstaff County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Flagstaff County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Kneehill County
kneehill_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Kneehill County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Kneehill County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Lac Ste. Anne County
lsac_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lac Ste. Anne County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Lac Ste. Anne County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Lacombe County
lacombe_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lacombe County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Lacombe County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Lamont County
lamont_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lamont County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Lamont County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Leduc County
leduc_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Leduc County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Leduc County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Lethbridge County
leth_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lethbridge County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Lethbridge County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Acadia No. 34
acadia_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Acadia No. 34",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Acadia No. 34"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Bonnyville No. 87
bonny_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Bonnyville No. 87",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Bonnyville No. 87"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Fairview No. 136
fairview_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Fairview No. 136",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Fairview No. 136"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Foothills No. 31
foothills_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Foothills No. 31",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Foothills No. 31"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Peace No. 135
peace_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Peace No. 135",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Peace No. 135"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Pincher Creek No. 9
pincher_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Pincher Creek No. 9",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Pincher Creek No. 9"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Provost No. 52
provost_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Provost No. 52",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Provost No. 52"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Smoky River No. 130
smokyriver_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Smoky River No. 130",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Smoky River No. 130"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Spirit River No. 133
spiritriver_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Spirit River No. 133",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Spirit River No. 133"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Taber
taber_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Taber",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Taber"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Wainwright No. 61
wainright_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Wainwright No. 61",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Wainwright No. 61"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# M.D. of Willow Creek No. 26
willow_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Willow Creek No. 26",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["M.D. of Willow Creek No. 26"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Mountain View County
mountainv_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Mountain View County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Mountain View County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Parkland County
parkland_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Parkland County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Parkland County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Ponoka County
ponoka_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Ponoka County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Ponoka County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Red Deer County
reddeer_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Red Deer County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Red Deer County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Rocky View County
rockyview_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Rocky View County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Rocky View County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Smoky Lake County
smokylake_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Smoky Lake County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Smoky Lake County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Special Areas 2
sa2_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 2",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Special Areas 2"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Special Areas 3
sa3_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 3",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Special Areas 3"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Special Areas 4
sa4_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 4",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Special Areas 4"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Starland County
starland_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Starland County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Starland County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Strathcona County
strathcona_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Strathcona County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Strathcona County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Sturgeon County
sturgeon_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Sturgeon County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Sturgeon County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Thorhild County
thorhild_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Thorhild County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Thorhild County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Vulcan County
vulcan_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Vulcan County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Vulcan County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Westlock County
westlock_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Westlock County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Westlock County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

# Wheatland County
wheatland_down_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Wheatland County",
         pct_down_mean <= 0.97) %>%
  select(LLD, down_mean) %>%
  mutate(j_down_mean = cut(down_mean, 
                          breaks = downmean[["Wheatland County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_down_mean = replace_na(j_down_mean, 1))

all_down_mean <- bind_rows(
  acadia_down_mean, athabasca_down_mean, barrhead_down_mean, beaver_down_mean,
  birchhills_down_mean, bonny_down_mean, camrose_down_mean,
  cardston_down_mean, cypress_down_mean, fairview_down_mean,
  flagstaff_down_mean, foothills_down_mean, fortymile_down_mean, gp_down_mean, kneehill_down_mean,
  lacombe_down_mean, lamont_down_mean, leduc_down_mean, leth_down_mean, lsac_down_mean,
  minburn_down_mean, mountainv_down_mean, newell_down_mean, paintearth_down_mean, parkland_down_mean,
  peace_down_mean, pincher_down_mean, ponoka_down_mean, provost_down_mean, 
  reddeer_down_mean, rockyview_down_mean, sa2_down_mean, sa3_down_mean, sa4_down_mean,
  smokylake_down_mean, smokyriver_down_mean, spiritriver_down_mean, starland_down_mean, 
  stettler_down_mean, stpaul_down_mean, strathcona_down_mean, sturgeon_down_mean, taber_down_mean,
  thorhild_down_mean, twohills_down_mean, vermilion_down_mean, vulcan_down_mean, warner_down_mean,
  westlock_down_mean, wetaskiwin_down_mean, wheatland_down_mean, willow_down_mean,
  wainright_down_mean
)

#-------------------------------------------------------------------------------

# Phosphorus on-pixel score

pix_pmean <- cwpt_qs_mun_1_w50 %>%
  # Filter out high-end outliers - to be assigned a '10' later
  filter(pct_pix_p_mean <= 0.97) %>%
  split(.$County_MD_SA) %>%
  map(~ classIntervals(.$pix_p_mean, style = "fisher", n = 10))

# Athabasca County
athabasca_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Athabasca County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Athabasca County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Beaver County
beaver_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Beaver County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Beaver County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Birch Hills County
birchhills_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Birch Hills County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Birch Hills County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Camrose County
camrose_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Camrose County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Camrose County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Cardston County
cardston_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cardston County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Cardston County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Barrhead No. 11
barrhead_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Barrhead No. 11",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Barrhead No. 11"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Forty Mile No. 8
fortymile_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Forty Mile No. 8",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Forty Mile No. 8"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Grande Prairie No. 1
gp_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Grande Prairie No. 1",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Grande Prairie No. 1"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Minburn No. 27
minburn_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Minburn No. 27",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Minburn No. 27"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Newell
newell_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Newell",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Newell"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Paintearth No. 18
paintearth_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Paintearth No. 18",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Paintearth No. 18"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of St. Paul No. 19
stpaul_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of St. Paul No. 19",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of St. Paul No. 19"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Stettler No. 6
stettler_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Stettler No. 6",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Stettler No. 6"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Two Hills No. 21
twohills_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Two Hills No. 21",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Two Hills No. 21"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Vermilion River
vermilion_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Vermilion River",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Vermilion River"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Warner No. 5
warner_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Warner No. 5",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Warner No. 5"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# County of Wetaskiwin No. 10
wetaskiwin_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "County of Wetaskiwin No. 10",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["County of Wetaskiwin No. 10"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Cypress County
cypress_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Cypress County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Cypress County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Flagstaff County
flagstaff_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Flagstaff County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Flagstaff County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Kneehill County
kneehill_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Kneehill County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Kneehill County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Lac Ste. Anne County
lsac_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lac Ste. Anne County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Lac Ste. Anne County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Lacombe County
lacombe_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lacombe County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Lacombe County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Lamont County
lamont_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lamont County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Lamont County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Leduc County
leduc_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Leduc County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Leduc County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Lethbridge County
leth_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Lethbridge County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Lethbridge County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Acadia No. 34
acadia_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Acadia No. 34",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Acadia No. 34"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Bonnyville No. 87
bonny_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Bonnyville No. 87",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Bonnyville No. 87"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Fairview No. 136
fairview_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Fairview No. 136",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Fairview No. 136"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Foothills No. 31
foothills_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Foothills No. 31",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Foothills No. 31"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Peace No. 135
peace_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Peace No. 135",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Peace No. 135"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Pincher Creek No. 9
pincher_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Pincher Creek No. 9",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Pincher Creek No. 9"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Provost No. 52
provost_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Provost No. 52",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Provost No. 52"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Smoky River No. 130
smokyriver_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Smoky River No. 130",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Smoky River No. 130"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Spirit River No. 133
spiritriver_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Spirit River No. 133",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Spirit River No. 133"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Taber
taber_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Taber",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Taber"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Wainwright No. 61
wainright_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Wainwright No. 61",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Wainwright No. 61"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# M.D. of Willow Creek No. 26
willow_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "M.D. of Willow Creek No. 26",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["M.D. of Willow Creek No. 26"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Mountain View County
mountainv_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Mountain View County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Mountain View County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Parkland County
parkland_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Parkland County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Parkland County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Ponoka County
ponoka_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Ponoka County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Ponoka County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Red Deer County
reddeer_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Red Deer County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Red Deer County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Rocky View County
rockyview_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Rocky View County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Rocky View County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Smoky Lake County
smokylake_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Smoky Lake County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Smoky Lake County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Special Areas 2
sa2_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 2",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Special Areas 2"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Special Areas 3
sa3_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 3",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Special Areas 3"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Special Areas 4
sa4_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Special Areas 4",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Special Areas 4"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Starland County
starland_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Starland County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Starland County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Strathcona County
strathcona_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Strathcona County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Strathcona County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Sturgeon County
sturgeon_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Sturgeon County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Sturgeon County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Thorhild County
thorhild_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Thorhild County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Thorhild County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Vulcan County
vulcan_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Vulcan County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Vulcan County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Westlock County
westlock_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Westlock County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Westlock County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

# Wheatland County
wheatland_pix_p_mean <- cwpt_qs_mun_1_w50 %>%
  filter(County_MD_SA == "Wheatland County",
         pct_pix_p_mean <= 0.97) %>%
  select(LLD, pix_p_mean) %>%
  mutate(j_pix_p_mean = cut(pix_p_mean, 
                           breaks = pix_pmean[["Wheatland County"]]$brks,
                           labels = as.numeric(1:10))) %>%
  mutate(j_pix_p_mean = replace_na(j_pix_p_mean, 1))

all_pix_p_mean <- bind_rows(
  acadia_pix_p_mean, athabasca_pix_p_mean, barrhead_pix_p_mean, beaver_pix_p_mean,
  birchhills_pix_p_mean, bonny_pix_p_mean, camrose_pix_p_mean,
  cardston_pix_p_mean, cypress_pix_p_mean, fairview_pix_p_mean,
  flagstaff_pix_p_mean, foothills_pix_p_mean, fortymile_pix_p_mean, gp_pix_p_mean, kneehill_pix_p_mean,
  lacombe_pix_p_mean, lamont_pix_p_mean, leduc_pix_p_mean, leth_pix_p_mean, lsac_pix_p_mean,
  minburn_pix_p_mean, mountainv_pix_p_mean, newell_pix_p_mean, paintearth_pix_p_mean, parkland_pix_p_mean,
  peace_pix_p_mean, pincher_pix_p_mean, ponoka_pix_p_mean, provost_pix_p_mean, 
  reddeer_pix_p_mean, rockyview_pix_p_mean, sa2_pix_p_mean, sa3_pix_p_mean, sa4_pix_p_mean,
  smokylake_pix_p_mean, smokyriver_pix_p_mean, spiritriver_pix_p_mean, starland_pix_p_mean, 
  stettler_pix_p_mean, stpaul_pix_p_mean, strathcona_pix_p_mean, sturgeon_pix_p_mean, taber_pix_p_mean,
  thorhild_pix_p_mean, twohills_pix_p_mean, vermilion_pix_p_mean, vulcan_pix_p_mean, warner_pix_p_mean,
  westlock_pix_p_mean, wetaskiwin_pix_p_mean, wheatland_pix_p_mean, willow_pix_p_mean,
  wainright_pix_p_mean
)

# Okay, done! (ish)

#-------------------------------------------------------------------------------

# Combine all

# First, let's suss out which are the 97% and above, and which are 'true' NAs

# These are weird QS's that are basically all water .. but found their way into
# my QS layer. Weird. I guess I will need to filter them out. 

weird_qs <- cwpt_qs_mun_1_w50 %>%
  filter(is.na(up_p_max) | is.na(up_p_mean))

# The '3%' won't be in here ... but it kind of will?  
all_mun <- all_up_pmax %>%
  full_join(all_up_pmean, by = "LLD") %>%
  full_join(all_down_mean, by = "LLD") %>%
  full_join(all_pix_p_mean, by = "LLD") %>%
  # Take out weird QS's
  filter(!LLD %in% weird_qs$LLD) %>%
  # Now, assume every NA in here was a 97 and above
  mutate(j_up_pmax = replace_na(j_up_pmax, 10),
         j_up_pmean = replace_na(j_up_pmean, 10),
         j_down_mean = replace_na(j_down_mean, 10),
         j_pix_p_mean = replace_na(j_pix_p_mean, 10)) %>%
  mutate_if(is.factor, as.character) %>%
  # Invert downslope retention score
  mutate(j_down_mean1 = 11L - as.numeric(j_down_mean)) %>%
  select(LLD, j_up_pmax, j_up_pmean, j_down_mean, 
         j_down_mean1, j_pix_p_mean) %>%
  mutate_at(vars(j_up_pmax:j_pix_p_mean), as.numeric)

qs_mun_w50 <- cwpt_qs_mun_1_w50 %>%
  # Take out weird Qs's
  filter(!LLD %in% weird_qs$LLD) %>%
  left_join(all_mun, by = "LLD") %>%
  # Calculate quantile indices
  group_by(County_MD_SA) %>%
  mutate(q_up_pmax = as.numeric(cut2(up_p_max, g = 10)),
         q_up_pmean = as.numeric(cut2(up_p_mean, g = 10)),
         q_down_mean = as.numeric(cut2(down_mean, g = 10)),
         q_pix_p_mean = as.numeric(cut2(pix_p_mean, g = 10))) %>%
  ungroup() %>%
  # Reverse downslope retention score
  mutate(q_down_mean1 = 11L - as.numeric(q_down_mean)) %>%
  # Calculate landscape importance score
  mutate(j_land_max = (j_up_pmax + j_down_mean1) / 2,
         j_land_mean = (j_up_pmean + j_down_mean1) / 2,
         q_land_max = (q_up_pmax + q_down_mean1) / 2,
         q_land_mean = (q_up_pmean + q_down_mean1) / 2)

# Write as Clean csv files
#write_csv(qs_mun_w50_1, "./Data/Clean/cwpt_qs_mun_w50.csv")

#-------------------------------------------------------------------------------

# Workaround to refine the Ag Qs's.

ng <- cwpt_qs_all_1 %>%
  select(LinkID, pct_ng)

qs_mun_w50_1 <- cwpt_qs_mun_w50 %>%
  left_join(ng, by = "LinkID") %>%
  mutate(pct_rp = round(pct_rp, digits = 2),
         pct_crop = round(pct_crop, digits = 2),
         pct_tp = round(pct_tp, digits = 2),
         pct_ma = round(pct_ma, digits = 2),
         pct_ng = round(pct_ng, digits = 2)) %>%
  mutate(pct_rp = if_else(is.na(pct_rp), 0, pct_rp),
         pct_crop = if_else(is.na(pct_crop), 0, pct_crop),
         pct_tp = if_else(is.na(pct_tp), 0, pct_tp),
         pct_ma = if_else(is.na(pct_ma), 0, pct_ma),
         pct_ng = if_else(is.na(pct_ng), 0, pct_ng)) %>%
  mutate(pct_rp = if_else(pct_rp > 1, 1, pct_rp),
         pct_crop = if_else(pct_crop > 1, 1, pct_crop),
         pct_tp = if_else(pct_tp > 1, 1, pct_tp),
         pct_ma = if_else(pct_ma > 1, 1, pct_ma),
         pct_ng = if_else(pct_ng > 1, 1, pct_ng))
         
# Write as Clean csv files
write_csv(qs_mun_w50_1, "./Data/Clean/cwpt_qs_mun_w50.csv")


#-------------------------------------------------------------------------------

# Let's take a look at what we've got! 

plot1 <- qs_mun_w50 %>%
  filter(County_MD_SA == "Lac Ste. Anne County",
         pct_pix_p_mean <= 0.97,
         j_land_mean >= 7) %>%
  ggplot(aes(x = pix_p_mean)) +
  geom_histogram(aes(fill = as.factor(j_pix_p_mean)), bins = 50)

plot1

#-------------------------------------------------------------------------------






























