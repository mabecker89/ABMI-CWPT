# Starting with 15m upslope accumulation max ----------------------------------

pix_1 <- ab_cwpt2_qs_3 %>%
  # Filter out high-end outliers - to be assigned a '10' later
  filter(pt_pix_1 <= 0.99) %>%
  split(.$County_MD_SA) %>%
  map(~ classIntervals(.$qs_p_load, style = "fisher", n = 10))

# Athabasca County
athabasca_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Athabasca County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Athabasca County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Beaver County
beaver_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Beaver County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Beaver County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Big Lakes County
biglakes_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Big Lakes County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Big Lakes County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Birch Hills County
birchhills_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Birch Hills County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Birch Hills County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Brazeau County
brazeau_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Brazeau County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Brazeau County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Camrose County
camrose_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Camrose County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Camrose County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Cardston County
cardston_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Cardston County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Cardston County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Clear Hills County
clearhills_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Clear Hills County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Clear Hills County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of  Northern Lights
northlights_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of  Northern Lights",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of  Northern Lights"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Barrhead No. 11
barrhead_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Barrhead No. 11",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Barrhead No. 11"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Forty Mile No. 8
fortymile_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Forty Mile No. 8",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Forty Mile No. 8"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Grande Prairie No. 1
gp_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Grande Prairie No. 1",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Grande Prairie No. 1"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Minburn No. 27
minburn_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Minburn No. 27",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Minburn No. 27"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Newell
newell_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Newell",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Newell"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Paintearth No. 18
paintearth_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Paintearth No. 18",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Paintearth No. 18"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of St. Paul No. 19
stpaul_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of St. Paul No. 19",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of St. Paul No. 19"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Stettler No. 6
stettler_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Stettler No. 6",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Stettler No. 6"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Two Hills No. 21
twohills_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Two Hills No. 21",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Two Hills No. 21"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Vermilion River
vermilion_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Vermilion River",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Vermilion River"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Warner No. 5
warner_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Warner No. 5",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Warner No. 5"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# County of Wetaskiwin No. 10
wetaskiwin_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of Wetaskiwin No. 10",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["County of Wetaskiwin No. 10"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Cypress County
cypress_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Cypress County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Cypress County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Flagstaff County
flagstaff_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Flagstaff County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Flagstaff County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Kneehill County
kneehill_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Kneehill County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Kneehill County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Lac La Biche County
llb_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Lac La Biche County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Lac La Biche County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Lac Ste. Anne County
lsac_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Lac Ste. Anne County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Lac Ste. Anne County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Lacombe County
lacombe_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Lacombe County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Lacombe County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Lamont County
lamont_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Lamont County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Lamont County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Leduc County
leduc_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Leduc County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Leduc County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Lethbridge County
leth_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Lethbridge County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Lethbridge County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Acadia No. 34
acadia_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Acadia No. 34",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Acadia No. 34"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Bighorn No. 8
bighorn_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Bighorn No. 8",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Bighorn No. 8"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Bonnyville No. 87
bonny_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Bonnyville No. 87",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Bonnyville No. 87"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Fairview No. 136
fairview_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Fairview No. 136",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Fairview No. 136"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Foothills No. 31
foothills_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Foothills No. 31",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Foothills No. 31"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Peace No. 135
peace_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Peace No. 135",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Peace No. 135"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Pincher Creek No. 9
pincher_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Pincher Creek No. 9",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Pincher Creek No. 9"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Provost No. 52
provost_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Provost No. 52",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Provost No. 52"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Ranchland No. 66
ranchland_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Ranchland No. 66",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Ranchland No. 66"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Smoky River No. 130
smokyriver_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Smoky River No. 130",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Smoky River No. 130"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Spirit River No. 133
spiritriver_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Spirit River No. 133",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Spirit River No. 133"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Taber
taber_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Taber",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Taber"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Wainright No. 61
wainright_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Wainwright No. 61",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Wainwright No. 61"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# M.D. of Willow Creek No. 26
willow_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "M.D. of Willow Creek No. 26",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["M.D. of Willow Creek No. 26"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Mountain View County
mountainv_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Mountain View County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Mountain View County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Municipality of Crowsnest Pass
crowsnest_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Municipality of Crowsnest Pass",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Municipality of Crowsnest Pass"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Parkland County
parkland_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Parkland County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Parkland County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Ponoka County
ponoka_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Ponoka County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Ponoka County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Red Deer County
reddeer_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Red Deer County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Red Deer County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Rocky View County
rockyview_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Rocky View County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Rocky View County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Saddle Hills County
saddle_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Saddle Hills County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Saddle Hills County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Smoky Lake County
smokylake_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Smoky Lake County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Smoky Lake County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Special Areas 2
sa2_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Special Areas 2",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Special Areas 2"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Special Areas 3
sa3_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Special Areas 3",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Special Areas 3"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Special Areas 4
sa4_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Special Areas 4",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Special Areas 4"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Starland County
starland_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Starland County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Starland County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Strathcona County
strathcona_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Strathcona County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Strathcona County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Sturgeon County
sturgeon_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Sturgeon County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Sturgeon County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Thorhild County
thorhild_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Thorhild County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Thorhild County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Vulcan County
vulcan_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Vulcan County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Vulcan County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Westlock County
westlock_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Westlock County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Westlock County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))

# Wheatland County
wheatland_pix <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "Wheatland County",
         pt_pix_1 <= 0.99) %>%
  select(LLD, qs_p_load) %>%
  mutate(j_pix_1 = cut(qs_p_load, 
                          breaks = pix_1[["Wheatland County"]]$brks,
                          labels = as.numeric(1:10))) %>%
  mutate(j_pix_1 = replace_na(j_pix_1, 1))


# Missing Wainwright
all_pix <- bind_rows(
  acadia_pix, athabasca_pix, barrhead_pix, beaver_pix, bighorn_pix,
  biglakes_pix, birchhills_pix, bonny_pix, brazeau_pix, camrose_pix,
  cardston_pix, clearhills_pix, crowsnest_pix, cypress_pix, fairview_pix,
  flagstaff_pix, foothills_pix, fortymile_pix, gp_pix, kneehill_pix,
  lacombe_pix, lamont_pix, leduc_pix, leth_pix, llb_pix, lsac_pix,
  minburn_pix, mountainv_pix, newell_pix, paintearth_pix, parkland_pix,
  peace_pix, pincher_pix, ponoka_pix, provost_pix, ranchland_pix, 
  reddeer_pix, rockyview_pix, sa2_pix, sa3_pix, sa4_pix, saddle_pix,
  smokylake_pix, smokyriver_pix, spiritriver_pix, starland_pix, 
  stettler_pix, stpaul_pix, strathcona_pix, sturgeon_pix, taber_pix,
  thorhild_pix, twohills_pix, vermilion_pix, vulcan_pix, warner_pix,
  westlock_pix, wetaskiwin_pix, wheatland_pix, willow_pix
)

# County of  Northern Lights
northlights_umea <- ab_cwpt2_qs_3 %>%
  filter(County_MD_SA == "County of  Northern Lights",
         pt_upmean_1 <= 0.99) %>%
  select(LLD, up_15__mea) %>%
  mutate(j_upmean_1 = cut(up_15__mea, 
                       breaks = upmean_1[["County of  Northern Lights"]]$brks,
                       labels = as.numeric(1:10))) %>%
  mutate(j_upmean_1 = replace_na(j_upmean_1, 1))

all_nl <- northlights_dmean %>%
  full_join(northlights_pix, by = "LLD") %>%
  full_join(northlights_umax, by = "LLD") %>%
  full_join(northlights_umea, by = "LLD")















