#-------------------------------------------------------------------------------

# To render CWPT_Mun.Rmd with available Counties

aoi_names <-  c("Lac Ste. Anne County","Parkland County","Leduc County","County of Wetaskiwin No. 10",   
  "Lacombe County","County of Vermilion River","Flagstaff County","Red Deer County",               
  "Mountain View County","Rocky View County","Wheatland County","Vulcan County",                 
  "Camrose County","Ponoka County","County of Grande Prairie No. 1","County of Barrhead No. 11",    
  "Westlock County","Strathcona County","Sturgeon County")
                        
function(aoi_names) {
  rmarkdown::render("./CWPT_Mun.Rmd", 
    output_file = paste0(str_replace_all(aoi_names, "[ .]", ""), "_cwpt.html"),
    params = list(aoi_names = aoi_names), 
    output_options = list(self_contained = FALSE, lib_dir = "./docs/libs"),
    output_dir = "./docs")
}

purrr::walk(aoi_names, renderCWPT)