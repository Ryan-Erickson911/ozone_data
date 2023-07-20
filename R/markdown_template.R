# ---
#   title: "Ozone Krige Prediciton"
# subtitle: "Variable: Elevation"
# author: "Ryan Erickson"
# date: "2023-05-19"
# output: html_document
# ---
#   
# Data Prep and Display
source("R/ozone_krige.R")
# get elevation from DEM data. 
# 1. Ozone with Temperature
# 2. Ozone with distance to nearest A1 road
# 3. Ozone with total length of all A1 roads within 500 m
# 4. Ozone with temperature and RH
# 5. Ozone with temperature and distance to nearest A1 road
# 6. Ozone with temperature and elevation
# 7. Ozone with temperature and elevation and distance to nearest A1 road.

path_to_data_folder="data/krige_data/"
added_tiffs_list = list.files(path_to_data_folder)
# added_tiffs_list -> for identifying correct tiffs
o3_data_to_add = raster(paste0(path_to_data_folder,added_tiffs_list[2]))
added_data_projected = raster::projectRaster(o3_data_to_add, crs=prg)
o3_projected$elev=round(raster::extract(added_data_projected,o3_projected),2)

plot(added_data_projected, main="Elevation")
plot(o3_projected, add=TRUE)
plot(den_projected, add=TRUE)

head(cbind(o3_projected$site_name,o3_projected$elev))
# join elevation column back to o3_projected
# o3_projected = merge(x=o3_projected,y=o3_elevs,by="site_name", duplicateGeoms = TRUE)
#o3_projected
year_o3 = as.data.frame(o3_projected)
year_o3 = year_o3 %>% 
  dplyr::select(c("site_name","elev","long","lat"),everything())

# use to create dataframe of specific months, ex below is summer
# summer_o3 = year_o3 %>%
#   dplyr::select(contains(c("site_name","lat","long","elevation","Apr","May","Jun","Jul","Aug","Sep","Oct")))

# preview data
#summer_o3


# Variograms

# # create months as spatial object
coordinates(year_o3) = c('long', 'lat')
proj4string(year_o3) = CRS(prg)
year_o3 = spTransform(year_o3, CRS(prg))
# head(year_o3,3)

# create list of column month names 
cols = names(year_o3)[-c(1:2)]

# create empty list to store variograms
vgms = vector("list")

# loop thru columns
# var name = name of each month
# reformulate to do month~1 -> this is oridinary/simple kriging
# store in vgms, -> add models
for(i in seq_along(cols)) {
  var_name = cols[i]
  v = autofitVariogram(reformulate("max_temp",var_name),year_o3)
  vgms[[i]] = v
}
# vgms

for(i in seq_along(cols)) {
  plot(vgms[[i]])
}
