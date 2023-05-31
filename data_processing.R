# This is for downloading and processing the .nc files from GridMET: https://www.climatologylab.org/gridmet.html
# I used the second option '2. Create wget script for downloading NetCDF files' for this: https://www.climatologylab.org/wget-gridmet.html


# "Leave one out" cross validation
# 



library(raster)
library(rasterVis)
library(ncdf4)
library(lattice)
library(stringr)
library(raster)
library(sf)
library(dplyr)
source("../geocoding_addresses/R/ozone_krige.R")

# Necessary Folders
path_start = "data/" #change as needed - I set my project directory to the same folder as the data
# Data: http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html
path_to_data_folder="../geocoding_addresses/data/krige_data/"
added_tiffs_list = list.files(path_to_data_folder)

# Random Forest Variables
# Spatial Variables:

### Dist to nearest Road - 
path_to_data_folder="../geocoding_addresses/data/krige_data/"
added_tiffs_list = list.files(path_to_data_folder)
path_to_roads="../geocoding_addresses/data/krige_data/co_roads_2019/"
road_shp_path = "co_roads_2019.shp"
# C = County
# I = Interstate
# M = Common Name
# O = Other
# S = State recognized
# U = U.S.
roads_shp = st_read(paste0(path_to_roads,road_shp_path))
roads = st_transform(roads_shp,crs=CRS(prg))
roads_projected = as(roads, "Spatial")
dist2road = o3_projected$dist2road[1]=round(rgeos::gDistance(roads_projected,o3_projected[1,]),2)
for(i in 2:nrow(o3_projected)){
  dist2road=c(dist2road,round(rgeos::gDistance(roads_projected,o3_projected[i,]),2))
}
o3_projected$dist2road=dist2road

# sum of roads in 500m buffer - 
add_road_buffer = read_csv("../geocoding_addresses/data/roads_in_500m_buffer.csv")[,c(2,4)] %>% 
  filter(site_name != "Aspen Park" & site_name != "Evergreen" & site_name !="Welch")
names(add_road_buffer) = c("site_name","road_length")
add_road_buffer$site_name=toupper(add_road_buffer$site_name)
add_road_buffer$road_length=ifelse(is.na(add_road_buffer$road_length),0,add_road_buffer$road_length)

# lat 
o3_projected@coords[,"lat"]

# long
o3_projected@coords[,"long"]

# elevation
elevation_to_add = raster(paste0(path_to_data_folder,added_tiffs_list[2]))
elevation_projected = raster::projectRaster(elevation_to_add, crs=prg)
o3_projected$elev=round(raster::extract(elevation_projected,o3_projected),2)


# Temporal Variables:
# monthly dummy variable -
# make a 1 for each month of interest and a 0 for other months
# yearly dummy variable -
# make a 1 for each year of interest and a 0 for other years

# Spatio-Temporal Variables:
# yearly NDVI - Average NDVI 500m buffer - need buffer data is ready
# Monthly mode of wind direction
# Monthly total precip
# Monthly max tmep
# Monthly RH 
# added_tiffs_list -> for identifying correct tiffs
max_temperature_to_add = raster(paste0(path_to_data_folder,added_tiffs_list[15]))
max_temperature_projected = raster::projectRaster(max_temperature_to_add, crs=prg)
o3_projected$max_temp=round(raster::extract(max_temperature_projected,o3_projected),2)
#rounded K to F formula: 1.8*(K-273) + 32

# head(cbind(o3_projected$site_name,o3_projected$dist2road,o3_projected$max_temp,o3_projected$elev))
# join elevation column back to o3_projected
# o3_projected = merge(x=o3_projected,y=o3_elevs,by="site_name", duplicateGeoms = TRUE)
#o3_projected
year_o3 = as.data.frame(o3_projected)
year_o3 = year_o3 %>% 
  dplyr::select(c("site_name","dist2road","long","lat"),everything())

# use to create dataframe of specific months, ex below is summer
# summer_o3 = year_o3 %>%
#   dplyr::select(contains(c("site_name","lat","long","elevation","Apr","May","Jun","Jul","Aug","Sep","Oct")))

# preview data
#summer_o3

# Monthly max rh 
# Monthly wind speed