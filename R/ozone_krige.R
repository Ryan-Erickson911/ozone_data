library(gstat)
library(maptools)
library(tidyverse)
library(maps)
library(ggmap)
library(ggsn)
library(elevatr)
library(sf)
library(automap)
library(gt)
library(raster)
# remotes::install_github("AllanCameron/geomtextpath")
library(geomtextpath)

getwd()
####HAND VARIOGRAMS
# semi-varigram formula : 1/2n*sum[Z(u)-Z(u+h)]2
# n=number of pairs
# Z(u) = data
# Z(h) = lag data

## read in ozone data - data cleaning for ozone krige
# mda8 = weekly average of max daily 8 hour ozone levels 
o3 = read_csv("../final_data/o3_mthly_wide.csv") # the "../" bit of code is only so the .R file will run for the .Rmd file
# head(o3)

# turn o3 into a dataframe
# turn site names to sentence case
# mutate vars to be * 1000 for better results
o3 = as.data.frame(o3) %>%
  dplyr::group_by(site_name) %>% 
  dplyr::summarise(across(everything(), ~.[!is.na(.)][1])) %>% 
  dplyr::mutate(site_name = str_to_title(site_name)) %>%
  dplyr::mutate_at(vars(-site_name, -long, -lat), ~ . * 1000)

write_this = o3 %>% 
  dplyr::select(site_name, lat, long)

# write.csv2(write_this,"data/guage_locs.csv")

# change month names
colnames(o3)[4:60] = format(as.Date(colnames(o3)[4:60]), "%b %Y")

# preview data
# head(o3)
# summary(o3)

# Review NAs; flag ones to delete
# Aspen Park, Briggsdale, Deadman, Evergreen, Pawnee Buttes, Plateville Atmospheric Observatory, US Air Force
o3 %>%
  dplyr::group_by(site_name) %>%
  dplyr::summarise_all(~sum(is.na(.))) %>%
  dplyr::transmute(site_name, sumNA = rowSums(.[-1]))

# Remove missing obs
o3 = o3[!(o3$site_name=="Aspen Park" | o3$site_name=="Evergreen" | o3$site_name=="Welch"),]

# project stations
## need to get these to add

# set projection string
# "+proj=utm +zone=13" -> old one
prg = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs +type=crs"
o3 = o3 %>% 
  filter(!is.na(long))

o3_500m = st_as_sf(o3, coords = c("long", "lat"), crs="EPSG:4326") %>% 
  st_buffer(500)
#%>% st_write("data/krige_data/co_roads_crop/gauge_buffers.shp")

o3_500m_projected= o3_500m %>% 
  as("Spatial") %>% 
  spTransform(CRS(prg))

coordinates(o3) = c('long', 'lat')
proj4string(o3) = CRS(SRS_string = "EPSG:4326")
o3_projected = spTransform(o3, CRS(prg))
#o3_projected

# read in CO counties shp
# source: CDPHE https://data-cdphe.opendata.arcgis.com/datasets/CDPHE::colorado-county-boundaries/about
# co_co = st_read("data/co_counties/Colorado_County_Boundaries.shp") # old shp file
co_co = USAboundaries::us_counties(states = "Colorado") %>% 
  st_transform(crs="EPSG:32613")#2020 boundaries and implemented package

# Select only Denver metro counties and geometry
den_co = co_co[which(co_co$name %in% c("Adams","Arapahoe","Boulder","Broomfield","Denver","Douglas","Jefferson")),] %>%
  dplyr::select(name, geometry)
# map check
# plot(den_co)
# convert to a spatial object, reproject
den_projected = as(den_co, "Spatial")
grd = SpatialPixels(SpatialPoints(makegrid(den_projected)))
# den_co = spTransform(den_co, CRS(prg)) # I changed the variable names den_co and den_projected. 
# Original methods had den_co as a spatial object.
#summary(den_projected)
# plot(den_projected)

# create a spatial pixels grid of Denver
# currently the spatial resolution is low to run kriging faster -> Note to inspect when kriging is done