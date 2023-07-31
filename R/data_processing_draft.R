# This is for downloading and processing the .nc files from GridMET: https://www.climatologylab.org/gridmet.html
# I used the second option '2. Create "wget script" -> might take this out
# file_processing.R creates mean/sum rasters

# Careful when running this, the reprojeciton takes a long time. 

library(raster)
library(rasterVis)
library(ncdf4)
library(lattice)
library(stringr)
library(raster)
library(sf)
library(dplyr)

#### Special R Document 
source("R/ozone_krige.R") #need change for mac when applicable
# Necessary Folders
# Data: http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html
# seek ozone relationship to droughts to use EDDI (https://pubs.acs.org/doi/10.1021/acs.est.1c07260) 
# seek ozone relationship to ground temps/IR reflection

# full sites in use in model across Colorado = 16
co_data_list = list.files("final_data/")

# Random Forest Variables
### Spatial Variables:

### Dist to nearest Road -  
getting_folders = grep("co_roads_2019", co_data_list, value = T)
road_shp_file = "co_roads_2019.shp"
path_to_roads = paste0("final_data/",getting_folders,"/")
# C = County
# I = Interstate
# M = Common Name
# O = Other
# S = State recognized
# U = U.S.
roads_shp = st_read(paste0(path_to_roads,road_shp_file))
roads_transformed = st_transform(roads_shp,crs=CRS(prg))
roads_projected = as(roads_transformed, "Spatial")
dist2road = o3_projected$dist2road[1]=round(rgeos::gDistance(roads_projected,o3_projected[1,]),2)
for(i in 2:nrow(o3_projected)){
  dist2road=c(dist2road,round(rgeos::gDistance(roads_projected,o3_projected[i,]),2))
}
o3_projected$dist2road=dist2road

# sum of roads in 500m buffer - 
add_road_buffer = read_csv("final_data/sum_of_rds_500m_buffer_co.csv") %>% 
  dplyr::select(site_name, sum_Length_METERS) %>% 
  filter(site_name %in% o3_projected$site_name)
names(add_road_buffer) = c("site_name","road_length")
add_road_buffer$road_length=ifelse(is.na(add_road_buffer$road_length),0,add_road_buffer$road_length)
o3_projected = merge(x=o3_projected,y=add_road_buffer,by="site_name")

# elevation
elevation_to_add = raster("final_data/elevation.tiff")
elevation_projected = raster::projectRaster(elevation_to_add, crs=prg)
o3_projected$elev=round(raster::extract(elevation_projected,o3_projected),2)

#data frame creation Temporal Rez = Jan 2018 - Sep 2022
year_o3 = as.data.frame(o3_projected) %>% 
  dplyr::select(c("site_name","elev","dist2road","road_length","lat","long"),everything()) %>% 
  pivot_longer(cols = contains(c("2018","2019","2020","2021","2022")), names_to = "date", values_to = "mda8")

# use to create data frame of specific months, ex below is summer
# summer_o3 = year_o3 %>%
#   dplyr::select(contains(c("site_name","lat","long","elev","dist2road","road_length","Apr","May","Jun","Jul","Aug","Sep","Oct"))) %>% 
#   pivot_longer(cols = contains(c("Apr","May","Jun","Jul","Aug","Sep","Oct")), names_to = "date", values_to = "mda8")
# # preview data
# #summer_o3
# 
# # lat 
# summer_o3$lat
# 
# # long
# summer_o3$long

# Spatio-Temporal Variables:
#fix 2017 issue later
new_path = "final_data/Monthly_Averages/"
monthly_path = list.files(paste0(new_path))
max_rh_files=grep("rmax_",monthly_path, value = T)[-1]
max_temp_files=grep("tmmx_",monthly_path, value = T)[-1]
vpd_files=grep("vpd_",monthly_path, value = T)
sum_precip_files=grep("pr_[0-9]+_[A-Za-z]+_sum",monthly_path, value = T)[-1]

renameing_convention = c(paste0(month.abb,".",2018),
                         paste0(month.abb,".",2019),
                         paste0(month.abb,".",2020),
                         paste0(month.abb,".",2021),
                         paste0(month.abb,".",2022))

coordinates(year_o3)=c('long', 'lat')
proj4string(year_o3)=CRS(SRS_string = prg)

site1=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[1]),]
site2=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[2]),]
site3=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[3]),]
site4=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[4]),]
site5=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[5]),]
site6=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[6]),]
site7=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[7]),]
site8=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[8]),]
site9=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[9]),]
site10=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[10]),]
site11=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[11]),]
site12=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[12]),]
site13=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[13]),]
site14=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[14]),]
site15=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[15]),]
site16=year_o3[which(year_o3$site_name==unique(year_o3$site_name)[16]),]

ndvi_sort_2018=year_o3[grep(".2018",year_o3$date),c("site_name","date")]
ndvi_sort_2019=year_o3[grep(".2019",year_o3$date),c("site_name","date")]
ndvi_sort_2020=year_o3[grep(".2020",year_o3$date),c("site_name","date")]
ndvi_sort_2021=year_o3[grep(".2021",year_o3$date),c("site_name","date")]
ndvi_sort_2022=year_o3[grep(".2022",year_o3$date),c("site_name","date")]

################################################# Monthly total precip

sum_precip_to_add = stack(paste0(new_path,sum_precip_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
# plots
# plot(sum_precip_to_add$tmmx_2017_monthly_avg_1.1, main="Precipitation for Jan 2017")
sum_precip_projected = raster::projectRaster(sum_precip_to_add, crs=prg)
names(sum_precip_projected) = renameing_convention
#plots: after running names() (above code)
# plot(sum_precip_projected$Jan.2017, main="Precipitation for Jan 2017")

total_precip=sum_precip_projected[[c(grep("2018", names(sum_precip_projected)),
                                           grep("2019", names(sum_precip_projected)),
                                           grep("2020", names(sum_precip_projected)),
                                           grep("2021", names(sum_precip_projected)),
                                           grep("2022", names(sum_precip_projected)))]]

# summer_sum_precip =total_precip[[c(grep("Apr", names(total_precip)),
#                                         grep("May", names(total_precip)),
#                                         grep("Jun", names(total_precip)),
#                                         grep("Jul", names(total_precip)),
#                                         grep("Aug", names(total_precip)),
#                                         grep("Sep", names(total_precip)),
#                                         grep("Oct", names(total_precip)))]]

################################################## Monthly max temp -> rounded K to F formula just in case: 1.8*(K-273) + 32

max_temperature_to_add = stack(paste0(new_path,max_temp_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
#plots
# plot(max_temperature_to_add$tmmx_2017_monthly_avg_1.1, main="Temperature for Jan 2017")

max_temperature_projected = raster::projectRaster(max_temperature_to_add, crs=prg)
names(max_temperature_projected) = renameing_convention
#plots: after running names() (above code)
# plot(max_temperature_projected$Jan.2017, main="Temperature for Jan 2017")

max_temps=max_temperature_projected[[c(grep("2018", names(max_temperature_projected)),
                                               grep("2019", names(max_temperature_projected)),
                                               grep("2020", names(max_temperature_projected)),
                                               grep("2021", names(max_temperature_projected)),
                                               grep("2022", names(max_temperature_projected)))]]

# summer_max_temps=max_temps[[c(grep("Apr", names(max_temps)),
#                                       grep("May", names(max_temps)),
#                                       grep("Jun", names(max_temps)),
#                                       grep("Jul", names(max_temps)),
#                                       grep("Aug", names(max_temps)),
#                                       grep("Sep", names(max_temps)),
#                                       grep("Oct", names(max_temps)))]]

##################################################  Monthly RH 

max_rh_to_add = stack(paste0(new_path,max_rh_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
#plots
# plot(max_rh_to_add$tmmx_2017_monthly_avg_1.1, main="Relative Humidity for Jan 2017")
max_rh_projected = raster::projectRaster(max_rh_to_add, crs=prg)
names(max_rh_projected) = renameing_convention
#plots: after running names() (above code)
# plot(max_rh_projected$Jan.2017, main="Relative Humidity for Jan 2017")
max_rh=max_rh_projected[[c(grep("2018", names(max_rh_projected)),
                                   grep("2019", names(max_rh_projected)),
                                   grep("2020", names(max_rh_projected)),
                                   grep("2021", names(max_rh_projected)),
                                   grep("2022", names(max_rh_projected)))]]

# summer_max_rh=max_rh[[c(grep("Apr", names(max_rh)),
#                                 grep("May", names(max_rh)),
#                                 grep("Jun", names(max_rh)),
#                                 grep("Jul", names(max_rh)),
#                                 grep("Aug", names(max_rh)),
#                                 grep("Sep", names(max_rh)),
#                                 grep("Oct", names(max_rh)))]]

##################################################  Monthly VPD 

vpd_to_add = stack(paste0(new_path,vpd_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
#plots
# plot(vpd_to_add$vpd_2017_monthly_avg_1.1, main="Vapor Pressure Deficit for Jan 2017")
vpd_projected = raster::projectRaster(vpd_to_add, crs=prg)
names(vpd_projected) = renameing_convention
#plots: after running names() (above code)
# plot(max_rh_projected$Jan.2017, main="Relative Humidity for Jan 2017")
vpd_avg=vpd_projected[[c(grep("2018", names(vpd_projected)),
                           grep("2019", names(vpd_projected)),
                           grep("2020", names(vpd_projected)),
                           grep("2021", names(vpd_projected)),
                           grep("2022", names(vpd_projected)))]]

################################################## yearly NDVI - Average NDVI 500m buffer - run comments if ndvi is not projected

# Process for creating and extracting NDVIs in R:
# import rasters/mosaic as stacks for each year, folder has only b4 and b5 bands 
# band math
# reproject to match points
# have o3_500m
# path_start = "data/"
# ndvi = "NDVI_data/"
# ndvi_folders = paste0(path_start,ndvi)

ndvi_folder = "NDVIs/"
ndvi_path = list.dirs(paste0("final_data/",ndvi_folder))[-1]
ndvi_files = paste0(ndvi_path,"/co_ndvi_",2018:2022,".tif")
# ndvi_2018 = raster(ndvi_files[1])
# ndvi_2018_projected = raster::projectRaster(ndvi_2018, crs=prg)
# writeRaster(ndvi_2018_projected, paste0(ndvi_path[1],"/co_ndvi_",2018,"projected.tif"), overwrite=T)
# ndvi_2019 = raster(ndvi_files[2])
# ndvi_2019_projected = raster::projectRaster(ndvi_2019, crs=prg)
# writeRaster(ndvi_2019_projected, paste0(ndvi_path[2],"/co_ndvi_",2019,"projected.tif"), overwrite=T)
# ndvi_2020 = raster(ndvi_files[3])
# ndvi_2020_projected = raster::projectRaster(ndvi_2020, crs=prg)
# writeRaster(ndvi_2020_projected, paste0(ndvi_path[3],"/co_ndvi_",2020,"projected.tif"), overwrite=T)
# ndvi_2021 = raster(ndvi_files[4])
# ndvi_2021_projected = raster::projectRaster(ndvi_2021, crs=prg)
# writeRaster(ndvi_2021_projected, paste0(ndvi_path[4],"/co_ndvi_",2021,"projected.tif"), overwrite=T)
# ndvi_2022 = raster(ndvi_files[5])
# ndvi_2022_projected = raster::projectRaster(ndvi_2022, crs=prg)
# writeRaster(ndvi_2022_projected, paste0(ndvi_path[5],"/co_ndvi_",2022,"projected.tif"), overwrite=T)
ndvi_2018_projected = raster(ndvi_files[1])
ndvi_2019_projected = raster(ndvi_files[2])
ndvi_2020_projected = raster(ndvi_files[3])
ndvi_2021_projected = raster(ndvi_files[4])
ndvi_2022_projected = raster(ndvi_files[5])

# Replacing values less than 0 - need new value
# ndvi_2018_projected = clamp(ndvi_2018_projected, lower=0, useValues=TRUE)
# ndvi_2019_projected = clamp(ndvi_2019_projected, lower=0, useValues=TRUE)
# ndvi_2020_projected = clamp(ndvi_2020_projected, lower=0, useValues=TRUE)
# ndvi_2021_projected = clamp(ndvi_2021_projected, lower=0, useValues=TRUE)
# ndvi_2022_projected = clamp(ndvi_2022_projected, lower=0, useValues=TRUE)

ndvi_sort_2018$ndvi = raster::extract(ndvi_2018_projected,ndvi_sort_2018,buffer=500,fun=mean)
ndvi_sort_2019$ndvi = raster::extract(ndvi_2019_projected,ndvi_sort_2019,buffer=500,fun=mean)
ndvi_sort_2020$ndvi = raster::extract(ndvi_2020_projected,ndvi_sort_2020,buffer=500,fun=mean)
ndvi_sort_2021$ndvi = raster::extract(ndvi_2021_projected,ndvi_sort_2021,buffer=500,fun=mean)
ndvi_sort_2022$ndvi = raster::extract(ndvi_2022_projected,ndvi_sort_2022,buffer=500,fun=mean)

ndvi_to_final_dataframe = as.data.frame(rbind(ndvi_sort_2018,ndvi_sort_2019,ndvi_sort_2020,ndvi_sort_2021,ndvi_sort_2022)) %>% 
  dplyr::select(-lat,-long)
#plot
# plot(ndvi_2018_projected)
################################################## Exposure Assignment
for(i in 1:nrow(site1)) {
  site1$tmax[i] = raster::extract(max_temps[[i]],site1[i,])
  site1$rhmax[i] = raster::extract(max_rh[[i]],site1[i,])
  site1$pmax[i] = raster::extract(total_precip[[i]],site1[i,])
  site1$vpd[i] = raster::extract(vpd_avg[[i]],site1[i,])
}
for(i in 1:nrow(site2)) {
  site2$tmax[i] = raster::extract(max_temps[[i]],site2[i,])
  site2$rhmax[i] = raster::extract(max_rh[[i]],site2[i,])
  site2$pmax[i] = raster::extract(total_precip[[i]],site2[i,])
  site2$vpd[i] = raster::extract(vpd_avg[[i]],site2[i,])
}
for(i in 1:nrow(site3)) {
  site3$tmax[i] = raster::extract(max_temps[[i]],site3[i,])
  site3$rhmax[i] = raster::extract(max_rh[[i]],site3[i,])
  site3$pmax[i] = raster::extract(total_precip[[i]],site3[i,])
  site3$vpd[i] = raster::extract(vpd_avg[[i]],site3[i,])
}
for(i in 1:nrow(site4)) {
  site4$tmax[i] = raster::extract(max_temps[[i]],site4[i,])
  site4$rhmax[i] = raster::extract(max_rh[[i]],site4[i,])
  site4$pmax[i] = raster::extract(total_precip[[i]],site4[i,])
  site4$vpd[i] = raster::extract(vpd_avg[[i]],site4[i,])
}
for(i in 1:nrow(site5)) {
  site5$tmax[i] = raster::extract(max_temps[[i]],site5[i,])
  site5$rhmax[i] = raster::extract(max_rh[[i]],site5[i,])
  site5$pmax[i] = raster::extract(total_precip[[i]],site5[i,])
  site5$vpd[i] = raster::extract(vpd_avg[[i]],site5[i,])
}
for(i in 1:nrow(site6)) {
  site6$tmax[i] = raster::extract(max_temps[[i]],site6[i,])
  site6$rhmax[i] = raster::extract(max_rh[[i]],site6[i,])
  site6$pmax[i] = raster::extract(total_precip[[i]],site6[i,])
  site6$vpd[i] = raster::extract(vpd_avg[[i]],site6[i,])
}
for(i in 1:nrow(site7)) {
  site7$tmax[i] = raster::extract(max_temps[[i]],site7[i,])
  site7$rhmax[i] = raster::extract(max_rh[[i]],site7[i,])
  site7$pmax[i] = raster::extract(total_precip[[i]],site7[i,])
  site7$vpd[i] = raster::extract(vpd_avg[[i]],site7[i,])
}
for(i in 1:nrow(site8)) {
  site8$tmax[i] = raster::extract(max_temps[[i]],site8[i,])
  site8$rhmax[i] = raster::extract(max_rh[[i]],site8[i,])
  site8$pmax[i] = raster::extract(total_precip[[i]],site8[i,])
  site8$vpd[i] = raster::extract(vpd_avg[[i]],site8[i,])
}
for(i in 1:nrow(site9)) {
  site9$tmax[i] = raster::extract(max_temps[[i]],site9[i,])
  site9$rhmax[i] = raster::extract(max_rh[[i]],site9[i,])
  site9$pmax[i] = raster::extract(total_precip[[i]],site9[i,])
  site9$vpd[i] = raster::extract(vpd_avg[[i]],site9[i,])
}
for(i in 1:nrow(site10)) {
  site10$tmax[i] = raster::extract(max_temps[[i]],site10[i,])
  site10$rhmax[i] = raster::extract(max_rh[[i]],site10[i,])
  site10$pmax[i] = raster::extract(total_precip[[i]],site10[i,])
  site10$vpd[i] = raster::extract(vpd_avg[[i]],site10[i,])
}
for(i in 1:nrow(site11)) {
  site11$tmax[i] = raster::extract(max_temps[[i]],site11[i,])
  site11$rhmax[i] = raster::extract(max_rh[[i]],site11[i,])
  site11$pmax[i] = raster::extract(total_precip[[i]],site11[i,])
  site11$vpd[i] = raster::extract(vpd_avg[[i]],site11[i,])
}
for(i in 1:nrow(site12)) {
  site12$tmax[i] = raster::extract(max_temps[[i]],site12[i,])
  site12$rhmax[i] = raster::extract(max_rh[[i]],site12[i,])
  site12$pmax[i] = raster::extract(total_precip[[i]],site12[i,])
  site12$vpd[i] = raster::extract(vpd_avg[[i]],site12[i,])
}
for(i in 1:nrow(site13)) {
  site13$tmax[i] = raster::extract(max_temps[[i]],site13[i,])
  site13$rhmax[i] = raster::extract(max_rh[[i]],site13[i,])
  site13$pmax[i] = raster::extract(total_precip[[i]],site13[i,])
  site13$vpd[i] = raster::extract(vpd_avg[[i]],site13[i,])
}
for(i in 1:nrow(site14)) {
  site14$tmax[i] = raster::extract(max_temps[[i]],site14[i,])
  site14$rhmax[i] = raster::extract(max_rh[[i]],site14[i,])
  site14$pmax[i] = raster::extract(total_precip[[i]],site14[i,])
  site14$vpd[i] = raster::extract(vpd_avg[[i]],site14[i,])
}
for(i in 1:nrow(site15)) {
  site15$tmax[i] = raster::extract(max_temps[[i]],site15[i,])
  site15$rhmax[i] = raster::extract(max_rh[[i]],site15[i,])
  site15$pmax[i] = raster::extract(total_precip[[i]],site15[i,])
  site15$vpd[i] = raster::extract(vpd_avg[[i]],site15[i,])
}
for(i in 1:nrow(site16)) {
  site16$tmax[i] = raster::extract(max_temps[[i]],site16[i,])
  site16$rhmax[i] = raster::extract(max_rh[[i]],site16[i,])
  site16$pmax[i] = raster::extract(total_precip[[i]],site16[i,])
  site16$vpd[i] = raster::extract(vpd_avg[[i]],site16[i,])
}
ggs = as.data.frame(rbind(site1,
                          site2,
                          site3,
                          site4,
                          site5,
                          site6,
                          site7,
                          site8,
                          site9,
                          site10,
                          site11,
                          site12,
                          site13,
                          site14,
                          site15,
                          site16))

merge(ndvi_to_final_dataframe, ggs, by=c("site_name", "date")) %>%
  group_by(site_name) %>% 
  slice_head(n=1)

################################################## Dummy Variables:

# monthly dummy variable -
# make a 1 for each month of interest and a 0 for other months
rough_variables = merge(ndvi_to_final_dataframe, ggs, by=c("site_name", "date"))
# rough_variables$apr_dummy = ifelse(str_detect(rough_variables$date, "Apr."),1,0)
# rough_variables$may_dummy = ifelse(str_detect(rough_variables$date, "May."),1,0)
# rough_variables$jun_dummy = ifelse(str_detect(rough_variables$date, "Jun."),1,0)
# rough_variables$jul_dummy = ifelse(str_detect(rough_variables$date, "Jul."),1,0)
# rough_variables$aug_dummy = ifelse(str_detect(rough_variables$date, "Aug."),1,0)
# rough_variables$sep_dummy = ifelse(str_detect(rough_variables$date, "Sep."),1,0)
# rough_variables$oct_dummy = ifelse(str_detect(rough_variables$date, "Oct."),1,0)
# # yearly dummy variable -
# # make a 1 for each year of interest and a 0 for other years
# rough_variables$yr_2018_dummy = ifelse(str_detect(rough_variables$date, ".2018"),1,0)
# rough_variables$yr_2019_dummy = ifelse(str_detect(rough_variables$date, ".2019"),1,0)
# rough_variables$yr_2020_dummy = ifelse(str_detect(rough_variables$date, ".2020"),1,0)
# rough_variables$yr_2021_dummy = ifelse(str_detect(rough_variables$date, ".2021"),1,0)
# rough_variables$yr_2022_dummy = ifelse(str_detect(rough_variables$date, ".2022"),1,0)
rough_variables=rough_variables %>% 
  dplyr::select(site_name,date,lat,long,mda8,everything())
write.csv(rough_variables,"final_data/ozone_data.csv")
##################################################  RF Model, "Leave one out" cross validation
# STAT 5610 RF Notes
################################################################################################
## Random forests for regression: simple example
## But, also kindof stupid because random forests are meant for high-dimensional feature space
################################################################################################
# library(MASS)
# library(rpart)
# library(ranger)
# library(pROC)
# library(Metrics)
# library(RColorBrewer)
# 
# ozone_data=read.csv("final_data/ozone_data.csv")%>% 
#   dplyr::select(site_name,date,lat,long,mda8,everything())
# 
# ozone_data$site_name = as.factor(ozone_data$site_name)
# ozone_data$date = as.factor(ozone_data$date)  
# 
# ozone_data_no.mda8 = ozone_data %>% 
#   dplyr::select(-c("mda8"))
# 
# ozone_data_mda8_first = ozone_data %>% 
#   dplyr::select(mda8,everything())
# 
# 
# line = glm(mda8~., data=ozone_data)
# 
# 
# n <- 24
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# col=sample(col_vector, n)
# 
# 
# for (i in 1:ncol(ozone_data_no.mda8)){
#   plot(x=ozone_data_no.mda8[,i],
#        y=ozone_data_mda8_first$mda8,
#        main = paste0('Figure ',i,': MDA8 vs. ',colnames(ozone_data_no.mda8)[i]),
#        xlab = paste0(colnames(ozone_data_no.mda8)[i]),
#        ylab = "MDA8 Value",
#        pch = 19)
#   abline(ozone_data_mda8_first,lm(reformulate(paste0(names(ozone_data_no.mda8[i])),"mda8")),
#          col = col[i],
#          lwd = 2)
# }
# 
# ### (b) Splitting the data
# sample_size = floor(0.50 * nrow(ozone_data_mda8_first))
# set.seed(09111997)
# split_dat = sample(seq_len(nrow(ozone_data_mda8_first)), size = sample_size, replace=FALSE)
# 
# ozone_train = ozone_data_mda8_first[split_dat, ]
# ozone_test = ozone_data_mda8_first[-split_dat, ]
# 
# lm.final = glm(mda8~., data = ozone_train)
# pred.vals = predict(lm.final, ozone_test)
# summary(lm.final)
# 
# ### (c) Fitting a regression tree
# fit.tree = rpart(medv~.,data=boston_train)
# #summary(fit.tree)
# par(xpd = NA)
# plot(fit.tree)
# text(fit.tree)
# pred.tree = predict(fit.tree,newdata=boston_test)
# 
# ### (d) Fitting a bagged tree
# set.seed(09111997)
# pred.boot = ranger(medv~.,data=boston_train,mtry=dim(boston_train)[2]-1,num.trees=500)
# pred.bag = predict(pred.boot,data=boston_test)$predictions
# imp_feats = ranger(medv~.,data=boston_train,probability=TRUE,importance="impurity_corrected", mtry=dim(boston_train)[2]-1,num.trees=500)
# cbind(sort(importance(imp_feats)))
# 
# ### (e) Random Forest
# set.seed(0911997)
# 
# fit.rf = ranger(medv~.,data=boston_train, num.trees = 500)
# pred.rf = predict(fit.rf,data=boston_test)
# pred.rf = pred.rf$predictions
# 
# imp_feats2 = ranger(medv~.,data=boston_train,probability=TRUE,importance="impurity_corrected", num.trees = 500)
# cbind(sort(importance(imp_feats2)))