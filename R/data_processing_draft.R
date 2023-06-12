# This is for downloading and processing the .nc files from GridMET: https://www.climatologylab.org/gridmet.html
# I used the second option '2. Create "wget script" -> might take this out

# Carful when running this, the reprojeciton takes a long time. 

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
co_data_list = list.files("final_data/")
# Random Forest Variables
# Spatial Variables:

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
add_road_buffer = read_csv("final_data/roads_in_500m_buffer.csv")[,c(2,4)] %>% 
  filter(site_name != "Aspen Park" & site_name != "Evergreen" & site_name !="Welch")
names(add_road_buffer) = c("site_name","road_length")
add_road_buffer$road_length=ifelse(is.na(add_road_buffer$road_length),0,add_road_buffer$road_length)
o3_projected = merge(x=o3_projected,y=add_road_buffer,by="site_name")

# elevation
elevation_to_add = raster("final_data/elevation.tiff")
elevation_projected = raster::projectRaster(elevation_to_add, crs=prg)
o3_projected$elev=round(raster::extract(elevation_projected,o3_projected),2)

#data frame creation
year_o3 = as.data.frame(o3_projected) %>% 
  dplyr::select(c("site_name","elev","dist2road","road_length","lat","long"),everything())

# use to create dataframe of specific months, ex below is summer
summer_o3 = year_o3 %>%
  dplyr::select(contains(c("site_name","lat","long","elev","dist2road","road_length","Apr","May","Jun","Jul","Aug","Sep","Oct"))) %>% 
  pivot_longer(cols = contains(c("Apr","May","Jun","Jul","Aug","Sep","Oct")), names_to = "date", values_to = "mda8")
# preview data
#summer_o3

# lat 
summer_o3$lat

# long
summer_o3$long

# Temporal Variables: -> Can't do dummy variables until pivoted? (easiest method off top of head)
# monthly dummy variable -
# make a 1 for each month of interest and a 0 for other months
# yearly dummy variable -
# make a 1 for each year of interest and a 0 for other years

# Spatio-Temporal Variables:
new_path = "final_data/Monthly_Averages/"
monthly_path = list.files(paste0(new_path))
max_rh_files=grep("rmax_",monthly_path, value = T)
max_temp_files=grep("tmmx_",monthly_path, value = T)
max_precip_files=grep("pr_",monthly_path, value = T)
renameing_convention = c(paste0(month.abb,".",2017),
                         paste0(month.abb,".",2018),
                         paste0(month.abb,".",2019),
                         paste0(month.abb,".",2020),
                         paste0(month.abb,".",2021),
                         paste0(month.abb,".",2022))
coordinates(summer_o3) = c('long', 'lat')
proj4string(summer_o3) = CRS(SRS_string = prg)
AE=summer_o3[which(summer_o3$site_name=="Aurora East"),]
BR=summer_o3[which(summer_o3$site_name=="Boulder Reservoir"),]
DC=summer_o3[which(summer_o3$site_name=="Denver - Camp"),]
HR=summer_o3[which(summer_o3$site_name=="Highland Reservoir"),]
LA=summer_o3[which(summer_o3$site_name=="La Casa"),]
NREL=summer_o3[which(summer_o3$site_name=="National Renewable Energy Labs - Nrel"),]
RF=summer_o3[which(summer_o3$site_name=="Rocky Flats-N"),]
WY=summer_o3[which(summer_o3$site_name=="Welby"),]

ndvi_sort_2018=summer_o3[grep(".2018",summer_o3$date),c("site_name","date")]
ndvi_sort_2019=summer_o3[grep(".2019",summer_o3$date),c("site_name","date")]
ndvi_sort_2020=summer_o3[grep(".2020",summer_o3$date),c("site_name","date")]
ndvi_sort_2021=summer_o3[grep(".2021",summer_o3$date),c("site_name","date")]
ndvi_sort_2022=summer_o3[grep(".2022",summer_o3$date),c("site_name","date")]

################################################# Monthly total precip

max_precip_to_add = stack(paste0(new_path,max_precip_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
#plots
# plot(max_precip_to_add$tmmx_2017_monthly_avg_1.1, main="Precipitation for Jan 2017")
max_precip_projected = raster::projectRaster(max_precip_to_add, crs=prg)
names(max_precip_projected) = renameing_convention
#plots: after running names() (above code)
# plot(max_precip_projected$Jan.2017, main="Precipitation for Jan 2017")
summer_max_precip =max_precip_projected[[c(grep("2018", names(max_precip_projected)),
                                           grep("2019", names(max_precip_projected)),
                                           grep("2020", names(max_precip_projected)),
                                           grep("2021", names(max_precip_projected)),
                                           grep("2022", names(max_precip_projected)))]]
summer_max_precip =summer_max_precip[[c(grep("Apr", names(summer_max_precip)),
                                        grep("May", names(summer_max_precip)),
                                        grep("Jun", names(summer_max_precip)),
                                        grep("Jul", names(summer_max_precip)),
                                        grep("Aug", names(summer_max_precip)),
                                        grep("Sep", names(summer_max_precip)),
                                        grep("Oct", names(summer_max_precip)))]]

################################################## Monthly max temp -> rounded K to F formula just in case: 1.8*(K-273) + 32

max_temperature_to_add = stack(paste0(new_path,max_temp_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
#plots
# plot(max_temperature_to_add$tmmx_2017_monthly_avg_1.1, main="Temperature for Jan 2017")

max_temperature_projected = raster::projectRaster(max_temperature_to_add, crs=prg)
names(max_temperature_projected) = renameing_convention
#plots: after running names() (above code)
# plot(max_temperature_projected$Jan.2017, main="Temperature for Jan 2017")
summer_max_temps =max_temperature_projected[[c(grep("2018", names(max_temperature_projected)),
                                               grep("2019", names(max_temperature_projected)),
                                               grep("2020", names(max_temperature_projected)),
                                               grep("2021", names(max_temperature_projected)),
                                               grep("2022", names(max_temperature_projected)))]]
summer_max_temps =summer_max_temps[[c(grep("Apr", names(summer_max_temps)),
                                      grep("May", names(summer_max_temps)),
                                      grep("Jun", names(summer_max_temps)),
                                      grep("Jul", names(summer_max_temps)),
                                      grep("Aug", names(summer_max_temps)),
                                      grep("Sep", names(summer_max_temps)),
                                      grep("Oct", names(summer_max_temps)))]]

##################################################  Monthly RH 

max_rh_to_add = stack(paste0(new_path,max_rh_files)) # create stack of raster bricks (each "stack" is a year, each "brick" is a month)
#plots
# plot(max_rh_to_add$tmmx_2017_monthly_avg_1.1, main="Relative Humidity for Jan 2017")
max_rh_projected = raster::projectRaster(max_rh_to_add, crs=prg)
names(max_rh_projected) = renameing_convention
#plots: after running names() (above code)
# plot(max_rh_projected$Jan.2017, main="Relative Humidity for Jan 2017")
summer_max_rh =max_rh_projected[[c(grep("2018", names(max_rh_projected)),
                                   grep("2019", names(max_rh_projected)),
                                   grep("2020", names(max_rh_projected)),
                                   grep("2021", names(max_rh_projected)),
                                   grep("2022", names(max_rh_projected)))]]
summer_max_rh =summer_max_rh[[c(grep("Apr", names(summer_max_rh)),
                                grep("May", names(summer_max_rh)),
                                grep("Jun", names(summer_max_rh)),
                                grep("Jul", names(summer_max_rh)),
                                grep("Aug", names(summer_max_rh)),
                                grep("Sep", names(summer_max_rh)),
                                grep("Oct", names(summer_max_rh)))]]

################################################## yearly NDVI - Average NDVI 500m buffer - run comments if ndvi is not projected
# ndvi_folder = "NDVIs"
# ndvi_path = list.dirs(paste0("final_data/",ndvi_folder))[-1]
# ndvi_files = paste0(ndvi_path,"/den_CO_NDVI_",2018:2022,".tif")
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
ndvi_folder = "NDVIs"
ndvi_path = list.dirs(paste0("final_data/",ndvi_folder))[-1]
ndvi_files = paste0(ndvi_path,"/den_CO_NDVI_",2018:2022,".tif")
ndvi_2018_projected = raster(paste0(ndvi_path[1],"/co_ndvi_",2018,"projected.tif"))
ndvi_2019_projected = raster(paste0(ndvi_path[2],"/co_ndvi_",2019,"projected.tif"))
ndvi_2020_projected = raster(paste0(ndvi_path[3],"/co_ndvi_",2020,"projected.tif"))
ndvi_2021_projected = raster(paste0(ndvi_path[4],"/co_ndvi_",2021,"projected.tif"))
ndvi_2022_projected = raster(paste0(ndvi_path[5],"/co_ndvi_",2022,"projected.tif"))

# Replacing values less than 0 - need new value
values(ndvi_2018_projected)=ifelse(values(ndvi_2018_projected)<0,0,values(ndvi_2018_projected))
values(ndvi_2019_projected)=ifelse(values(ndvi_2019_projected)<0,0,values(ndvi_2019_projected))
values(ndvi_2020_projected)=ifelse(values(ndvi_2020_projected)<0,0,values(ndvi_2020_projected))
values(ndvi_2021_projected)=ifelse(values(ndvi_2021_projected)<0,0,values(ndvi_2021_projected))
values(ndvi_2022_projected)=ifelse(values(ndvi_2022_projected)<0,0,values(ndvi_2022_projected))

ndvi_sort_2018$ndvi = raster::extract(ndvi_2018_projected,ndvi_sort_2018)
ndvi_sort_2019$ndvi = raster::extract(ndvi_2019_projected,ndvi_sort_2019)
ndvi_sort_2020$ndvi = raster::extract(ndvi_2020_projected,ndvi_sort_2020)
ndvi_sort_2021$ndvi = raster::extract(ndvi_2021_projected,ndvi_sort_2021)
ndvi_sort_2022$ndvi = raster::extract(ndvi_2022_projected,ndvi_sort_2022)
ndvi_to_final_dataframe = as.data.frame(rbind(ndvi_sort_2018,ndvi_sort_2019,ndvi_sort_2020,ndvi_sort_2021,ndvi_sort_2022)) %>% 
  select(-lat,-long)
#plot
# plot(ndvi_2018_projected)
################################################## Exposure Assignment
for(i in 1:nrow(AE)) {
  AE$tmax[i] = raster::extract(summer_max_temps[[i]],AE[i,])
  AE$rhmax[i] = raster::extract(summer_max_rh[[i]],AE[i,])
  AE$pmax[i] = raster::extract(summer_max_precip[[i]],AE[i,])
}
for(i in 1:nrow(BR)) {
  BR$tmax[i] = raster::extract(summer_max_temps[[i]],BR[i,])
  BR$rhmax[i] = raster::extract(summer_max_rh[[i]],BR[i,])
  BR$pmax[i] = raster::extract(summer_max_precip[[i]],BR[i,])
}
for(i in 1:nrow(DC)) {
  DC$tmax[i] = raster::extract(summer_max_temps[[i]],DC[i,])
  DC$rhmax[i] = raster::extract(summer_max_rh[[i]],DC[i,])
  DC$pmax[i] = raster::extract(summer_max_precip[[i]],DC[i,])
}
for(i in 1:nrow(HR)) {
  HR$tmax[i] = raster::extract(summer_max_temps[[i]],HR[i,])
  HR$rhmax[i] = raster::extract(summer_max_rh[[i]],HR[i,])
  HR$pmax[i] = raster::extract(summer_max_precip[[i]],HR[i,])
}
for(i in 1:nrow(LA)) {
  LA$tmax[i] = raster::extract(summer_max_temps[[i]],LA[i,])
  LA$rhmax[i] = raster::extract(summer_max_rh[[i]],LA[i,])
  LA$pmax[i] = raster::extract(summer_max_precip[[i]],LA[i,])
}
for(i in 1:nrow(NREL)) {
  NREL$tmax[i] = raster::extract(summer_max_temps[[i]],NREL[i,])
  NREL$rhmax[i] = raster::extract(summer_max_rh[[i]],NREL[i,])
  NREL$pmax[i] = raster::extract(summer_max_precip[[i]],NREL[i,])
}
for(i in 1:nrow(RF)) {
  RF$tmax[i] = raster::extract(summer_max_temps[[i]],RF[i,])
  RF$rhmax[i] = raster::extract(summer_max_rh[[i]],RF[i,])
  RF$pmax[i] = raster::extract(summer_max_precip[[i]],RF[i,])
}
for(i in 1:nrow(WY)) {
  WY$tmax[i] = raster::extract(summer_max_temps[[i]],WY[i,])
  WY$rhmax[i] = raster::extract(summer_max_rh[[i]],WY[i,])
  WY$pmax[i] = raster::extract(summer_max_precip[[i]],WY[i,])
}
ggs = as.data.frame(rbind(AE,BR,DC,HR,LA,NREL,RF,WY))
merge(ndvi_to_final_dataframe, ggs, by=c("site_name", "date")) %>%
  group_by(site_name) %>% 
  slice_head(n=1)

################################################## Dummy Variables:

# monthly dummy variable -
# make a 1 for each month of interest and a 0 for other months
rough_variables = merge(ndvi_to_final_dataframe, ggs, by=c("site_name", "date"))
rough_variables$apr_dummy = ifelse(str_detect(rough_variables$date, "Apr."),1,0)
rough_variables$may_dummy = ifelse(str_detect(rough_variables$date, "May."),1,0)
rough_variables$jun_dummy = ifelse(str_detect(rough_variables$date, "Jun."),1,0)
rough_variables$jul_dummy = ifelse(str_detect(rough_variables$date, "Jul."),1,0)
rough_variables$aug_dummy = ifelse(str_detect(rough_variables$date, "Aug."),1,0)
rough_variables$sep_dummy = ifelse(str_detect(rough_variables$date, "Sep."),1,0)
rough_variables$oct_dummy = ifelse(str_detect(rough_variables$date, "Oct."),1,0)
# yearly dummy variable -
# make a 1 for each year of interest and a 0 for other years
rough_variables$yr_2018_dummy = ifelse(str_detect(rough_variables$date, ".2018"),1,0)
rough_variables$yr_2019_dummy = ifelse(str_detect(rough_variables$date, ".2019"),1,0)
rough_variables$yr_2020_dummy = ifelse(str_detect(rough_variables$date, ".2020"),1,0)
rough_variables$yr_2021_dummy = ifelse(str_detect(rough_variables$date, ".2021"),1,0)
rough_variables$yr_2022_dummy = ifelse(str_detect(rough_variables$date, ".2022"),1,0)
rough_variables=rough_variables %>% 
  dplyr::select(site_name,date,lat,long,mda8,everything())
write.csv(rough_variables,"final_data/ozone_data.csv", overwrite=TRUE)
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

##########################################################################################################################################

############## FROM RMD ->>>>>>> COPY THIS BACK IN THE RMD AFTER UNCOMMENTING

##########################################################################################################################################

# ```