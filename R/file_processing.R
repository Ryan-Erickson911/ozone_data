source("R/ozone_krige.R")

path_start = "data/" #change as needed - I set my project directory to the same folder as the data
path_final = "tiffs/"
path_cropped = "final_data/"

#Checking to make sure folders exist - creating folders if necessary
# download.file("URL", "dest", mode = 'wb')
ifelse(dir.exists(path_start),"Data folder is present",
       "You need to download the data from GridMET and move it into the 'data' folder.")
ifelse(dir.exists(path_final), "Folder to save files to already exists, proceeding...", dir.create(path_final))
ifelse(dir.exists(paste0(path_final,"Daily/")), "Folder to save Daily files to already exists, proceeding...", dir.create(paste0(path_final,"Daily/")))
ifelse(dir.exists(paste0(path_final,"Monthly/")), "Folder to save Monthly files to already exists, proceeding...", dir.create(paste0(path_final,"Monthly/")))
ifelse(dir.exists(paste0(path_final,"Yearly/")), "Folder to save Yearly files to already exists, proceeding...", dir.create(paste0(path_final,"Yearly/")))
ifelse(dir.exists(paste0(path_cropped,"Daily_Averages/")), "Folder to save final Daily files to already exists, proceeding...", dir.create(paste0(path_cropped,"Daily/")))
ifelse(dir.exists(paste0(path_cropped,"Monthly_Averages/")), "Folder to save final Monthly files to already exists, proceeding...", dir.create(paste0(path_cropped,"Monthly/")))
ifelse(dir.exists(paste0(path_cropped,"Yearly_Averages/")), "Folder to save final Yearly files to already exists, proceeding...", dir.create(paste0(path_cropped,"Yearly/")))
ifelse(dir.exists(path_cropped), "Folder to save cropped files to already exists, proceeding...", dir.create(path_cropped))

#Data download
dl_data = function(var_name,years){
  NKN_site = "http://thredds.northwestknowledge.net:8080/thredds/fileServer/MET/"
  website = paste0(NKN_site,var_name,"/",var_name,"_",years,".nc")
  file_names = sub(paste0(".*/MET/",var_name,"/"), "", website) 
  for(i in file_names){
    ifelse(file.exists(paste0(path_start,i)),paste0(i," exists,  skipping..."),download.file(paste0(NKN_site,var_name,"/",i), destfile = paste0(path_start,i), method="curl"))
  }
  file_names
}

# Input Files
temp_averages = dl_data("tmmx",2018:2022)
rh_averages = dl_data("rmax",2018:2022)
vpd_averages = dl_data("vpd",2018:2022)
sums = dl_data("pr",2018:2022)
# rh_now = grep("rmax_",input_nc, value=TRUE)
# modes = grep("th_",input_nc, value=TRUE)
#Mapping Shapefile
usab_prj="EPSG:4326"
co_bound = st_as_sf(USAboundaries::us_states(states="Colorado"), crs=raster::crs(usab_prj))

#Averages
for (x in temp_averages){
  ncfile =  ncdf4::nc_open(paste0(path_start,x))
  varnames = format(as.Date(ncfile$dim$day$vals, origin=as.Date("1900-01-01")),"%B/%d/%Y")
  nc2raster = stack(paste0(path_start,x))
  names(nc2raster) = varnames
  nm = gsub(".nc","",x)
  # daily = paste0(path_final,"Daily/",nm,"_daily.tiff")
  monthly = paste0(path_final,"Monthly/",nm,"_monthly_avg.tiff")
  yearly = paste0(path_final,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(nc2raster,daily,format = 'GTiff',overwrite = TRUE)
  
  # crop_daily = crop(nc2raster,co_bound)
  # output = paste0(path_cropped,"Daily/",nm,".tiff")
  # writeRaster(crop,output,format = 'GTiff',overwrite = TRUE)
  
  jan_average=mean(nc2raster[[grep("Jan",names(nc2raster))]])
  feb_average=mean(nc2raster[[grep("Feb",names(nc2raster))]])
  mar_average=mean(nc2raster[[grep("Mar",names(nc2raster))]])
  apr_average=mean(nc2raster[[grep("Apr",names(nc2raster))]])
  may_average=mean(nc2raster[[grep("May",names(nc2raster))]])
  jun_average=mean(nc2raster[[grep("Jun",names(nc2raster))]])
  jul_average=mean(nc2raster[[grep("Jul",names(nc2raster))]])
  aug_average=mean(nc2raster[[grep("Aug",names(nc2raster))]])
  sep_average=mean(nc2raster[[grep("Sep",names(nc2raster))]])
  oct_average=mean(nc2raster[[grep("Oct",names(nc2raster))]])
  nov_average=mean(nc2raster[[grep("Nov",names(nc2raster))]])
  dec_average=mean(nc2raster[[grep("Dec",names(nc2raster))]])

  stacked_months=brick(c(jan_average,feb_average,mar_average,apr_average,may_average,jun_average,jul_average,aug_average,sep_average,oct_average,nov_average,dec_average))
  names(stacked_months) = paste0(month.abb,"_avg")
  writeRaster(stacked_months, monthly, format = 'GTiff', overwrite = TRUE)

  crop_monthly = crop(stacked_months,co_bound)
  output_mo = paste0(path_cropped,"Monthly_Averages/",nm,"_monthly_avg.tiff")
  writeRaster(crop_monthly,output_mo,format = 'GTiff',overwrite = TRUE)

  # yearly_average=mean(nc2raster)
  # writeRaster(yearly_average,yearly,format = 'GTiff',overwrite = TRUE)
  # 
  # crop_yearly = crop(yearly_average,co_bound)
  # output_yr = paste0(path_cropped,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(crop_yearly,output_yr,format = 'GTiff',overwrite = TRUE)
}

for (x in rh_averages){
  ncfile =  ncdf4::nc_open(paste0(path_start,x))
  varnames = format(as.Date(ncfile$dim$day$vals, origin=as.Date("1900-01-01")),"%B/%d/%Y")
  nc2raster = stack(paste0(path_start,x))
  names(nc2raster) = varnames
  nm = gsub(".nc","",x)
  # daily = paste0(path_final,"Daily/",nm,"_daily.tiff")
  monthly = paste0(path_final,"Monthly/",nm,"_monthly_avg.tiff")
  yearly = paste0(path_final,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(nc2raster,daily,format = 'GTiff',overwrite = TRUE)
  
  # crop_daily = crop(nc2raster,co_bound)
  # output = paste0(path_cropped,"Daily/",nm,".tiff")
  # writeRaster(crop,output,format = 'GTiff',overwrite = TRUE)
  
  jan_average=mean(nc2raster[[grep("Jan",names(nc2raster))]])
  feb_average=mean(nc2raster[[grep("Feb",names(nc2raster))]])
  mar_average=mean(nc2raster[[grep("Mar",names(nc2raster))]])
  apr_average=mean(nc2raster[[grep("Apr",names(nc2raster))]])
  may_average=mean(nc2raster[[grep("May",names(nc2raster))]])
  jun_average=mean(nc2raster[[grep("Jun",names(nc2raster))]])
  jul_average=mean(nc2raster[[grep("Jul",names(nc2raster))]])
  aug_average=mean(nc2raster[[grep("Aug",names(nc2raster))]])
  sep_average=mean(nc2raster[[grep("Sep",names(nc2raster))]])
  oct_average=mean(nc2raster[[grep("Oct",names(nc2raster))]])
  nov_average=mean(nc2raster[[grep("Nov",names(nc2raster))]])
  dec_average=mean(nc2raster[[grep("Dec",names(nc2raster))]])
  
  stacked_months=brick(c(jan_average,feb_average,mar_average,apr_average,may_average,jun_average,jul_average,aug_average,sep_average,oct_average,nov_average,dec_average))
  names(stacked_months) = paste0(month.abb,"_avg")
  writeRaster(stacked_months, monthly, format = 'GTiff', overwrite = TRUE)
  
  crop_monthly = crop(stacked_months,co_bound)
  output_mo = paste0(path_cropped,"Monthly_Averages/",nm,"_monthly_avg.tiff")
  writeRaster(crop_monthly,output_mo,format = 'GTiff',overwrite = TRUE)
  
  # yearly_average=mean(nc2raster)
  # writeRaster(yearly_average,yearly,format = 'GTiff',overwrite = TRUE)
  # 
  # crop_yearly = crop(yearly_average,co_bound)
  # output_yr = paste0(path_cropped,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(crop_yearly,output_yr,format = 'GTiff',overwrite = TRUE)
}

for (x in vpd_averages){
  ncfile =  ncdf4::nc_open(paste0(path_start,x))
  varnames = format(as.Date(ncfile$dim$day$vals, origin=as.Date("1900-01-01")),"%B/%d/%Y")
  nc2raster = stack(paste0(path_start,x))
  names(nc2raster) = varnames
  nm = gsub(".nc","",x)
  # daily = paste0(path_final,"Daily/",nm,"_daily.tiff")
  monthly = paste0(path_final,"Monthly/",nm,"_monthly_avg.tiff")
  yearly = paste0(path_final,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(nc2raster,daily,format = 'GTiff',overwrite = TRUE)
  
  # crop_daily = crop(nc2raster,co_bound)
  # output = paste0(path_cropped,"Daily/",nm,".tiff")
  # writeRaster(crop,output,format = 'GTiff',overwrite = TRUE)
  
  jan_average=mean(nc2raster[[grep("Jan",names(nc2raster))]])
  feb_average=mean(nc2raster[[grep("Feb",names(nc2raster))]])
  mar_average=mean(nc2raster[[grep("Mar",names(nc2raster))]])
  apr_average=mean(nc2raster[[grep("Apr",names(nc2raster))]])
  may_average=mean(nc2raster[[grep("May",names(nc2raster))]])
  jun_average=mean(nc2raster[[grep("Jun",names(nc2raster))]])
  jul_average=mean(nc2raster[[grep("Jul",names(nc2raster))]])
  aug_average=mean(nc2raster[[grep("Aug",names(nc2raster))]])
  sep_average=mean(nc2raster[[grep("Sep",names(nc2raster))]])
  oct_average=mean(nc2raster[[grep("Oct",names(nc2raster))]])
  nov_average=mean(nc2raster[[grep("Nov",names(nc2raster))]])
  dec_average=mean(nc2raster[[grep("Dec",names(nc2raster))]])
  
  stacked_months=brick(c(jan_average,feb_average,mar_average,apr_average,may_average,jun_average,jul_average,aug_average,sep_average,oct_average,nov_average,dec_average))
  names(stacked_months) = paste0(month.abb,"_avg")
  writeRaster(stacked_months, monthly, format = 'GTiff', overwrite = TRUE)
  
  crop_monthly = crop(stacked_months,co_bound)
  output_mo = paste0(path_cropped,"Monthly_Averages/",nm,"_monthly_avg.tiff")
  writeRaster(crop_monthly,output_mo,format = 'GTiff',overwrite = TRUE)
  
  # yearly_average=mean(nc2raster)
  # writeRaster(yearly_average,yearly,format = 'GTiff',overwrite = TRUE)
  # 
  # crop_yearly = crop(yearly_average,co_bound)
  # output_yr = paste0(path_cropped,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(crop_yearly,output_yr,format = 'GTiff',overwrite = TRUE)
}

for (x in sums){
  ncfile =  ncdf4::nc_open(paste0(path_start,x))
  varnames = format(as.Date(ncfile$dim$day$vals, origin=as.Date("1900-01-01")),"%B/%d/%Y")
  nc2raster = stack(paste0(path_start,x))
  names(nc2raster) = varnames
  nm = gsub(".nc","",x)
  # daily = paste0(path_final,"Daily/",nm,"_daily.tiff")
  monthly = paste0(path_final,"Monthly/",nm,"_monthly_avg.tiff")
  yearly = paste0(path_final,"Yearly/",nm,"_yearly_avg.tiff")
  # writeRaster(nc2raster,daily,format = 'GTiff',overwrite = TRUE)
  
  # crop_daily = crop(nc2raster,co_bound)
  # output = paste0(path_cropped,"Daily/",nm,".tiff")
  # writeRaster(crop,output,format = 'GTiff',overwrite = TRUE)
  
  jan_sum=sum(nc2raster[[grep("Jan",names(nc2raster))]])
  feb_sum=sum(nc2raster[[grep("Feb",names(nc2raster))]])
  mar_sum=sum(nc2raster[[grep("Mar",names(nc2raster))]])
  apr_sum=sum(nc2raster[[grep("Apr",names(nc2raster))]])
  may_sum=sum(nc2raster[[grep("May",names(nc2raster))]])
  jun_sum=sum(nc2raster[[grep("Jun",names(nc2raster))]])
  jul_sum=sum(nc2raster[[grep("Jul",names(nc2raster))]])
  aug_sum=sum(nc2raster[[grep("Aug",names(nc2raster))]])
  sep_sum=sum(nc2raster[[grep("Sep",names(nc2raster))]])
  oct_sum=sum(nc2raster[[grep("Oct",names(nc2raster))]])
  nov_sum=sum(nc2raster[[grep("Nov",names(nc2raster))]])
  dec_sum=sum(nc2raster[[grep("Dec",names(nc2raster))]])
  
  stacked_months=brick(c(jan_sum,feb_sum,mar_sum,apr_sum,may_sum,jun_sum,jul_sum,aug_sum,sep_sum,oct_sum,nov_sum,dec_sum))
  names(stacked_months) = paste0(month.abb,"_sum")
  writeRaster(stacked_months, monthly, format = 'GTiff', overwrite = TRUE)
  
  crop_monthly = crop(stacked_months,co_bound)
  output_mo = paste0(path_cropped,"Monthly_Averages/",nm,"_monthly_sum.tiff")
  writeRaster(crop_monthly,output_mo,format = 'GTiff',overwrite = TRUE)
}

# Modes

### Need mode function that works for raster stacks - not worrying about mode for now 

# for (y in modes){
#   ncfile =  ncdf4::nc_open(paste0(path_start,y))
#   varnames = format(as.Date(ncfile$dim$day$vals, origin=as.Date("1900-01-01")),"%b.%d.%Y")
#   nc2raster = stack(paste0(path_start,y))
#   names(nc2raster) = varnames
#   nm = gsub(".nc","",y)
#   nm = gsub("th","wind_direction",nm)
#   daily = paste0(path_final,"Daily/",nm,"_daily.tiff")
#   monthly = paste0(path_final,"Monthly/",nm,"_monthly_mode.tiff")
#   writeRaster(nc2raster,daily,format = 'GTiff',overwrite = TRUE)
#   
#   jan_mode=mode(nc2raster[[grep("Jan",names(nc2raster))]])
#   feb_mode=mode(nc2raster[[grep("Feb",names(nc2raster))]])
#   mar_mode=mode(nc2raster[[grep("Mar",names(nc2raster))]])
#   apr_mode=mode(nc2raster[[grep("Apr",names(nc2raster))]])
#   may_mode=mode(nc2raster[[grep("May",names(nc2raster))]])
#   jun_mode=mode(nc2raster[[grep("Jun",names(nc2raster))]])
#   jul_mode=mode(nc2raster[[grep("Jul",names(nc2raster))]])
#   aug_mode=mode(nc2raster[[grep("Aug",names(nc2raster))]])
#   sep_mode=mode(nc2raster[[grep("Sep",names(nc2raster))]])
#   oct_mode=mode(nc2raster[[grep("Oct",names(nc2raster))]])
#   nov_mode=mode(nc2raster[[grep("Nov",names(nc2raster))]])
#   dec_mode=mode(nc2raster[[grep("Dec",names(nc2raster))]])
#   stacked_months(c(jan_mode,feb_mode,mar_mode,apr_mode,may_mode,jun_mode,jul_mode,aug_mode,sep_mode,oct_mode,nov_mode,dec_mode))
#   names(stacked_months) = paste0(month.abb,"_avg")
#   writeRaster(stacked_months,monthly,format = 'GTiff',overwrite = TRUE)
# }

# Quick checks
# test = raster(paste0(path_final,list.files(paste0(path_final)))[1])
# plot(test)
# test2 = raster(paste0(path_cropped,list.files(path_cropped)[11]))
# plot(test2, main="Mean Temperature for CO (Celsius): April 2018")
# 
# plot(added_data_projected, main="Maximum Temperature")
# plot(o3_projected, add=TRUE)
# plot(den_projected, add=TRUE)
# 
# plot(added_data_projected1, main="Elevation")
# plot(o3_projected, add=TRUE)
# plot(den_projected, add=TRUE)
# 
# plot(roads_projected, main = "Roads Network")
# plot(den_projected, col="red", add=TRUE)
# plot(o3_projected, col="green", add=TRUE)