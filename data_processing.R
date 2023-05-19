# This is for downloading and processing the .nc files from GridMET: https://www.climatologylab.org/gridmet.html
# I used the second option '2. Create wget script for downloading NetCDF files' for this: https://www.climatologylab.org/wget-gridmet.html
library(raster)
library(rasterVis)
library(ncdf4)
library(lattice)
library(stringr)
library(raster)
library(sf)
library(dplyr)

# Necessary Folders
path_start = "data/" #change as needed - I set my project directory to the same folder as the data
path_final = "tiffs/"
path_cropped = "final_data/"

#Checking to make sure folders exist - creating folders if necessary
ifelse(dir.exists(path_start),"Data folder is present",
       "You need to download the data from GridMET and move it into the 'data' folder.")
ifelse(dir.exists(path_final), "Folder to save files to exists, proceeding...", dir.create(path_final))
ifelse(dir.exists(path_cropped), "Folder to save cropped files to exists, proceeding...", dir.create(path_cropped))

# Input Files
input_nc = list.files(path_start) # making 14 varialbes so there are 14 NetCDFs
#Mapping Shapefile
prj="EPSG:4326"
co_bound = st_as_sf(USAboundaries::us_states(states="Colorado"), crs=raster::crs(prj))

for (x in input_nc){
  ncfile =  ncdf4::nc_open(paste0(path_start,x))
  varname = names(ncfile$var)
  nc2raster = stack(raster(paste0(path_start,x),varname = varname,band = 1))
  nm = gsub(".nc","",x)
  output = paste0(path_final,nm,".tiff")
  writeRaster(nc2raster,output,format = 'GTiff',overwrite = TRUE)
}

input_tiffs = list.files(path_final)
for (x in input_tiffs) {
  r = raster(paste0(path_final,x))
  crop = crop(r,co_bound)
  nm = gsub(".tiff","",x)
  output = paste0(path_cropped,nm,".tiff")
  writeRaster(crop,output,format = 'GTiff',overwrite = TRUE)
}

# Temperature Conversion - 12 layer NC file
# download from: http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/data/catalog.html

folder_name = "TerraClim_tmax_2017-2022/" #name of the folder you moved the terra climate data into
final_folder_name = "temperatures/" #name of the folder you moved the terra climate data into
input_temp_ncs = list.files(paste0(path_start,folder_name)) # making 14 variables so there are 14 NetCDFs

for (x in input_temp_ncs){
  ncfile =  ncdf4::nc_open(paste0(path_start,folder_name,x))
  varnames=format(as.Date(ncfile$dim$time$val, origin=as.Date("1900-01-01")),"%b.%Y")
  nc2raster=stack(brick(paste0(path_start,folder_name,x)))
  names(nc2raster)=varnames
  nm = gsub(".nc","",x)
  output = paste0(path_final,final_folder_name,nm,".tiff")
  writeRaster(nc2raster,output,format = 'GTiff',overwrite = TRUE)
}

input_tiffs = list.files(paste0(path_final,final_folder_name))
for (x in input_tiffs) {
  r = raster(paste0(path_final,final_folder_name,x))
  crop = crop(r,co_bound)
  nm = gsub(".tiff","",x)
  output = paste0(path_cropped,nm,".tiff")
  writeRaster(crop,output,format = 'GTiff',overwrite = TRUE)
}

# A quick check
test = raster(paste0(path_final,final_folder_name,list.files(paste0(path_final,final_folder_name)))[1])
plot(test)
test2 = raster(paste0(path_cropped,list.files(path_cropped)[11]))
plot(test2, main="Mean Temperature for CO (Celsius): April 2018")
