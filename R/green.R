# Author: Yanni Zhan
# Date  : 03/30/2017
# Sentinel 2 Greenest NDVI selecting script


rm(list=ls())
source("/data3/rstudio/sentinel2/green_function.R")


## parameters
input_dir="/data2/sentinel2/download" #the directory that includes the unzipped sentinel 2 data in a given year
output_dir="/data2/sentinel2/analysis" #the directory you want to save the output result

## GRASS settings
gisBase="/usr/lib/grass64"
gisDbase="/data3/grassdata"
mapset="sen2"

## clip map
base_map="/data2/gadm2/countries/GHA_100m_LAEA.tif"





##### resample to 100m

t1=system.time(jp2_to_100mtif(input_dir,output_dir))
t1


##### select using max NDVI

t2=system.time(greeness(output_dir))
t2



##### mosaic same utm zone & reproject to lambert

t3=system.time(mosaic_utm(output_dir))
t3



##### mosaic whole country & clip & rescale

t4=system.time(mosaic_country_rescale(output_dir,gisBase,gisDbase,mapset,base_map))
t4


