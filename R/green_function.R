# Author: Yanni Zhan
# Date  : 03/30/2017
# Sentinel 2 Greenest NDVI selecting function (improved)

require(rgdal)
require(raster)
require(doParallel)
require(spgrass6)
require(stringr)


##########

raws_100m_dir_create=function(input_dir,output_dir){
  
  setwd(input_dir)
  
  #create /output_dir/raws_100m
  dir.create(paste(output_dir,"/raws_100m",sep=""),mode="0775",showWarnings=T)
  system(command=paste("chmod -R 2775",paste(output_dir,"/raws_100m",sep="")))
  
  #list all the scenes in the input_dir
  folder_list=list.files(pattern="*.SAFE",full.names=T)
  #folder_list
  
  #list all the tiles inside the GRANULE folder
  tile_list=list.files(path=paste(folder_list,"/GRANULE",sep=""),full.names=T)
  tile_list
  
  tile_sep=strsplit(tile_list,"[_/]+")
  tile_sep

  toMatch2="^T[[:digit:]][[:digit:]][[:upper:]][[:upper:]][[:upper:]]$"
  
  #op_folder=sapply(val_sep,function(x ) unique(grep(paste(toMatch2,collapse="|"),x,value=T)))
  tile_folder=sapply(tile_sep,function(x ) unique(grep(toMatch2,x,value=T)))
  tile_folder=(substring(tile_folder,2,length(tile_folder)))
  tile=levels(as.factor(tile_folder))
  tile
  
  #create detailed output folders
  lapply(paste(output_dir,"/raws_100m/",tile,sep=""),
         function(x) if(!dir.exists(x)) dir.create(x,mode="0775",showWarnings=T))
  
  raw100_band_dir=paste(output_dir,"/raws_100m/",tile,sep="")
  Bands=c("B02","B03","B04","B08","B05","B06","B07","B8A","B11","B12")
  
  lapply(as.vector(outer(raw100_band_dir, Bands, paste, sep="/")),
         function(x) if(!dir.exists(x)) dir.create(x,mode="0775",showWarnings=T))
  
}


##########

jp2_to_100mtif=function(input_dir,output_dir){
  
  #create raws_100m directoies (if do not exist)
  raws_100m_dir_create(input_dir,output_dir)
  
  setwd(input_dir)
  
  #list all the scenes in the input_dir
  folder_list=list.files(pattern="*.SAFE",full.names=T)
  #folder_list
  
  #list all the tiles inside the GRANULE folder
  tile_list=list.files(path=paste(folder_list,"/GRANULE",sep=""),full.names=T)
  #tile_list
  
  #list all available jp2 files
  band_files2=list.files(path=paste(tile_list,"/IMG_DATA",sep=""),pattern="*.jp2",full.names=T)
  #band_files2
  
  #select 10 bands
  toMatch=c("B02","B03","B04","B08","B05","B06","B07","B8A","B11","B12")
  valid=unique(grep(paste(toMatch,collapse="|"),band_files2,value=T))
  valid
  
  val_sep=strsplit(valid,"[_/]+")
  val_sep

  toMatch2="^T[[:digit:]][[:digit:]][[:upper:]][[:upper:]][[:upper:]]$"
  
  #op_folder=sapply(val_sep,function(x ) unique(grep(paste(toMatch2,collapse="|"),x,value=T)))
  op_folder=sapply(val_sep,function(x ) unique(grep(toMatch2,x,value=T)))
  op_folder=substring(op_folder,2,length(op_folder))
  op_folder
  
  ip=sapply(strsplit(valid,"/"),function(x) x[6])
  ip=substring(ip,1,nchar(ip)-4)
  ip
  
  op_folder2=substring(ip,nchar(ip)-2,nchar(ip))
  op_folder2
  
  op=paste(output_dir,"/raws_100m/",op_folder,"/",op_folder2,"/",ip,".tif",sep="")
  op

  #bulk resample to 100m
  cl1=makeCluster(detectCores())
  registerDoParallel(cl1)

  foreach( i=1:length(valid),.packages=c("rgdal","raster")  ) %dopar% {
  #for (j in 1:length(valid)) {
    system(command=paste("gdalwarp -r average -tr 100 100 -overwrite -srcnodata 0 -multi -wm 5000",
                          valid[i],
                          op[i]))
  }
  stopCluster(cl1)

}


##########

greeness=function(output_dir){

  tif_dir=paste(output_dir,"/raws_100m",sep="")
  setwd(tif_dir)
  
  #create output folders for each band
  dir.create("../green",mode="0775",showWarnings=T)
  system(command=paste("chmod -R 2775","../green"))
  
  bands=as.factor(c("B02","B03","B04","B05","B06","B07","B08","B8A","B11","B12","NDVI"))
  
  lapply(paste("../green/",bands,sep=""),
         function(x) if(!dir.exists(x)) dir.create(x,mode="0775",showWarnings=T))
  
  #list all the tiles
  tile_list=list.files(full.names=T)
  tile_list=substring(tile_list,3,length(tile_list))
  
  tile_dir=unique(substring(tile_list,1,2))
  green_band_dir=paste("../green/",bands,sep="")
  
  lapply(as.vector(outer(green_band_dir, tile_dir, paste, sep="/")),
         function(x) if(!dir.exists(x)) dir.create(x))
  
  #i=1
  #tile_list[i]
  
  #enable parallel running to selec greenest for each tile
  cl = makeCluster(detectCores())
  registerDoParallel(cl)
  
  foreach(i=1:length(tile_list) ,.packages=c("raster","doParallel") ) %dopar% {
  #for (i in 1:3){ 

    toMatch=c("B02","B03","B04","B08","B05","B06","B07","B8A","B11","B12")
    
    for (j in 1:length(toMatch)){
      #read in each band
      valid_Band=list.files(path=paste("./",tile_list[i],"/",toMatch[j],sep=""),pattern=paste("*.tif",sep=""),full.names=T,recursive=T)
      valid_Band
      
      #bulk process
      Band_ras=lapply(valid_Band,raster) 
      Band_val=lapply(Band_ras,values)
      
      #merge the list into one matrix
      new_Band=do.call(cbind,Band_val)
      
      #create new variable name
      nam = paste("new", toMatch[j], sep = "_")
      assign(nam, new_Band)
    }

    #remove all na values
    new_B04[is.na(new_B12)==T|is.na(new_B02)==T|is.na(new_B03)==T|is.na(new_B05)==T|is.na(new_B06)==T|is.na(new_B07)==T|is.na(new_B11)==T|is.na(new_B8A)==T]=NA
    
    ####NDVI
    ndvi_num=new_B08-new_B04
    ndvi_den=new_B08+new_B04
    
    new_NDVI = ndvi_num / ndvi_den
    
    #select using max.col
    max_index = max.col(replace(new_NDVI, is.na(new_NDVI), -9999), ties.method="first")
    
    bands=as.factor(c("B02","B03","B04","B05","B06","B07","B08","B8A","B11","B12","NDVI"))
    
    for (k in 1:length(bands)){
      #select highest ndvi and the corresponding values of other bands
      matrix_band=get(paste("new",bands[k],sep="_"))
      max_index_band=matrix_band[cbind(seq_along(max_index),max_index)]
      
      #put back the value
      output_band_ras=Band_ras[[1]]
      values(output_band_ras)=max_index_band
      
      #save as tif file
      op_band=paste("../green/",bands[k],"/",substring(tile_list[i],1,2),"/",tile_list[i],"_",bands[k],".tif",sep="")
      writeRaster(output_band_ras,file=op_band,format="GTiff",overwrite=T)
    }
    
  }
  stopCluster(cl)
}


##########


mosaic_utm=function(output_dir){
  green_dir=paste(output_dir,"/green",sep="")
  setwd(green_dir)
  
  dir.create("../mosaic",mode="0775",showWarnings=T)
  system(command=paste("chmod -R 2775","../mosaic"))
  
  bands=as.factor(c("B02","B03","B04","B05","B06","B07","B08","B8A","B11","B12","NDVI"))
  
  lapply(paste("../mosaic/",bands,sep=""),function(x) if(!dir.exists(x)) dir.create(x,mode="0775",showWarnings=T))
  
  mo_bands=as.factor(c("B02","B03","B04","B05","B06","B07","B08","B11","B12","B8A","NDVI"))
  mo_zone=list.files(path=paste("./",mo_bands[1],sep=""),full.names=T)
  mo_zone=substring(mo_zone,7,8)
  
  #enable parallel running
  cl0=makeCluster(3)
  registerDoParallel(cl0)
  
  foreach( k=1:length(mo_zone),.packages=c("doParallel","raster") ) %dopar% {
    
    cl3 = makeCluster(11)
    registerDoParallel(cl3)
    
    foreach( n=1:length(mo_bands),.packages=c("rgdal","raster") ) %dopar% {
      #setwd("/data3/rstudio/s2_test/")
      #n=3
      ip_mosaic=list.files(path=paste("./",mo_bands[n],"/",mo_zone[k],sep=""),pattern=".tif",full.names=TRUE)
      ip_mosaic
      ip_mosaic_ras=lapply(ip_mosaic,raster)
      ip_mosaic_ras
      
      #mosaic
      rast.list=list()
      for (m in 1:length(ip_mosaic_ras)){
        rast.list[m]=ip_mosaic_ras[m]
      }
      
      #mosaci settings
      rast.list$fun=mean
      rast.list$tolerance=50000000
      rast.list$na.rm=TRUE
      rast.mosaic=do.call(mosaic,rast.list)
      
      ###to enable gdalinfo to check the result
      rast.val=values(rast.mosaic)
      rast.val[ is.na(rast.val) == T ] = 0
      values(rast.mosaic)=rast.val
      ###
      
      writeRaster(rast.mosaic,filename=paste("../mosaic/",mo_bands[n],"/",mo_bands[n],"_",mo_zone[k],"_mosaic.tif",sep=""),overwrite=T)
      #writeRaster(rast.mosaic,filename=paste("../mosaic/",mo_bands[n],"_",mo_zone[k],"_mosaic.tif",sep=""),overwrite=T)
    }
    stopCluster(cl3)
  }
  stopCluster(cl0)
  
  
  ###reproject to laea:
  #list all the files as input
  repro_ip=list.files(path="../mosaic",pattern=paste("*.tif",sep=""),full.names=T,recursive=T)
  repro_ip
  repro_ip2=substring(repro_ip,1,nchar(repro_ip)-11)
  repro_ip2
  
  #create a new name as output
  repro_op=paste(repro_ip2,"_laea.tif",sep="")
  repro_op
  
  #bulk reproject
  cl_repro=makeCluster(11)
  registerDoParallel(cl_repro)
  
  foreach( a=1:length(repro_ip),.packages="rgdal" ) %dopar% {
    #for (a in 1:length(repro_ip)) {
    system(command= paste("gdalwarp -overwrite -t_srs '+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -multi -wm 5000",
                          repro_ip[a],
                          repro_op[a]))
    file.remove(repro_ip[a])
  }
  stopCluster(cl_repro)
  
}


##########


mosaic_country_rescale=function(output_dir,gisBase,gisDbase,mapset,base_map){
  
  setwd(paste(output_dir,"/mosaic",sep=""))
  
  #create a new folder for outputs
  dir.create("../outputs",mode="0775",showWarnings=T)
  
  #band names
  mo_bands=as.factor(c("B02","B03","B04","B05","B06","B07","B08","B11","B12","B8A","NDVI"))
  
  #important materials:
  #https://www.rdocumentation.org/packages/rgrass7/versions/0.1-9/topics/initGRASS
  #https://stat.ethz.ch/pipermail/r-sig-geo/2009-April/005418.html
  #https://cran.r-project.org/web/packages/spgrass6/spgrass6.pdf
  
  #location of your GRASS installation
  loc = initGRASS(gisBase=gisBase,home=tempdir(),gisDbase=gisDbase,
                  location="lambert",mapset=mapset,override=T)
  loc
  
  #import the country map as base map and set the region
  execGRASS("r.in.gdal", parameters=list(input=base_map,output="country_basemap"))
  execGRASS("r.mask", flags="o", parameters=list(input=paste("country_basemap@",mapset,sep="")))
  execGRASS("g.region", parameters=list(rast=paste("country_basemap@",mapset,sep="")))
  execGRASS("g.region", parameters=list(res="100"))
  #execGRASS("g.region",flags="p")
  
  #enable parallel running
  cl4 = makeCluster(11)
  registerDoParallel(cl4)
  
  foreach( i=1:length(mo_bands),.packages="spgrass6" ) %dopar% {
    #for(i in 1:length(mo_bands)) { 
    
    #list all the utm files of each band
    r_in_gdal_list=list.files(path=paste("./",mo_bands[i],sep=""),pattern="*.tif",full.names=T)
    r_in_gdal_list
    
    #create a new name as output
    r_in_gdal_map=substring(r_in_gdal_list,8,nchar(r_in_gdal_list)-4)
    r_in_gdal_map
    
    #read each tif file one by one into GRASS
    for (j in 1:length(r_in_gdal_list)){
      execGRASS("r.in.gdal", parameters=list(input=r_in_gdal_list[j], output=r_in_gdal_map[j]))
    }
    
    #mosaic
    rpatch_input=paste(r_in_gdal_map[1],"@",mapset,",",r_in_gdal_map[2],"@",mapset,",",r_in_gdal_map[3],"@",mapset,sep="")
    rpatch_input
    execGRASS("r.patch", parameters=list(input=rpatch_input, output=paste(mo_bands[i],"_mosaic",sep="")))
    
    #rescale (if not NDVI) & export
    if(mo_bands[i]=="NDVI"){
      r_out_gdal_output=paste("../outputs/",mo_bands[i],"_mosaic_laea.tif",sep="")
      execGRASS("r.out.gdal", parameters=list(input=paste(mo_bands[i],"_mosaic@",mapset,sep=""), 
                                              output=r_out_gdal_output))
    } else {
      execGRASS("r.mapcalculator",parameters=list(formula=paste(mo_bands[i],"_mosaic@",mapset," / 10000",sep=""),
                                                  outfile=paste(mo_bands[i],"_mosaic2",sep="")))
      r_out_gdal_output=paste("../outputs/",mo_bands[i],"_mosaic_laea_x0.0001.tif",sep="")
      execGRASS("r.out.gdal", parameters=list(input=paste(mo_bands[i],"_mosaic2@",mapset,sep=""), 
                                              output=r_out_gdal_output))
    }
  }
  stopCluster(cl4)
  
  #remove the directory created by GRASS
  unlink(paste(gisDbase,"/lambert/",mapset,sep=""),recursive=T)
}
