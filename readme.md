**This directory contains the R scripts used to develop the Sentinel 2 Greenest Pixel Composite data products on country level for the Africa Soil Information Service (AfSIS). The scripts were written by Yanni Zhan, CIESIN, Columbia University.**

## Algorithms
1) Resample the 10 m, 20 m and 60 m .jp2 files to 100 m .tif files
2) Select greenest NDVI for each pixel of all the data available and the corresponding values of other bands
3) Mosaic each UTM zone in the country
4) Reporject to laea and mosaic the whole country

## Scripts
### green.R
* Parameters
   * input_dir (the dirctory that includes the unzipped sentinel 2 data in a given year)
   * output_dir (the directory you want to save the output result)
   * gisBase, gisDbase & mapset (GRASS settings on your local machine)
   * base_map (the country map used to clip)
* The Sentinel 2 composites are located on AfSIS FTP Site:
   * ftp://africagrids.net/100m/Sentinel2/

### green_function.R
* This script contains all the functions needed in the green.R script.

