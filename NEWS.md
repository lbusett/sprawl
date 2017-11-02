# sprawl 0.3.0
<br>

02/11/2017 - First semi-stable releas 

## Major changes 

 - Improvements on `extract_rast`, now dealing also with categorical rasters
 
 - Added plotting routines `plot_rast_gg` and `plot_vect`
 
 - Added routines for better dealing with categorical rasters (`set_rastlabels`, 
   `recategorize_rast`)
 
 - Added routines for extracting info from raster data (`get_rastinfo`, 
   `get_raststats`, `plot_rasthist`)
   
 - Added routines for simplified reading of raster files (`read_rast`) and 
   vector file (`read_vect`) from disk
   
 - Added routines for reprojection of raster and vector data (`reproj_rast`, 
   `reproj_vect`, `reproj_extent`)
 
 - Added routines for creation of fishnets (`create_fishnet`)
 
 - Added routines for cropping and masking rasters, based on cretion of temporary
   gdal vrt files (`crop_rast`, `mask_rast`)
   
 - Added routines for cropping vectors (`crop_vect`)
 
 - Added routine for automatic downloaf of administartive boundaries polygons (
  `get_boundaries`)
  
 - Added routine for migrating virtual raster files (RData and GDAL vrt) (
  `migrate_virtrast`)
 
## Minor changes 

 - Addition of several helper functions (`make_folder`, `fillpals`, `select_extent`
 `find_gdal`, `cuttails_rast`, `convert_rast_dtype`, `add_scale_fill`, 
 `sprawl_scalebar`)
 
 - Homogenized argument names
 
 - Improved tests and examples
 
## Bug Fixes 

 - too many to mention



# sprawl 0.2.0
<br>

20/08/2017 - Second major release, adding some core funtionality and helpers. 

## Major changes 

 - Addition of `mask_rast` and `crop_rast`
 
 - Addition of several helper functions (`get_projstring`, `get_extent`, `cast_vect`
 `cast_rast`, `reproj_extent`...)

## Minor changes 

 - Added coding style and contributing pages to website
 
## Bug Fixes 

 - too many to mention

________________________________________________________________________________

# sprawl 0.1.0.9000
<br>
## Major changes 

 - 21/07/2017 Release of the first (rather unstable) version of `sprawl`. Introduces
  some raher complex functions (`extract_rast`, `aggregate_rast`, plus a series of
  smaller-scope functions used as helpers). 

## Minor changes 


## Bug Fixes 

