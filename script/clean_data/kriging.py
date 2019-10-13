"""
This script executes Kriging interpolation acc. to the steps documented in 
Cooperman (2017)'s supplementary material. The file was run in ArcGIS's jupyter
notebook console.
---------------------------------
The overall steps are as follows:
1. reproject rainfall data points to USA Contiguous Equidistant Conic;
2. run kriging interpolation on shape file output from step 1;
3. reproject the raster output from step 2 to USA Contiguous Equal Area Conic;
4. compute average rainfall level from raster output from step 3.
-------------------------------
ArcGIS often raises an error that a file name is invalid when the filename is too long,
especially when the output is a raster. Creating `tempfile` objects raises a similar error. 
To avoid this error, the immediate output of arcpy 
operations are named "temp" and then renamed later. 
--------------------------------
ArcGIS runs into an error when given too many files to iterate over. Therefore,
the following steps were often run over two to five files at a time instead of over 
all files at once.
--------------------------------
Each step creates output files. 
- The input files of STEP 1 are stored in "output/rainfall/points"
- STEP 1 outputs (STEP 2 inputs) are stored in "output/rainfall/points_reproj"
- STEP 2 outputs (STEP 3 inputs) are stored in "output/rainfall/krigoutput"
- STEP 3 outputs (STEP 4 inputs) are stored in "output/rainfall/raster_reproj"
- STEP 4 outputs are stored in "output/rainfall/zonal_stat" 
"""

import arcpy
from arcpy import env
from arcpy.sa import *
import os
import re
import requests, zipfile, io
import shutil
from dbfread import DBF
from pandas import DataFrame

# set up 
arcpy.CheckOutExtension("Spatial") # requires Spatial Analyst license to run KrigingModelUniversal
env.parallelProcessingFactor = "100%"
env.overwriteOutput = True
data_dir = os.path.join(os.path.dirname(os.getcwd()), "data")
out_dir = os.path.join(os.path.dirname(os.getcwd()), "output", "rainfall")
pt_dir = os.path.join(out_dir, "points") # directory storing input files
pt_rprj_dir = os.path.join(out_dir, "points_reproj")
krigout_dir = os.path.join(out_dir, "krigoutput")
rstr_rprj_dir = os.path.join(out_dir, "raster_reproj")
znl_stt_dir = os.path.join(out_dir, "zonal_stat")


# write a function that removes any file named "temp" from a directory
def remove_temp(dirname):
    """
    dirname (str)
    """
    for temp in os.listdir(dirname):
        if temp.startswith("temp"):
            os.remove(temp)
        
    
    

""" STEP 1: Reproject rainfall data points to USA Contiguous Equidistant Conic"""
pattern = re.compile("^rainfall(?P<date>[0-9]{4}\-[0-9]{2}\-[0-9]{2})\.shp$")
shp_files = [file for file in os.listdir(pt_dir) if pattern.search(file) != None] #check len is 42
env.workspace = pt_rprj_dir
for file in shp_files:
    date = pattern.search(file).group("date")
        
    # Reproject the rainfall shapefile to "shp/rainfall<date>_reproj.shp"
    input_file = os.path.join(pt_dir, file)
    proj = arcpy.SpatialReference("USA Contiguous Equidistant Conic")
    reprojected = "rainfall" + date + "_reproj"
    arcpy.Project_management(input_file, os.path.join(pt_rprj_dir, reprojected), proj) 
    
    ## check that the reprojected shapefile now has meter units 
    pr = arcpy.Describe(os.path.join(pt_rprj_dir, reprojected)).spatialReference
    print("Rainfall shapefile for {} is not in {}".format(date, pr.linearUnitName))


    
    
""" STEP 2: Run Kriging interpolation """
pattern = re.compile("^rainfall(?P<date>[0-9]{4}\-[0-9]{2}\-[0-9]{2})\_reproj\.shp$")
reproj_files = [file for file in os.listdir(pt_rprj_dir) if pattern.search(file) != None] #check len is 42
env.workspace = krigout_dir
for file in reproj_files:
    date = pattern.search(file).group("date")
    
    # set up Kriging model 
    cellsize = 4000 # "cell size of 4000 meters squared"
    field = "value"
    kradius = RadiusVariable(12)
    kModel = KrigingModelUniversal("LINEARDRIFT")
    
    # run Kriging and save output as "krigoutput<date>.tif"
    input_data = os.path.join(shpdir, file)
    outKrig = Kriging(input_data, field, kModel, cellsize, kradius)
    tifname = os.path.join(krigout_dir, "krigoutput" + date + ".tif")
    outKrig.save(tifname)
    
    # print the average rainfall across all cells of the raster
    print("Average rainfall for {}: {}".format(date, arcpy.GetRasterProperties_management(tifname, "MEAN")))

          
          
          
""" STEP 3: Reproject the raster output from STEP 2 and resample values """    
pattern = re.compile("^krigoutput(?P<date>[0-9]{4}\-[0-9]{2}\-[0-9]{2}).tif$")
krig_files = [file for file in os.listdir(krigout_dir) if pattern.search(file) != None] #check len is 42
env.workspace = rstr_rprj_dir
for file in krig_files: 
    inRas = os.path.join(krigout_dir, file)
    date = pattern.search(file).group("date")
    
    # Reproject Kriging output raster and resample. Save output to "krigoutput<date>_reproj.tif"
    outProj = arcpy.SpatialReference("USA Contiguous Albers Equal Area Conic")
    outRas = os.path.join(rstr_rprj_dir, "temp.tif")
    cellsize = "1000" # "resample to cell sizes of 1000 meters squared"
    arcpy.ProjectRaster_management(inRas, outRas, outProj, "BILINEAR", cell_size = cellsize)
    
    # print the average rainfall across all cells of the raster
    print("Average rainfall for {}: {}".format(date, arcpy.GetRasterProperties_management(outRas, "MEAN")))
    
    # rename "temp" files as "krigoutput<date>_reproj"
    temps = [t for t in os.listdir(rstr_rprj_dir) if t.startswith("temp")]
    for temp in temps:
        filename, file_ext = os.path.splitext(temp)
        filename = "krigoutput" + date + "_reproj"
        shutil.move(os.path.join(rstr_rprj_dir, temp), os.path.join(rstr_rprj_dir, filename + file_ext))

# remove temp files from directory
remove_temp(env.workspace)
    
    
          
""" STEP 4: Obtain average rainfall for each county  """
# layer name of US county cartographic boundary .shp file from the 2010 Census
layer_name = "gz_2010_us_050_00_500k"

pattern = re.compile("^krigoutput(?P<date>[0-9]{4}\-[0-9]{2}\-[0-9]{2})\_reproj.tif$")
krig_reproj_files = [file for file in os.listdir(krigout_dir) if pattern.search(file)!= None] #check len is 42
## get field names for the us county shapefile 
#for f in arcpy.ListFields(os.path.join(shpdir, zipname + ".shp")):
#    print(f.name)
env.workspace = znl_stt_dir
for file in krig_reproj_files:
    date = pattern.search(file).group("date")
    
    # Get zonal statistics as table 
    counties = os.path.join("data", layer_name, layer_name + ".shp") # each county is a zone for which we compute the mean
    zone_id = "GEO_ID" # this is the unique id for each county in the us county shapefile
    temp = "temp.dbf"
    arcpy.gp.ZonalStatisticsAsTable_sa(counties, zone_id, file, temp, "NODATA", "ALL")
    
    ## print out the first 5 rows(counties) of the dbf
    frame = DataFrame(iter(DBF(os.path.join(rstr_reprj_dir, temp))))
    print("First five rows of {}: \n {}".format(date, frame.head(5))
    
    
    ## rename the temporary file to "county-rainfall<date>"
    temps = [t for t in os.listdir(rstr_reprj_dir) if t.startswith("temp")]
    for temp in temps:
        filename, file_ext = os.path.splitext(temp)
        filename = "county-rainfall" + date
        shutil.move(os.path.join(rstr_reprj_dir, temp), os.path.join(znl_stt_dir, filename + file_ext))

# remove temp files
remove_temp(env.workspace)
