# -*- coding: utf-8 -*-
"""
Created on Mon Sep 13 08:58:26 2021

@author: clairesimpson
@author: david.white@usda.gov

This script will take an input folder in which all geotiffs from GEE script have been saved
and iteratively mosaic all the files within the folder based on 'tags' in the filename (e.g. Slope, NDVI..)
-- optional snapping and mandatory NoData reassignment (can also specify NoData value = None and let this 
script "Auto-detect" the NoData value) 

"""

# USER INPUT: set to local folder in which all GEE outputs have been downloaded
#Note: the r preceding the folder path will convert the file path into a raw string which is interpretable by python regardless of direction of slashes in filepath
inFolder=r'D:/Soils2026/download/ca793TEST'
#USER INPUT: set to raster to which you want to snap all GEE outputs (e.g. SSURGO or gNATSGO raster)
snapRaster = r'D:/Soils2026/gSSURGO-mukey.tif' #or None
#USER INPUT: set to horizontal resolution (meters) of spectral data and topographic data
#spectralResolution = 10 
#topographicResolution = 10
res = 10
#USER INPUT: set to no data values for spectral and topographic data (as set in GEE)
#alternatively you can set these value to None and let the script choose the no data value *HOWEVER* this assumes that the upper left corner of your raster is populated with No Data 
spectralNoData = -32768 #None
topoNoData = None #-9999
#set resampling method if desired (options: 'BILINEAR', 'CUBIC', 'NEAREST') (default is NEAREST)
resampling = 'BILINEAR'

#end of user parameters
########################################################################################################################################################
########################################################################################################################################################

import arcpy
from arcpy.sa import *
import os
import zipfile
arcpy.env.overwriteOutput=True
arcpy.env.parallelProcessingFactor='100%'

if os.path.exists(inFolder)==False:
    print('ERROR: specified inFolder does not exist')
if snapRaster is not None and os.path.exists(snapRaster) ==False:
    print('ERROR: specified snapRaster does not exist')
if type(res)is str:
    print('ERROR: the datatype for the spectral and topographic resolution should be int or float (or NoneType)')
if type(spectralNoData) is str or type(topoNoData) is str:
    print('ERROR: the datatype for the spectral and topographic no data values should be int or float (or NoneType)')
if resampling is not None and resampling.upper() !='BILINEAR' and resampling.upper() != 'CUBIC' and resampling.upper()!='NEAREST':
    print('ERROR: incorrect value set for resampling. SHould be either bilinear, cubic, nearest, or None')

## DEFINE FUNCTIONS
outFolder = os.path.join(inFolder)
if os.path.exists(outFolder) == False:
    os.mkdir(outFolder)
arcpy.env.workspace = inFolder 
if snapRaster is not None:
    arcpy.env.snapRaster = snapRaster
if resampling is not None:
    arcpy.env.resamplingMethod = resampling.upper()

prefixList =  [i for i in os.listdir(inFolder) if i.endswith('.tif')][0].split('_')
if len(prefixList) ==2:
    prefix = prefixList[0]
else:
    prefix = '_'.join(prefixList[:-1])
print('\nYour file prefix is:',prefix)

tagList_topo = ['elev', 'slope', 'aspect', 'hillshade',
                'mincrv', 'maxcrv',
               'gauscrv', 'eastness', 'northness', 'horzcrv', 'shpidx',
               'meancrv', 'vertcurv']
tagList_spectral = ['red', 'green', 'blue', 're1','re2','re3', 'nir1', 'nir2', 'swir1',
               'swir2', 'ndvi', 'nbr', 'bsi', 'ndmi','ferrIdx','ndsi', 'ndci','natricidx','hi',
               'evi','savi','TCbright', 'TCgreen', 'TCwet', 'TC4', 'TC5', 'TC6', 
               'gpyidx','feoxidx','clayidx','wateridx','rockidx']#,'msavi2'] removed msavi from specral list as it is a 32 float rest are 16 signed

yearsList = list(set([i.split('_')[-1] for i in os.listdir(inFolder) if i.endswith('.tif') and 'mosaic' not in i and 'cNDVI' in i]))

## mosaicking step
print('mosaicking...')

#mosaic for cNDVI
for year in yearsList: 
    tif_list = [i for i in os.listdir(inFolder) if 'mosaic' not in i and prefix+'_cNDVI'+year in i and i.endswith('.tif')]
    if len(tif_list) == 1:
        print('composite NDVI for year',year, 'was not tiled, skipped')
    elif len(tif_list)>1:    
        tif_string = ';'.join(i for i in tif_list)    
        output_name = prefix+'_cNDVI'+year +'_mosaic.tif'
        output_prj = ''
        output_dt = "16_BIT_SIGNED"
##8_BIT_UNSIGNED | 1_BIT | 2_BIT | 4_BIT 
##8_BIT_SIGNED | 16_BIT_UNSIGNED | 16_BIT_SIGNED | 32_BIT_FLOAT | 32_BIT_UNSIGNED 
##32_BIT_SIGNED | | 64_BIT
        cellsize = str(res)
        output_numbands = 1
        mosaic_method = "MAXIMUM"
        arcpy.MosaicToNewRaster_management(tif_string, outFolder, output_name, output_prj, output_dt, cellsize, output_numbands,mosaic_method)
        print ('done mosaicking',output_name)  

#mosaic for topo data 
for tag in tagList_topo:
    tif_list = [i for i in os.listdir(inFolder) if i.endswith('.tif') and 'mosaic' not in i and prefix+'_'+tag in i]
    if len(tif_list) == 1:
        print(tag, 'was not tiled, skipped')
    elif len(tif_list)>1:    
        tif_string = ';'.join(i for i in tif_list)    
        output_name = prefix+'_'+tag +'_mosaic.tif'
        output_prj = ''
        output_dt = "32_BIT_FLOAT"
        cellsize = str(res)#meters
        output_numbands = 1
        mosaic_method = "MAXIMUM"
        arcpy.MosaicToNewRaster_management(tif_string, outFolder, output_name, output_prj, output_dt, cellsize, output_numbands,mosaic_method)
        print ('done mosaicking',output_name)
    
    
# mosaic for all spectral data except msavi2 and cndvi    
for tag in tagList_spectral:
    tif_list = [i for i in os.listdir(inFolder) if i.endswith('.tif') and 'mosaic' not in i and prefix+'_'+tag in i and 'hillshade' not in i and 'msavi2' not in i]
    if len(tif_list) == 1:
        print(tag ,'was not tiled, skipped')
    elif len(tif_list)>0:
        tif_string = ';'.join(i for i in tif_list)
        output_name = prefix+'_'+tag +'_mosaic.tif'
        output_prj = ''
        output_dt = "16_BIT_SIGNED" 
        cellsize = str(res) #meters    
        output_numbands = 1
        mosaic_method = "MAXIMUM"
        arcpy.MosaicToNewRaster_management(tif_string, outFolder, output_name, output_prj, output_dt, cellsize, output_numbands,mosaic_method)
        print ('done mosaicking',output_name)

# mosaic for msavi2
tif_list = [i for i in os.listdir(inFolder) if i.endswith('.tif') and 'mosaic' not in i and prefix+'_'+'msavi2' in i]
if len(tif_list) == 1:
    print('msavi2 was not tiled, skipped')
elif len(tif_list)>0:
    tif_string = ';'.join(i for i in tif_list)
    output_name = prefix+'_'+'msavi2' +'_mosaic.tif'
    output_prj = ''
    output_dt = "32_BIT_FLOAT" 
    cellsize = str(res) #meters    
    output_numbands = 1
    mosaic_method = "MAXIMUM"
    arcpy.MosaicToNewRaster_management(tif_string, outFolder, output_name, output_prj, output_dt, cellsize, output_numbands,mosaic_method)
    print ('done mosaicking',output_name)


###### Set Raster Properties and Calc Stats
# set null and calculate statistics for derivatives
def setNull(imageList, nodata):
    if nodata is None:
        for i in imageList:
            rast = arcpy.Raster(i)
            rastNodata = rast[0,0]
            arcpy.management.SetRasterProperties(in_raster=i, nodata=[[1,rastNodata]])
            arcpy.CalculateStatistics_management(in_raster_dataset=i, x_skip_factor=100, y_skip_factor=100, skip_existing='OVERWRITE', ignore_values = [[1,rastNodata]])
    else:
        setNodata = nodata
        for i in imageList:
            arcpy.management.SetRasterProperties(in_raster=i, nodata=[[1,setNodata]])
            arcpy.CalculateStatistics_management(in_raster_dataset=i, x_skip_factor=100, y_skip_factor=100, skip_existing='OVERWRITE', ignore_values =[[1,setNodata]])

# create years list to include mosaic versions
yearsList2 = list(set([i for i in os.listdir(inFolder) if i.endswith('.tif') and 'cNDVI' in i and '-00' not in i]))

# create list of all tif files excluding the tiles
fileList = list([i for i in os.listdir(inFolder) if i.endswith('.tif') and '-00' not in i])

# set null and calc stats on cNDVI layers
cndviList = [i for i in fileList if any(year in i for year in yearsList2)]
for tag in cndviList:
    if len(cndviList)>0:
        tif_list = [i for i in os.listdir(inFolder) if tag in i and i.endswith('.tif')]
        setNull(tif_list, spectralNoData)
        #print('calculating stats for', tag)
print('done calculating cNDVI stats')

# set null and calc stats on topo layers
for tag in tagList_topo:
    if len(tagList_topo)>0:
        tif_list = [i for i in os.listdir(inFolder) if i.endswith('.tif') and prefix+'_'+tag in i and '-00' not in i] #the -00 is to skip calculations on the tiled images
        setNull(tif_list, topoNoData)
        #print('calculating stats for', tag)
print('done calculating topo stats')


# Set null and calc stats on spectral layers
for tag in tagList_spectral:
    tif_list = [i for i in os.listdir(inFolder) if i.endswith('.tif') and prefix+'_'+tag in i and 'hillshade' not in i and '-00' not in i]
    # in tif_list I had to exclude hillshade, it sometimes pulls in hillshade with hi
    setNull(tif_list, spectralNoData)
    #print('calculating stats for', tag)
print('done calculating spectral stats')

# set null and calc stats on msavi2
msavNoData = spectralNoData
msav = [i for i in fileList if 'msavi2' in i and '-00' not in i and i.endswith('.tif')]
if len(msav)>0:
    rast = arcpy.Raster(msav)
    arcpy.management.SetRasterProperties(in_raster=rast, nodata=[[1,msavNoData]])
    arcpy.CalculateStatistics_management(in_raster_dataset=rast, x_skip_factor=100,
                                     y_skip_factor=100, skip_existing='OVERWRITE',
                                     ignore_values = [[1,msavNoData]])
    print('done calculating msavi2 stats')    



print('THIS SCRIPT IS FINISHED - YAY')
