# Import modules
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.env.parallelProcessingFactor = "75%"

# Define folder location of files to use
env.workspace = r'D:\Jornada LRU Update\CovDev\DEM'

# elevation raster to work with
elevlp3 = Raster("dem10m.tif")
#list of neighborhood radii to use in focal stats
radlist = [1, 2, 4, 8, 16, 32, 64, 128, 384]
#list of different types of relative elevation surfaces to comput
layerlst = ['relht', 'relmeanht']
#type of focal statistic for each relelev surface
layerdict = {'relht':"MINIMUM", 'relmeanht':"MEAN"}
print('Done with layer definitions')

#two nested for loops to calculate all surf types at all selected radii
for lyr in layerlst:
    for rad in radlist:
        rastname = str(lyr) + str(rad) + '.tif'
        lyrtype = layerdict[lyr]
        focalstat = FocalStatistics(elevlp3, NbrCircle(rad, "CELL"), lyrtype, "DATA")
        focalstat = elevlp3 - focalstat
        focalstat = focalstat * 100
        focalstat = Int(focalstat)
        focalstat.save("D:\Jornada LRU Update\CovDev\DEM\cov\ " + rastname)
        print("Done with " + rastname)

print("Done creating relative elevation surfaces")
                                    
