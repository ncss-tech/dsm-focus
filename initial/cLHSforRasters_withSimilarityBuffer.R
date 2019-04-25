
#cLHS 
#https://cran.r-project.org/web/packages/clhs/clhs.pdf

# Scripting by C Simpson 4/2019
# GitHub @cesimpson

# Use: 
# cost-constrained Latin Hypercube Sampling with input raster covariates
# Similarity buffers on all output points 


##########################################################
## Housekeeping: import packages, assign output filename variables, set workspace...
##########################################################

rm(list = setdiff(ls(), lsf.str())) ##clear all variables

library(spatialEco)
library(sf)
library(rgdal)
library(raster)
library(clhs)
library(stringr)
library(sp)
library(tools)
library(FactoMineR)
library(dplyr)
library(RStoolbox)
library(reshape2)
library(factoextra)
library(ggplot2)
library(grid)
library(reshape2)
library(plyr)

#model parameters
bufDist=100 #radius of similarity buffer 
numPoints = 1500 #number of clhs points to create
numIter = 10000 #number of iterations for clhs

#file name identifiers 
runNum=0 #Unique arbitrary identifier for output files
res = '10m' #spatial resolution - identify for output filenames 

#names of output files 
imageName <- paste('clhsImage', res, numPoints, numIter, runNum, '.RDATA', sep='_')
clhsShpName <- paste('clhsPts', res, numPoints, numIter, runNum,  sep='_')
dataName <- paste('clhsRData', res, numPoints, numIter, runNum, '.RDATA',  sep='_')
rasterStackName <- paste('rasStack', res, numPoints, numIter, runNum, '.RDATA',  sep='_')
outSimilarityBuffers <- paste('similarityBuffers',res,numPoints,numIter, runNum,'.TIF',sep='_')
txtFile <- paste('README',res,numPoints,numIter, runNum,'.txt',sep='_')

filePath = 'F:/BMShareDrive/30m_origLayers/clhs_outputs/'
folderName <- paste(filePath, 'cLHS_run',runNum,setp='')
dir.create(folderName)
setwd(folderName)

script.start.time <- Sys.time()
set.seed(1)

##########################################################
## 1. Create list of raster data (covariates for cLHS input)
##########################################################
#allFiles <- list.files(filePath,pattern=".tif$", full.names=TRUE,recursive=FALSE)
#allFiles<-allFiles[!str_detect(allFiles,pattern="Int_b9.tif")]

# list of raster covariates
allFiles <- c(' C:/DEF.tif ',
              ' C:/ElevSlopeAspect_clipInt_b1.tif ',
              ' C:/ElevSlopeAspect_clipInt_b2.tif ',
              ' C:/Landsat_SR_b13.tif ',
              ' C:/Landsat_SR_b14.tif ',
              ' C:/Landsat_SR_b7.tif ',
              ' C:/srad.tif ',
              ' C:/StandardizedHeightInt.tif ',
              ' C:/tmin_annual.tif ',
              ' C:/TPI_01500Int.tif ',
              ' C:/NED_SAGAWI2Int.tif ',
              ' C:/PI_50IntClip.tif ',
              ' C:/gensym-sup-class-latest_prj_10m.tif',
              ' C:/Geomorphons.tif',
              ' C:/StreamElevDist.tif')

costFile <-"C:/Users/c/R1_Trails_subset/TrailsCost_5c3_10m4_reclas_waterMask.tif "


#assign names (<= 10 chars in length) for output clhs points shapefile attributes in order of layers in ras stack
fileNames <- c('Cost','DEF','Elev', 'Slope', 'LS13_NDCI', 'LS14bright', 'LS7_NDVI','SolarRad', 'StdHeight',
               'tmin', 'TPI',  'Wetness', 'PI', 'gensym', 'geomorphon', 'elevstr')



##########################################################
# 2. Crop all raster data to the same extent, remove areas of NA (areas outside of trail buffer) 
# then stack raster layers
##########################################################
r.stack<- raster(costFile)
ext <- extent(r.stack)
print (c(ext, "is extent of raster stack"))
mask <- (r.stack == 128 | r.stack == -128) #where 128 is nodata value of mask
#or: mask <- is.na(r.stack)

r.stack[mask]= NA


for (f in allFiles){
  print (c('Adding to stack:',f))
  r.Addlyr <-raster(f)
  print (extent(r.Addlyr)>ext) #if False, need to extend not crop
  
  #Crop layer to extent of cost layer
  r.Addlyr <- crop(r.Addlyr, ext) 
  
  #mask out values outside of cost layer 
  #(i.e. assign values in current layer as NA if they are NA in cost layer)
  r.Addlyr[mask] = NA
  
  #add layer to raster stack
  r.stack <- addLayer(r.stack,r.Addlyr)
  print (r.stack)
}
print (r.stack)

#free up memory
rm(r.Addlyr, mask, ext)
gc()

##########################################################
## 4. assign certain layers (thematic) to be of type factor 
##########################################################
r.stack$Geomorphons <- as.factor(r.stack$Geomorphons) #assign geomorphons to factor type
r.stack$gensym.sup.class.latest_prj_10m<- as.factor(r.stack$gensym.sup.class.latest_prj_10m)


##########################################################
## 5. run cLHS (on stack here, but could be on random samples if memory issues?)
###########################################################
##
gc()
start.time <- Sys.time()

s2 <- clhs(r.stack,
           size = numPoints,
           iter = numIter,
           cost = 'TrailsCost_5c3_10m4_reclas_waterMask', #trails buffered layer
           #weights= list(numeric = 1, factor = 4, correlation = 1),
           simple = FALSE, 
           progress = TRUE)

end.time <-Sys.time()
print (c("Time elapsed during cLHS run:", end.time - start.time))


##########################################################
# 6. plot clhs points
##########################################################
plot(s2, mode = c("obj", "cost")) #mode options: hist, box

##########################################################
# 7. save data
##########################################################
print (names(s2$sampled_data))

#reassign attribute names when exporting clhs result to shapefile
names(s2$sampled_data) <- fileNames 

print (c('New names of shp attr:',names(s2$sampled_data)))

#write clhs result to shapefile
writeOGR(s2$sampled_data, ".", clhsShpName, driver="ESRI Shapefile") 

#save workspace and certain data vars 
save(s2, file=dataName)
save(r.stack, file = rasterStackName)
save.image(imageName)

##########################################################
# 8. run similarity buffers

#method:
## create similarity buffer for each point and mosaic to rest of data points' similarity buffers with method 'max'
## * this helps with memory errors (generated if full extent of clhs data were used) 
## ** and reduces computational load of exporting [runNum=]1500 band raster if buffers could be created  
## *** WARNING: if two clhs points are closer together than buffer distance, 
## *** similarity buffers for these points will  be inaccurate, 
## *** with the max similarity val assigned to overlapping pixels
##########################################################

#drop trails cost layer from stack (so not included in similarity calculation)
r.stack.copy <- r.stack #create copy to look at later
r.stack <- dropLayer(r.stack,1)

#create initial mosaic object based on first clhs point
ext <- extent(s2$sampled_data[1,])+(bufDist*2)
r.stack.sub <- crop(r.stack, ext) 
buffer <- similarity_buffer(r.stack.sub, s2$sampled_data[1,], buffer= bufDist, fac=c(13,14)) #15 is index of geomorphons
plot(buffer)
buffer.mosaic <- mosaic(buffer, buffer,fun=max)
plot(buffer.mosaic)

#create similarity buffers for clhs points and mosaic together
for (i in 2:numPoints){
  print (c('..Processing point:', i))
  ext <- extent(s2$sampled_data[i,])+(bufDist*2)
  r.stack.sub <- crop(r.stack, ext)
  buffer <- similarity_buffer(r.stack.sub, s2$sampled_data[i,], buffer= bufDist, fac=c(13,14)) #15 is index of geomorphons
  buffer.mosaic <- mosaic(buffer.mosaic, buffer, fun=max)
}

print (buffer.mosaic)
#export single band mosaic of all similarity buffers of type integer 
#(where pixel value is percent similarity to point)
buffer.mosaic = buffer.mosaic*100
dataType(buffer.mosaic)<-'INT4S'
writeRaster(buffer.mosaic,filename=outSimilarityBuffers, format='GTiff', overwrite=TRUE, datatype='INT4S')

rm(ext, r.stack.sub, buffer)
gc()

##########################################################
# Write data specs to text file
##########################################################

sink(txtFile, type='output')
writeLines(c("README\n\n", "Created on:", Sys.time(),
             "\nModel Parameters:\n===================================================",
             '\nNumber of clhs points generated:',numPoints,"\nRun number with these specifications:",
             runNum,"\nNumber of clhs iterations:",numIter,"\nFiles used as covariates:",allFiles,
             'Cost File:',costFile))
sink()

##########################################################
