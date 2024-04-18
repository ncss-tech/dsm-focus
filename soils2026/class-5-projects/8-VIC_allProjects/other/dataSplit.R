

#Extract covariate data values to field observations points 

#Create data partitions for model training and validation 



# load necessary libraries 
library(terra) 
library(sf) 
library(caret) 





# check working directory (put in the path to your modeling folder) 
# make sure the slashes are / 

setwd("C:/8-VIC-class5Projects/cov100_clip") 
getwd() 

### read in raster layers to create raster stack for extracting covariate values to point data 
# read in raster layers from modeling folder and create list 
rlist=list.files(getwd(), pattern="tif$", full.names = FALSE) 



# create raster stack 
r_stack <- terra::rast(rlist) 
names(r_stack) 

## read in shapefile containing field observations for the classes to be predicted and extract values from the raster stack at each point; this process will relate each class to specific values from the raster covariates 
setwd("C:/8-VIC-class5Projects/") 
getwd() 
# read in shapefile 
pts <- st_read("pedons.shp") 

# check names of attributes in pts
names(pts) 

levels(as.factor(pts$Class))
summary(as.factor(pts$Class))

#remove classes with less than 3 observations
# remove classes with < 2 observations per class
pts <- pts[pts$Class %in% names(which(table(pts$Class)>3)),]
levels(as.factor(pts$Class))
pts$Class <- as.factor(pts$Class)
levels(pts$Class)

# extract raster covariate values at each training point
pts.sv <- terra::extract(r_stack, pts, xy=TRUE, bind=TRUE, na.rm=TRUE) 

# inspect the extraction for the first 5 covariates
head(pts.sv[,1:5]) 

plot(r_stack$ADIUCL5)
points(pts.sv) 



# convert SpatVector to sf 
pts.sf <- sf::st_as_sf(pts.sv) 

names(pts.sf)

# write shapefile 
st_write(pts.sf, dsn='C:/8-VIC-class5Projects', layer='pedons_ext', driver='ESRI Shapefile', delete_layer = TRUE)


names(pts.sf)

pts.sf <- pts.sf[-c(2:14, 176,177)]



pts.sf$Class <- as.factor(pts.sf$Class)
names(pts.sf)
#

summary(pts.sf$Class)



## Data splitting 
# we will use the caret package to split our data into two data sets. The training dataset will contain 70% of all of the observations, and the validation dataset will contain the remaining 30% 



set.seed(4345)# for reproducability 
# first we need to create an index referring to the class column and .7 indicating to partition 70% of the records since we are doing an 70/30 split 
splitIndex <- createDataPartition(pts.sf$Class,  p=.8, 
                                  list= F ,  
                                  times = 1) 

# now using the index we split our data 

# first is the training data set which will have 70% of the records; the index refers to .7 and is in the rows position of the dataframe reference, so we'll keep 70% of the rows and all columns 

trainData <- pts.sf[splitIndex,] 

# next is the validation data set which will have 30% of the records; the index refers to .7 so now we use a minus (-) before the index in the rows position of the dataframe reference, so we'll keep only 30% of the rows and all columns 

validationData <- pts.sf[-splitIndex,] 

# a quick check is to see how many points are in each data set and make sure it adds to the total number of points you started with and check you've got class and covariate values attached 

length(pts$Class) # total number of observations 
length(trainData$Class) # number of observations used for training data
length(validationData$Class) # number of observations used for validation data 


## Make shapefiles files of training and validation datasets 
# write shapefile 
st_write(trainData, dsn='C:/8-VIC-class5Projects', layer='train_8020', driver='ESRI Shapefile', delete_layer = TRUE) 

st_write(validationData, dsn='C:/8-VIC-class5Projects', layer='valid_8020', driver='ESRI Shapefile', delete_layer = TRUE) 
