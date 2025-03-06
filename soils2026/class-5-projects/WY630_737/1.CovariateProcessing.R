# load and install package"s
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

#set working directory to spectral covs
setwd("~/wy/cov30s_WY630_737/clip")
getwd()

# read in raster file names as a list
sList=list.files(getwd(), pattern="tif$", full.names = FALSE)
sList
names <- sList
# stack
specStack <- rast(sList,names)
names(specStack) <- sList

#look for NAs
NAs <-global(specStack, fun="isNA")
NAs

##### fix NAs #####
#first move the layers identified as having extra NA values to a different folder

setwd("~/wy/cov30s_WY630_737/clip/NAs")

sNAlist=list.files(getwd(), pattern="tif$", full.names = FALSE)
sNAlist
sNAStack <- rast(sNAlist)
names(sNAStack) <- sNAlist
names(sNAStack)
#look at number of NAs for reference
sNAs <- global(sNAStack, fun="isNA")
sNAs

#use focal filter to remove NA values (may not remove all of them but don't want to set the window too big)
sNAf <- focal(sNAStack, w=5, fun="mean", na.policy="only", fillvalue=NA, expand=FALSE, silent=FALSE, overwrite=FALSE)

#check number of NAs and compare to make sure they've gone down
newsNAs <- global(sNAf, fun="isNA")
sNAs
newsNAs

#write the rasters back to cov folder
setwd("~/wy/cov30s_WY630_737/clip")
sr2 <- writeRaster(sNAf, paste0(names(sNAf), ".tif"),overwrite=TRUE)

#now read and stack updated covs
# read in raster file names as a list
sList=list.files(getwd(), pattern="tif$", full.names = FALSE)
sList

# stack
specStack <- rast(sList)
names(specStack) <- sList
names(specStack)

#################################
# repeat process to bring in terrain covs, fix NAs, and stack
setwd("~/wy/cov30t_WY630_737")

# read in raster file names as a list
tList=list.files(getwd(), pattern="tif$", full.names = FALSE)
tList
tStack <- rast(tList)
#look for NAs
NAt <-global(tStack, fun="isNA")
NAt

##### fix NAs #####
#first move the layers identified as having extra NA values to a different folder

setwd("~/wy/cov30t_WY630_737/NAs")

NAlist=list.files(getwd(), pattern="tif$", full.names = FALSE)
NAlist
NAStack <- rast(NAlist)

#look at number of NAs for reference
NAs <- global(NAStack, fun="isNA")
NAs

#use focal filter to remove NA values (may not remove all of them but don't want to set the window too big)
NAf <- focal(NAStack, w=5, fun="mean", na.policy="only", fillvalue=NA, expand=FALSE, silent=FALSE, overwrite=FALSE)

#check number of NAs and compare to make sure they've gone down
newNAs <- global(NAf, fun="isNA")
NAs
newNAs

#write the rasters back to cov folder
setwd("~/wy/cov30t_WY630_737")
r2 <- writeRaster(NAf, paste0(names(NAf), ".tif"),overwrite=TRUE)