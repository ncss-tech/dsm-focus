# 8-vic covariate stack wrangling 

# clip spectral stack to dem, the spectal stack is a bit larger

#3/5/24


# load and install package"s
required.packages <- c("sf", "terra")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)




# set working directory for spectral data
setwd("~/data/8-vic/specCov30")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# stack
specStack <- rast(rList)

# repeat process to bring in elev data
setwd("~/data/8-vic/cov30")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# stack
demStack <- rast(rList)


# check extents 
ext(demStack)
ncol(demStack)
nrow(demStack)
ext(specStack)
ncol(specStack)
nrow(specStack)

specStack <- resample(specStack, demStack, method = 'bilinear', threads=T)



names(rStack)

setwd("~/data/8-vic/data")

# write stack to save
writeRaster(specStack, "specStack.tif", filetype ="GTiff", overwrite = T)

# to combine the stacks
#rStack <- c(specStack, demStack)

