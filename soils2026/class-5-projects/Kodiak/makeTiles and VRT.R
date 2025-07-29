
#
#   Mark Twain Other  12/11/24
#
#   stack covariates, subset based on correlation reduction, make tiles, create vrt

# Dave White


# load and install packages
required.packages <- c("terra","sf","doParallel")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)



# bring in covariate data and create a stack

# set working directory to dem covs
setwd("~/data/cov/")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# create stack of covs
rStack <- rast(rList)

rList
names(rStack)

#assign the names to the stack
names(rStack) <- gsub(".tif", "", list.files(getwd(), pattern="tif$", full.names = FALSE))

names(rStack)



###



setwd("~/data/tiles")
#numTiles <- 11 # numTiles^2 should <= the number of cores - in this case I have 123 cores available and 11x11 would give 121 tiles
numTiles <- 9
x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(rStack))
tl <- makeTiles(rStack, x, overwrite=T, extend=F)
tl <- list.files("~/data/tiles", pattern=".tif$", full.names=TRUE)
gc()


# make a virtual raster dataset from the tiles
vrtfile <- paste0(tempfile(), ".vrt")
v <- vrt(tl, vrtfile, filename="~/data/tiles/cov.vrt", overwrite=T, set_names = T)

# test to 

rasterStack.vrt <- rast("~/data/tiles/cov.vrt")

plot(rasterStack.vrt$backSlp)

