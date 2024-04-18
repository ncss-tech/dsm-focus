# Crop rasters to ensure similar extents

# 3/5/24
# Dave White

# load and install package"s
required.packages <- c("sf", "terra")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# set working directory for spectral data
setwd("~/data/8-vic/specCov30")

# read in raster file names as a list
sList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# bring in the first raster from the list as a spatraster
rSpec <- rast(sList[[1]])

# repeat process to bring in elev data
setwd("~/data/8-vic/cov30")

# read in raster file names as a list
tList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# bring in the first raster from the list as a spatraster
rDem <- rast(tList[[1]])


#### compare extents of single rasters
ext(rSpec)
ext(rDem)

ncol(rSpec)
ncol(rDem)

nrow(rSpec)
nrow(rDem)

# spectral covariates are larger than terrain covariates

###### clip spectral covariates to terrain covairates
# set a working directory to the covariates to be clipped
setwd("~/data/8-vic/specCov30")

# set the working directory of the covariates to be clipped to a variable
dir <- "~/data/8-vic/specCov30" #directory to covariates to clip

# set the name of the folder to store the clipped rasters
output <- "clip" #directory to create to store clipped rasters
# create output folder named above
dir.create(output, FALSE, FALSE)

# input file names
inf <- list.files(dir, pattern="tif$", full.names=F)

# create output file names and directory list of strings 
outf <- paste0(dir,"/",output,"/", inf)

# extent, using the dem as the clipping extent, could be shapefile
e <- rDem # extent to clip to

for (i in 1:length(inf)) { # looping through the list of raster names
  b <- rast(inf[i]) # bring in raster as spatRaster using terra
  crop(b, e, filename=outf[i]) #crop the spat raster to the clip extent, and write to file named outf
}

# Check the extents of the clipped data

# bring in clipped rasters stack with dem rasters

# set working directory for spectral data
setwd("~/data/8-vic/specCov30/clip")

# read in raster file names as a list
sList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# bring in the first raster from the list as a spatraster
rSpec <- rast(sList[[1]])


# repeat process to bring in elev data
setwd("~/data/8-vic/cov30")

# read in raster file names as a list
tList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# bring in the first raster from the list as a spatraster
rDem <- rast(tList[[1]])

#### compare extents of single rasters
ext(rSpec)
ext(rDem)

ncol(rSpec)
ncol(rDem)

nrow(rSpec)
nrow(rDem)

compare(rSpec, rDem, "==")

# stack rasters if extents match
rStack <- c(rSpec, rDem)

# view rasters
plot(rStack)
