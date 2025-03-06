#crop rasters to ensure similar extents
# 3/13/2024
# Jessica Philippe (base code from Dave White)

# load and install package"s
required.packages <- c("sf", "terra")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# set working directory for spectral data
setwd("~/wy/cov30s_WY630_737")
getwd()

#read in raster file names as a list
sList=list.files(getwd(), pattern="tif$", full.names=FALSE)
sList

#bring in the first raster from the list as a spatraster
rSpec <- rast(sList[[1]])
rSpec
plot(rSpec)

#repeat process to bring in elev data
setwd("~/wy/cov30t_WY630_737")
getwd()

# read in raster file names as a list
tList=list.files(getwd(), pattern="tif$", full.names = FALSE)
tList

# bring in the first raster from the list as a spatraster
rTer <- rast(tList[[109]])
rTer
plot(rTer)


#### compare extents of single rasters
ext(rSpec)
ext(rTer)

ncol(rSpec)
ncol(rTer)

nrow(rSpec)
nrow(rTer)

# spectral covariates are larger than terrain covariates

###### clip spectral covariates to terrain covairates

# set a working directory to the covariates to be clipped
setwd("~/wy/cov30s_WY630_737")

# set the working directory of the covariates to be clipped to a variable
dir <- "~/wy/cov30s_WY630_737" #directory to covariates to clip

# set the name of the folder to store the clipped rasters
output <- "clip" #directory to create to store clipped rasters
# create output folder named above
dir.create(output, FALSE, FALSE)

# input file names
inf <- list.files(dir, pattern="tif$", full.names=F)

# create output file names and directory list of strings 
outf <- paste0(dir,"/",output,"/", inf)

# extent, using the dem as the clipping extent, could be shapefile
e <- rTer # extent to clip to

for (i in 1:length(inf)) { # looping through the list of raster names
  b <- rast(inf[i]) # bring in raster as spatRaster using terra
  crop(b, e, filename=outf[i]) #crop the spat raster to the clip extent, and write to file named outf
}

# Check extents of clipped data

#set working directory to clipped data
setwd("~/wy/cov30s_WY630_737/clip")

#read in raster file names as a list
cList=list.files(getwd(), pattern="tif$", full.names = FALSE)
cList

#bring in one raster from the list as a spatraster
rClip <- rast(cList[[2]])
plot(rClip)

#### compare extents of single rasters
ext(rClip)
ext(rTer)

ncol(rClip)
ncol(rTer)

nrow(rClip)
nrow(rTer)

compare(rClip, rTer, "==")

#stack rasters if extents match
rStack <- c(rClip, rTer)

#view rasters
plot(rStack)
