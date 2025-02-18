# load and install packages
required.packages <- c("terra", "caret","sf","doParallel")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

# Data reduction Steps
# 


# there are a large number of spectral covs, need to reduce by correlation first

#Read in raster data for spectral covs
rList <- list.files("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/covs/fullStack/spec/", pattern="tif$", full.names = F)
rLnames <- gsub(".tif", "", rList)

rList <- list.files("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/covs/fullStack/spec/", pattern="tif$", full.names = T)

rs.s <- rast(rList)
names(rs.s) <- rLnames
names(rs.s)

size = round(ncol(rs.s) *length(names(rs.s))/10)

rs.df <- spatSample(rs.s, size, method="regular", xy=F, as.df=T)

rs.df <- na.omit(rs.df)

#correlation reduction
corMat <- cor(rs.df)

highCorr <- findCorrelation(corMat, cutoff=0.97)
highCorr

# remove covariates with high degree of correlation
df <- if(length(highCorr) > 0){
  rs.df[, -highCorr]
} else {
  rs.df
}



# subset rStack to crate a new raster stack of reduced covariates for use in other applications
rs.s2 <- subset(rs.s, names(df))
names(rs.s2)

# I am going to add back in mst evi22 to the stack because it was chosen for clustering
rList[92]

evi <- rast(rList[92])

# bring in dem cov data and stack all together
rList <- list.files("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/covs/fullStack/dem/", pattern="tif$", full.names = F)
rLnames <- gsub(".tif", "", rList)

rList <- list.files("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/covs/fullStack/dem/", pattern="tif$", full.names = T)

rs.d <- rast(rList)
names(rs.d) <- rLnames
names(rs.d)


# combine all 3 spat raster objects into one stack

rStack <- c(rs.s2, evi, rs.d)
names(rStack)

# subset rasterstack
length(names(rStack))

# make tiles 
setwd("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/covs/fullStack/tiles/")
gc()
numTiles <- 16
x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(rStack)) # create a grid of tiles 16X16 # could use a shape file instead

tl <- makeTiles(rStack, x, overwrite=T, gdal=c("COMPRESS=DEFLATE", "TFW=YES"), names=names(rStack))


tl <- list.files("~/data/sw-forests/covs/fullStack/tiles", pattern=".tif$", full.names=T)


# make a virtual raster dataset from the tiles
v1 <- vrt(tl, filename="~/data/sw-forests/covs/fullStack/tiles/covs.vrt", overwrite=T, set_names = T)





