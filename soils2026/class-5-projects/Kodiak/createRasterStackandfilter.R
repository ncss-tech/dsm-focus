# read rasters from folder then apply a focal filter only to the cells that contain NA values 
# and write out new filtered raster

# Dave White


# load and install packages
required.packages <- c("terra", "foreach", "doParallel")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=F)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# set working directory to dem covs
setwd("~/data/cov10m")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)


#setup parallel backend to use all but 4 cores
cl<-detectCores()-4
registerDoParallel(cl)

#loop
ls<-foreach(i = 1:length(rList)) %dopar% {
  raster <- rast(rList[i])
  name <- gsub(".tif", "", rList[i])
  f.r <- focal(raster, w=5, fun= mean, 
               na.policy="only", expand=F)
  writeRaster(f.r, 
              paste0("~/data/cov/", 
                     name, ".tif"))
}

