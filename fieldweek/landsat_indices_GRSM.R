### Landsat 8 covariate development
# Travis Nauman and Dave White



## Load packages
required.packages <- c("raster", "sp", "rgdal", "RStoolbox", 
                       "snow", "snowfall","parallel", "itertools","doParallel")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

## Increase actuve memory useable by raster package
memory.limit(110000)
rasterOptions(maxmemory = 5e+08, chunksize = 3e+07)


#### Extract individual bands from raster stacks.
# added a constant to each band. there were some issues in calculating the indices, where it was 
# returning no data cells. 

setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/on/")

ls8on <- stack("ls8_sr_on.tif")
ls8b1 <- ls8on$ls8_sr_on.1+1
writeRaster(ls8b1, filename = "ls8on1.tif", format = "GTiff", overwrite = TRUE)
ls8b2 <- ls8on$ls8_sr_on.2+1
writeRaster(ls8b2, filename = "ls8on2.tif", format = "GTiff", overwrite = TRUE)
ls8b3 <- ls8on$ls8_sr_on.3+1
writeRaster(ls8b3, filename = "ls8on3.tif", format = "GTiff", overwrite = TRUE)
ls8b4 <- ls8on$ls8_sr_on.4+1
writeRaster(ls8b4, filename = "ls8on4.tif", format = "GTiff", overwrite = TRUE)
ls8b5 <- ls8on$ls8_sr_on.5+1
writeRaster(ls8b5, filename = "ls8on5.tif", format = "GTiff", overwrite = TRUE)
ls8b6 <- ls8on$ls8_sr_on.6+1
writeRaster(ls8b6, filename = "ls8on6.tif", format = "GTiff", overwrite = TRUE)
ls8b7 <- ls8on$ls8_sr_on.7+1
writeRaster(ls8b7, filename = "ls8on7.tif", format = "GTiff", overwrite = TRUE)
ls8b8 <- ls8on$ls8_sr_on.8+1
writeRaster(ls8b8, filename = "ls8on8.tif", format = "GTiff", overwrite = TRUE)
ls8b9 <- ls8on$ls8_sr_on.9
writeRaster(ls8b9, filename = "ls8on9.tif", format = "GTiff", overwrite = TRUE)


setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/off/")

ls8off <- stack("ls8_sr_off.tif")
ls8b1 <- ls8off$ls8_sr_off.1+1
writeRaster(ls8b1, filename = "ls8off1.tif", format = "GTiff", overwrite = TRUE)
ls8b2 <- ls8off$ls8_sr_off.2+1
writeRaster(ls8b2, filename = "ls8off2.tif", format = "GTiff", overwrite = TRUE)
ls8b3 <- ls8off$ls8_sr_off.3+1
writeRaster(ls8b3, filename = "ls8off3.tif", format = "GTiff", overwrite = TRUE)
ls8b4 <- ls8off$ls8_sr_off.4+1
writeRaster(ls8b4, filename = "ls8off4.tif", format = "GTiff", overwrite = TRUE)
ls8b5 <- ls8off$ls8_sr_off.5+1
writeRaster(ls8b5, filename = "ls8off5.tif", format = "GTiff", overwrite = TRUE)
ls8b6 <- ls8off$ls8_sr_off.6+1
writeRaster(ls8b6, filename = "ls8off6.tif", format = "GTiff", overwrite = TRUE)
ls8b7 <- ls8off$ls8_sr_off.7+1
writeRaster(ls8b7, filename = "ls8off7.tif", format = "GTiff", overwrite = TRUE)
ls8b8 <- ls8off$ls8_sr_off.8+1
writeRaster(ls8b8, filename = "ls8off8.tif", format = "GTiff", overwrite = TRUE)
ls8b9 <- ls8off$ls8_sr_off.9
writeRaster(ls8b9, filename = "ls8off9.tif", format = "GTiff", overwrite = TRUE)


######################################################################




## Development of Landsat8 leaf off covariates

## set working directory for landsat 8 leaf off data
setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/off/")

## load ls8 data as a raster stack
r.list=list.files(getwd(), pattern="tif$", full.names = FALSE)
ls8.off <- stack(r.list)

## get individual bands
b2off <- ls8.off$ls8off2
b3off <- ls8.off$ls8off3
b4off <- ls8.off$ls8off4
b5off <- ls8.off$ls8off5
b6off <- ls8.off$ls8off6
b7off <- ls8.off$ls8off7

## Normalized Difference index function
nd_fn <- function(bd1,bd2) {ind <- (bd1 - bd2)/(bd1 + bd2)*1000
return(ind)
}

## set up cluster
beginCluster(11,type='SOCK')

#### set workspace for leaf off covariates
setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/off/cov/")


## ratio calcs for leaf off
# note the  compression and datatypes are commented out, check the data ranges of each raster produced and select the appropriate datatype. Use ?raster::datatype to see the different choices.
s4t2off <- stack(b4off,b2off) 
clusterR(s4t2off, overlay, args=list(fun=nd_fn),progress='text',filename="nd4t2off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s4t3off <- stack(b4off,b3off) 
clusterR(s4t3off, overlay, args=list(fun=nd_fn),progress='text',filename="nd4t3off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s4t7off <- stack(b4off,b7off) 
clusterR(s4t7off, overlay, args=list(fun=nd_fn),progress='text',filename="nd4t7off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s5t6off <- stack(b5off,b6off) 
clusterR(s5t6off, overlay, args=list(fun=nd_fn),progress='text',filename="nd5t6off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s6t2off <- stack(b6off,b2off) 
clusterR(s6t2off, overlay, args=list(fun=nd_fn),progress='text',filename="nd6t2off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s6t5off <- stack(b6off,b5off)
clusterR(s6t5off, overlay, args=list(fun=nd_fn), progress='text', filename="nd6t5off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"), dataType='INT2S')
s7t4off <- stack(b7off,b4off) 
clusterR(s7t4off, overlay, args=list(fun=nd_fn),progress='text',filename="nd7t4off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s7t6off <- stack(b7off,b6off) 
clusterR(s7t6off, overlay, args=list(fun=nd_fn),progress='text',filename="nd7t6off.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

## Other normalized indices
# calcareous sediment index
calsed.off <- stack(b6off, b3off)
clusterR(calsed.off, overlay, args=list(fun=nd_fn),progress='text',filename="calsedoff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

# ndvi - normalized difference vegitation index
ndvi.off <- stack(b5off, b4off)
clusterR(ndvi.off, overlay, args=list(fun=nd_fn),progress='text',filename="ndvioff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

# evi - enhanced vegtation index - for areas with a lot of biomass
evi_fn <- function(bd2,bd4,bd5) {ind <- 2.5*((bd5-bd4)/(bd5+6*bd4-7.5*bd2+1))
return(ind)
}
evi.off <- stack(b2off,b4off,b5off)
clusterR(evi.off, overlay, args=list(fun=evi_fn),progress='text',filename="evioff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')


# msavi2 - modified soil adjusted vegitation index - for areas of sparce vegetation
msavi_fn <- function(bd1,bd2) {ind <- ((2*bd1+1)-(sqrt((2*bd1+1)^2)-8*(bd1-bd2)))/2
return(ind)
}
msavi.off <- stack(b5off, b4off)
clusterR(msavi.off, overlay, args=list(fun=msavi_fn),progress='text',filename="msavioff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

# rock outcrop index
rock.off <- stack(b6off, b4off)
clusterR(rock.off, overlay, args=list(fun=nd_fn),progress='text',filename="rockoff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')


## Tasseled Cap
off.tc <- tasseledCap(ls8.off[[c(2:7)]], sat = "Landsat8OLI")

# extract TC
off.brightness <- off.tc$brightness
off.greenness <- off.tc$greenness
off.wetness <- off.tc$wetness

# write TC rasters
writeRaster(off.brightness, filename = "TCbrightoff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress='text')
writeRaster(off.greenness, filename = "TCgreenoff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress='text')
writeRaster(off.wetness, filename = "TCwetoff.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress='text')





## Principal Component Analysis
off.pca <- rasterPCA(ls8.off[[c(2:7)]])

# get individual PCs
offpc1 <- off.pca$map$PC1
offpc2 <- off.pca$map$PC2
offpc3 <- off.pca$map$PC3

# save rasters
writeRaster(offpc1, overwrite=F,filename="offpc1.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress="text")
writeRaster(offpc2, overwrite=F,filename="offpc2.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress="text")
writeRaster(offpc3, overwrite=F,filename="offpc3.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress="text")


# end parallel cluster
endCluster()

# clear working environment and memory
rm(list=ls())
gc()

##################################################################################################
## Development of Landsat8 leaf on covariates


## set working directory for landsat 8 leaf on data
setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/on/")

## load ls8 data as a raster stack
r.list=list.files(getwd(), pattern="tif$", full.names = FALSE)
ls8.on <- stack(r.list)

## get individual bands
b2on <- ls8.on$ls8on2
b3on <- ls8.on$ls8on3
b4on <- ls8.on$ls8on4
b5on <- ls8.on$ls8on5
b6on <- ls8.on$ls8on6
b7on <- ls8.on$ls8on7

## Normalized Difference index function
nd_fn <- function(bd1,bd2) {ind <- (bd1 - bd2)/(bd1 + bd2)*1000
return(ind)
}

## set up cluster
beginCluster(11,type='SOCK')

#### set workspace for leaf on covariates
setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/on/cov/")


## ratio calcs for leaf on
# note the  compression and datatypes are commented out, check the data ranges of each raster produced and select the appropriate datatype. Use ?raster::datatype to see the different choices.
s4t2on <- stack(b4on,b2on) 
clusterR(s4t2on, overlay, args=list(fun=nd_fn),progress='text',filename="nd4t2on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s4t3on <- stack(b4on,b3on) 
clusterR(s4t3on, overlay, args=list(fun=nd_fn),progress='text',filename="nd4t3on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s4t7on <- stack(b4on,b7on) 
clusterR(s4t7on, overlay, args=list(fun=nd_fn),progress='text',filename="nd4t7on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s5t6on <- stack(b5on,b6on) 
clusterR(s5t6on, overlay, args=list(fun=nd_fn),progress='text',filename="nd5t6on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s6t2on <- stack(b6on,b2on) 
clusterR(s6t2on, overlay, args=list(fun=nd_fn),progress='text',filename="nd6t2on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s6t5on <- stack(b6on,b5on)
clusterR(s6t5on, overlay, args=list(fun=nd_fn), progress='text', filename="nd6t5on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"), dataType='INT2S')
s7t4on <- stack(b7on,b4on) 
clusterR(s7t4on, overlay, args=list(fun=nd_fn),progress='text',filename="nd7t4on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')
s7t6on <- stack(b7on,b6on) 
clusterR(s7t6on, overlay, args=list(fun=nd_fn),progress='text',filename="nd7t6on.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

## Other normalized indices
# calcareous sediment index
calsed.on <- stack(b6on, b3on)
clusterR(calsed.on, overlay, args=list(fun=nd_fn),progress='text',filename="calsedon.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

# ndvi - normalized difference vegitation index
ndvi.on <- stack(b5on, b4on)
clusterR(ndvi.on, overlay, args=list(fun=nd_fn),progress='text',filename="ndvion.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

# evi - enhanced vegtation index - for areas with a lot of biomass
evi_fn <- function(bd2,bd4,bd5) {ind <- 2.5*((bd5-bd4)/(bd5+6*bd4-7.5*bd2+1))
return(ind)
}
evi.on <- stack(b2on,b4on,b5on)
clusterR(evi.on, overlay, args=list(fun=evi_fn),progress='text',
         filename="evion.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')


# msavi2 - modified soil adjusted vegitation index - for areas of sparce vegetation
msavi_fn <- function(bd1,bd2) {ind <- ((2*bd1+1)-(sqrt((2*bd1+1)^2)-8*(bd1-bd2)))/2
return(ind)
}
msavi.on <- stack(b5on, b4on)
clusterR(msavi.on, overlay, args=list(fun=msavi_fn),progress='text',filename="msavion.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')

# rock outcrop index
rock.on <- stack(b6on, b4on)
clusterR(rock.on, overlay, args=list(fun=nd_fn),progress='text',filename="rockon.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S')


## Tasseled Cap
on.tc <- tasseledCap(ls8.on[[c(2:7)]], sat = "Landsat8OLI")

# extract TC
on.brightness <- on.tc$brightness
on.greenness <- on.tc$greenness
on.wetness <- on.tc$wetness

# write TC rasters
writeRaster(on.brightness, filename = "TCbrighton.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress='text')
writeRaster(on.greenness, filename = "TCgreenon.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress='text')
writeRaster(on.wetness, filename = "TCweton.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress='text')



## Principal Component Analysis
on.pca <- rasterPCA(ls8.on[[c(2:7)]])

# get individual PCs
onpc1 <- on.pca$map$PC1
onpc2 <- on.pca$map$PC2
onpc3 <- on.pca$map$PC3

# save rasters
writeRaster(onpc1, overwrite=F,filename="onpc1.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress="text")
writeRaster(onpc2, overwrite=F,filename="onpc2.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress="text")
writeRaster(onpc3, overwrite=F,filename="onpc3.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT2S', progress="text")


# end parallel cluster
endCluster()


# clear working environment and memory
rm(list=ls())
gc()




##################################################################################################


#### Comparisons of leaf on to leaf off ###

## load rasters
#### set workspace for leaf on covariates
setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/on/cov/")

ndvion <- raster("ndvion.tif")
msavion <- raster("msavion.tif")
tcgon <- raster("TCGreenon.tif")

setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/off/cov/")

ndvioff <- raster("ndvioff.tif")
msavioff <- raster("msavioff.tif")
tcgoff <- raster("TCGreenoff.tif")

## Veg compare function
# originally the function had a multiplication factor of 1000 (bd1-bd2)*1000
# this caused errors because the data values were outside the range of the datatype
# could have changed the data type, but instead removed the multiplication factor
vc_fn <- function(bd1,bd2) {ind <- (bd1 - bd2) 
return(ind)
}

## Veg index differences
setwd("D:/DSM_Field_Week_GRSM/CurrentCovariates/ls8/")


## set up cluster
beginCluster(11,type='SOCK')


# compare ndvi
ndvind <- stack(ndvion, ndvioff) 
clusterR(ndvind, overlay, args=list(fun=vc_fn),progress='text',
         filename="ndvi_c.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),
         #datatype='INT2S', na.omit=TRUE)

# compare msavi
msavind <- stack(msavion,msavioff) 
clusterR(msavind, overlay, args=list(fun=vc_fn),progress='text',
         filename="msavi_c.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),
         #datatype='INT2S', na.omit=TRUE)

# compare greenness
tcgnd <- stack(tcgon, tcgoff) 
clusterR(tcgnd, overlay, args=list(fun=vc_fn),progress='text',
         filename="tc_green_c.tif")#, options=c("COMPRESS=DEFLATE", "TFW=YES"),
         #datatype='INT4S', na.omit=TRUE)



endCluster()

######################################################################





## Notes

# evi produced strange results: removed from dataset

# calsedoff, 4/2off, 4/3off, 4/7off, 6/2off, 7/4off, offpc3,
# and rockoff, had path line artifact: removed from dataset





