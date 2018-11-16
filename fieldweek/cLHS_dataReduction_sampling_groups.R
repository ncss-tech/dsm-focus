# cLHS stratified by sampling group with covariate reduction
# Dave White
#
#
#
# this script takes a raster stack, exracts based on sampling group boundary
# then performes a series of data reuction techniques to reduce the number of covariates while maximixing the variability of the total population that the selected set of covariates represent
#
# finally cLHS is used to generate a stratifified random sample of points across the subset sampling group



# load and install required packages
required.packages <- c("raster", "sp", "rgdal","snow", "snowfall","parallel", "itertools","doParallel","clhs", "caret", "corrplot", "psych", "foreach")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# increase active memory useable by raster package
memory.limit(size = 110000)
rasterOptions(maxmemory = 5e+08, chunksize = 3e+07)


# Section 1 - load rasters and extract by sampling group
# due to the large number of rasters, they will be added to the raster environment based on type : DEM vs LS8

# set directory that contains the DEM rasters 
# *** pay attention to the / below
setwd("C:/rwork/cov/dem/")

# read in raster layers to create raster stack
r.list=list.files(getwd(), pattern="tif$", full.names = FALSE)
r.list

#create raster stack of DEM covariates
r.stack.dem <- stack(r.list)
names(r.stack.dem)

# set directory that contains the ls8 rasters
setwd("C:/rwork/cov/ls8/")

# read in raster layers to create raster stack
r.list=list.files(getwd(), pattern="tif$", full.names = FALSE)
r.list

#create raster stack
r.stack.ls8 <- stack(r.list)
names(r.stack.ls8)

# read in shapefile to clip rasters
# NOTE - this is your sampling group!!! 
setwd("C:/rwork/sampling_groups/") # set the directory of individual shape files

poly <- readOGR("16.shp") # reads in shapefile

# crop raster stack by group - this clips the raster stack to the extent of the polygon
dem.crop <- crop(r.stack.dem, poly) 
ls8.crop <- crop(r.stack.ls8, poly)

# converts polygon to a raster for faster masking of raster stacks
poly.r <- rasterize(poly, dem.crop)

# mask raster stacks
r.dem <- mask(dem.crop, poly.r)
r.ls8 <- mask(ls8.crop, poly.r)

# combine both raster stacks into one for use later
r.stack <- stack(r.dem, r.ls8)

# checking one of the rasters visually
plot(r.stack$sagawi)

# clean up working environment
to.remove <- ls()
matches <- c("r.dem", "r.ls8", "poly", "poly.r", "r.stack")#this is where you put what you want to keep
to.remove <- c(to.remove[!grepl(paste0(matches, collapse = "|"), to.remove)], "to.remove")
rm(list=to.remove)
gc()

# before filtering we need to covert the raster stack to a dataframe this process is time consuming and ram intensive on large datasets, which is why we mask the raster stacks by sampling group. the covariate selection process implemeted below relies on dataframes for analysis.

# convert dem raster stack to dataframe, remove coords and NA's
dem.df <- as.data.frame(r.dem, xy=FALSE, na.rm=TRUE)

# convert ls8 raster stack to dataframe, remoe coords and NA's
ls8.df <- as.data.frame(r.ls8, xy=FALSE, na.rm=TRUE)

# check data frame size to make sure they can be combined
nrow(dem.df)
nrow(ls8.df)

# combine data frames into on for feature selection/covariate reduction
all.df <- cbind(dem.df, ls8.df)
nrow(all.df)

#########################################################################

# Section 2 - Data Reduction Methods


# near Zero Variance filtering
#diagnoses predictors that have one unique value (i.e. are zero variance predictors) or predictors that are have both of the following characteristics: they have very few unique values relative to the number of samples and the ratio of the frequency of the most common value to the frequency of the second most common value is large.
# removes covariates whose variance is near or equal to zero
# this process is parallelized to enable faster processing
# the foreach package to run in parallel
cl <- makeCluster(detectCores()-1) # makes a cluster using all but 1 cpu cores
registerDoParallel(cl)
# the actual zeroVar function, creates a vector containing which covariates should be removed
zeroVar <- nearZeroVar(all.df, allowParallel = TRUE)
stopCluster(cl)


# removing unused items from memory
gc()

# check the zeroVar object
head(zeroVar)
# *note integer(0), means that there are no covariates to be removed

# look for the covariates being removed
names(all.df)

# remove covariates with zeroVar
all.df <- if(length(zeroVar) > 0){
  all.df[, -zeroVar]
} else {
  all.df
}

# verify the covariates were removed
names(all.df)



# filtering by correlation
# a correlation matrix is determined and covariates with high degree of correlation are returned

# create correlation matrix
cor.mat <- cor(all.df)

# visually examin the correlation matrix
corrplot(cor.mat)

# find high degree of correlation, cutoff is the threshold to set. If cutoff = 0.85 then covariates > or = 85 correlated are removed
highCorr <- findCorrelation(cor.mat, cutoff = 0.85)

# the number of highly correlated covariates
length(highCorr)

# if this number seems to aggresive, change the cutoff to a larger number and rerun

# remove covariates with high degree of correlation
all.df <- if(length(highCorr) > 0){
  all.df[, -highCorr]
} else {
  all.df
}



# Principal Component Analysis
# creates a PCA computes the loading factors to determine the degree of correlation between PC and covariates then selects covariates that account for 95 percent of the overall population.


#iPCA function, returns the names of covariates to keep
iPCA <- function(all.df){
  # creates a correlation matrix
  #cor.mat <- cor(all.df)
  # runs the principal component analysis
  pca <- prcomp(all.df, scale = TRUE, center = TRUE)
  # obtain the eigan vectors
  evectors <- pca$rotation
  # for use converting rows to a matrix for calculations
  len <- length(all.df)
  # obtain the eigan values
  evalue <- pca$sdev^2
  evalues <- matrix((evalue), nrow = len, ncol = len, byrow = TRUE)
  # obtain the stdev
  sdev <- pca$sdev
  sdev <- matrix((sdev), nrow = len, ncol = len, byrow = TRUE)
  # compute loading factors and convert to a data frame
  lf <- as.data.frame(abs((evectors*(sqrt(evalues)))/(sdev)))
  # add column and sum up the loading factors
  lf$loadings <- rowSums(lf[, c(1:len)])
  # sort by loadings
  lf <- lf[order(-lf$loadings), ]
  # sort by loadings
  lf <- lf[order(-lf$loadings), ]
  # now we need to determine which covariates to drop
  esum <- sum(evalue)
  len <- length(evalue)
  cum.var <- matrix(evalue, nrow = len, byrow = TRUE)
  cum.var <- as.data.frame.matrix(cum.var)
  esum <- rep(esum, len)
  cum.var$esum <- esum
  cum.var$var <- cum.var$V1 / cum.var$esum
  cum.var$cumvar <- cumsum(cum.var$var)
  
  cum.var.len <- nrow(cum.var) # starting number of covariates
  #pc.len1 <- nrow(filter(cum.var, cumvar <= .95)) #number of covariates to keep
  pc.len <- length(which(cum.var$cumvar <= .951))
  # narrowing down the list
  lf.subset <- lf[1:pc.len, ]
  stack.names <- rownames(lf.subset)
  all.df <- (all.df[,c(stack.names)])
  return(all.df)
}    



# run the function on some data
all.df.2 <- iPCA(all.df)


# subset r.stack to crate a new raster stack of reduced covariates for use in other applications
r.stack.clhs <- subset(r.stack, names(all.df.2))

# view names
names(r.stack.clhs)
names(all.df.2)



########## cLHS - conditiond latin hypercube sample - generates sampling points

#load the cost raster and add to r.stack

setwd("C:/rwork/")

cost <- raster("cost.tif")

# crop raster stack by group
cost.crop <- crop(cost, poly) #takes a few minutes crops the raster stack to the extent of the polygon)

r.cost <- mask(cost.crop, poly.r)

plot(r.cost)

# add cost to the raster stack
r.stack.clhs$cost <- r.cost

names(r.stack.clhs)

#convert raster stack to spatial points data frame
s <- as(r.stack.clhs, "SpatialPointsDataFrame")

# cost surface is activated via `cost` argument
set.seed(19) # this allows the output to be reproducable, if you want a randomly generated set remove this line. if you want a different set of sampling points change the value of the seed - here it is 19 change to any other number

s.clhs <- clhs(s, size = 10, progress = TRUE, iter = 10000, cost = 'cost', simple = FALSE)#simple is set to false to obtain a clhs object so that we can use the build in plotting functions

# generate plots
plot(s.clhs, mode = "obj")
plot(s.clhs, mode = "box")
plot(s.clhs, mode = "dens")


# check visually on the cost raster
par(mar = c(1,1,1,1))
plot(r.stack.clhs$cost, axes=FALSE)
points(s.clhs$sampled_data, bg = 'red', pch=21)


subset.idx <- s.clhs$index_samples

# save cLHS points to shp 
# NOTE - rename your file to reflect the sampling group
writeOGR(s[subset.idx, ], dsn = 'C:/rwork', layer = 'cLHS_16', driver = 'ESRI Shapefile', overwrite_layer = TRUE)




