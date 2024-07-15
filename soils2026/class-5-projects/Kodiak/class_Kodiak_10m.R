# Updated Modeling Script for Kodiak Island
# Dave White


# load and install packages
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow", "foreign", "foreach")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=F)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)



# bring in covariate data and create a stack

# set working directory to dem covs
setwd("~/data/cov10m")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# create a raster stack of dem covs
rStack <- rast(rList)

rm(rList)

# Bring in training data points 
# read in shapefile 
# bring in pedon data

#set working directory to training data.
setwd("~/data")

pts <- read_sf("trainData071224.shp")

# extract raster covariate values at each training point for the all.pts dataset
pts.sv <- terra::extract(rStack, pts, xy=F, bind=TRUE, na.rm=TRUE) 

# convert SpatVector to sf 
all.pts <- sf::st_as_sf(pts.sv) 

names(all.pts)

# remove unwanted cols
all.pts <- all.pts[-c(1:4)]
colnames(all.pts)[1] <- "class"
names(all.pts)
all.pts$class <- as.factor(all.pts$class)
names(all.pts)
#
levels(all.pts$class)


# convert sf to a dataframe
pts.all <- as.data.frame(all.pts)[-c(208)]
names(pts.all)


# remove any observations with NAs
comp <- pts.all[complete.cases(pts.all),]

# convert Class col to a factor
#comp$Class <- as.factor(comp$Class)
is.factor(comp$class)
levels(comp$class)
summary((comp$class))

# There are a few classes with < 2 observations remove those classes

length(levels(comp$class))

nlevels(comp$class)

comp = comp[comp$class %in% names(table(comp$class)) [table(comp$class) >2],]
levels(comp$class)
summary(comp$class)
comp <- droplevels(comp)
levels(comp$class)
summary(comp$class)

length(levels(comp$class))
gc()
#---------------------------------------------------------------------------
# Recursive Feature Selection (RFE)


#subsets <- c(1,10,25,50, 75, 100, 150, 200)
subsets <- c(1,10,30,50,70,95,100,110,150,210)

# set seeds to get reproducible results when running the process in parallel
set.seed(238)
seeds <- vector(mode = "list", length=112)
for(i in 1:111) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[112]] <- sample.int(1000, 1)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       seeds = seeds, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE
detectCores()# number of cores
cores <- 60
cl <- makeCluster(cores) # base R only recognizes 128 cores and about 5 need to be left for OS
registerDoParallel(cl)
set.seed(9)
rfe <- rfe(x = comp[,-c(1)],
           y = comp$class,
           sizes = subsets,
           rfeControl = ctrl.RFE,
           allowParallel = TRUE
)
stopCluster(cl)             
gc()

# Look at the results
rfe
plot(rfe)



rfe$fit$confusion
rfe$results
plot(rfe) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rfe, metric="Kappa", main='RFE Kappa')
plot(rfe, metric="Accuracy", main='RFE Accuracy')

# see list of predictors
predictors(rfe)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *
rfe

# take the accuracy and subtract the accuracySD. look in the results of rf.RFE and find the accuracy that is > or = to this value. this is the number of covariates to use below
#predictors(rfeLand) # top number of covariates

# look at the top number of covariates that are equal to greater than the accuracy minus the accuracySD
#predictors(rf.RFE)[c(1:9,24:26)]

# assign this to a variable
a <- predictors(rfe)[c(1:50)]


# subset the covariate stack and the data frame by the selected covariates the variable a is your selected covariates

# subsed the raser stack to the selected covariates
rsm <- subset(rStack, a)


# subset the data frame points with the number of covariates selected
names(comp)
comp.sub <- (comp[,c("class", a)])


names(comp.sub)
is.factor(comp.sub$class)

#
# determine number of covariates
#length(a)

subsets <- 161
#subsets <- c(1,10, 25, 50, 100, 150, 161)
# set seeds to get reproducable results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, length(1:subsets) + 1)
seeds[[51]] <- sample.int(1000, 1)


# set up the train control
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10,
                           repeats = 5,
                           p = 0.8, #30% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)

# Random Forest - Parallel process
cl <- makeCluster(cores)
registerDoParallel(cl)
set.seed(48)
rfm = train(x = comp.sub[,-c(1)],
            y = comp.sub$class,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE)
stopCluster(cl)
gc()


# Inspect rfFit
rfm$finalModel


#look at overall accuracy, kappa, and SD associated with each mtry value
print(rfm)
rfm$results


# Convert caret wrapper into randomforest model object for raster prediction
rfm <- rfm$finalModel







# make predictions

# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
setwd("~/data/tiles/")


#numTiles <- 11 # numTiles^2 should <= the number of cores - in this case I have 123 cores available and 11x11 would give 121 tiles
numTiles <- round(sqrt(cores), digits = 0)

x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(rsm))

tl <- makeTiles(rsm, x, overwrite=T, extend=T)

cl <- parallel::makeCluster(length(tl)-14)
registerDoParallel(cl)

pred <- foreach(i = 1:length(tl),
                     .packages = c("terra", "randomForest")) %dopar% {
                       pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T))
                       return(pred)
                     }
pred <- do.call(terra::merge,lapply(pred,terra::rast))
plot(pred)
setwd("~/data/results")
# write rasters
writeRaster(pred, overwrite = TRUE, filename = "class.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
# write raster attribute table
#library(foreign)
write.dbf(levels(pred)[[1]], file='class.tif.vat.dbf') # make sure the first part of the file name is exactly the same as the predicted raster




# process probability stacks in smaller chunks
setwd("~/data/8-vic/results/prob")
length(tl)

predProb <- foreach(i = 1:20, #1:lenght(tl)
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob1.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 21:40,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob2.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 41:60,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob3.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 61:80,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob4.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 81:100,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob5.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 101:121,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob6.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

stopCluster(cl)

rm(predProb)

gc()



#set directory to prob tiles
setwd("~/data/8-vic/results/prob")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)

vrtfile <- paste0(tempfile(), ".vrt")
v <- vrt(rList, vrtfile)

## shannon entropy keeps failing, maybe write prob rasters to disk then bring in as vrt and calc shan

cl <- parallel::makeCluster(length(rList))
registerDoParallel(cl)
shan <- foreach(i = 1:length(rList),
                    .packages = c("terra", "aqp")) %dopar% {
                      shan <- wrap(aqp::shannonEntropy(terra::rast(rList[i])))
                      return(shan)
                    }
shan <- do.call(terra::merge,lapply(shan, terra::rast))
plot(shan)

gc()

#normalized shan entropy
b <- length(names(v))
shanNorm1 <- foreach(i = 1:3,
                    .packages = c("terra", "aqp")) %dopar% {
                      shanNorm1 <- wrap(aqp::shannonEntropy(terra::rast(rList[i]), b=b))
                      return(shanNorm1)
                    }
shanNorm1 <- do.call(terra::merge,lapply(shanNorm1, terra::rast))
shanNorm2 <- foreach(i = 4:6,
                     .packages = c("terra", "aqp")) %dopar% {
                       shanNorm2 <- wrap(aqp::shannonEntropy(terra::rast(rList[i]), b=b))
                       return(shanNorm2)
                     }
shanNorm2 <- do.call(terra::merge,lapply(shanNorm2, terra::rast))

shanNorm <- merge(shanNorm1, shanNorm2)


plot(shanNorm)

setwd("~/data/8-vic/results")
writeRaster(shan, overwrite = TRUE, filename = "shan.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
writeRaster(shanNorm, overwrite = TRUE, filename = "shanNorm.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

stopCluster(cl)

# get confusion matrix from model
cm <- confusionMatrix(rfm$predicted, rfm$y)
cm$byClass



save.image(file = "data032624.RData")
