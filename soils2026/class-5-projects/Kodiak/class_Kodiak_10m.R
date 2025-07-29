# Updated Modeling Script for Kodiak Island
# Dave White

#8/28/24

# load and install packages
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow", "foreign", "foreach")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=F)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)



# bring in covariate data and create a stack

# set working directory to dem covs
rStack <- rast("~/data/tiles/cov.vrt")



# Bring in training data points 
# read in shapefile 
# bring in pedon data

#set working directory to training data.
setwd("~/data")

pts <- read_sf("trainData72925.shp")

names(pts)

pts <- pts[5]

names(pts)
levels(as.factor(pts$class))
pts$class <- make.names(pts$class)
levels(as.factor(pts$class))

#names(pts)[1] <- 'class'
#names(pts)

# extract raster covariate values at each training point for the all.pts dataset
pts.sv <- terra::extract(rStack, pts, xy=F, bind=TRUE, na.rm=TRUE) 

# convert SpatVector to sf 
all.pts <- sf::st_as_sf(pts.sv) 

names(all.pts)

# remove unwanted cols
#all.pts <- all.pts[-c(1:6)]
#colnames(all.pts)[1] <- "class"
#names(all.pts)
all.pts$class <- as.factor(all.pts$class)
names(all.pts)
#
levels(all.pts$class)


# convert sf to a dataframe
pts.all <- as.data.frame(all.pts)[-c(208)]
names(pts.all)

comp <- pts.all
names(comp)
# convert sf to a dataframe
#comp <- (all.pts)

head(comp)

# remove any observations with NAs
comp <- comp[complete.cases(comp),]


# remove covariates with NAs
comp <- comp[,colSums(is.na(comp))==0] # removes columns that contain NA values

names(comp)

nm <- names(comp[-1])
rStack <- subset(rStack, nm)
names(rStack)

# convert Class col to a factor
comp$class <- as.factor(comp$class)
is.factor(comp$class)


# There are a few classes with < 9 observations remove those classes

length(levels(comp$class))

summary(comp$class)


#write.csv((as.data.frame(summary(comp$class))), file = "D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/results/scClassBalance.csv")

nlevels(comp$class)#

comp = droplevels(comp[comp$class %in% names(table(comp$class)) [table(comp$class) >2],])

nlevels(comp$class)
summary(comp$class)
#write.csv((as.data.frame(summary(comp$class))), file = "D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/results/scModeledClassBalance.csv")

levels(comp$class)

gc()
#---------------------------------------------------------------------------
#Boruta to reduce covs
# run the Boruta algorithm
#fs_bor <- Boruta(y = comp$class, x = comp[, -1], maxRuns = 15, doTrace = 2)#

# plot variable importance and selected features
#plot(fs_bor)

# plot evolution of the feature selection
#plotImpHistory(fs_bor)

# extract the selected feature variables
#vars <- getSelectedAttributes(fs_bor)
#vars

# view summary of the results
#View(attStats(fs_bor))

# subset raster stack
#rStack <- subset(rStack, vars)

names(rStack)


# Recursive Feature Selection (RFE)


subsets <- seq(0, length(names(rStack)), 20)
#subsets <- seq(35, 45, 1)
#subsets <- seq(20, 40, 1)

#set number and repeats for cross validation
number = 10
repeats = 1

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=(number*repeats+1))
for(i in 1:(number*repeats+1)) seeds[[i]] <- sample.int(1000, number*repeats+2)
seeds[[(number*repeats+1)]] <- sample.int(1000, 1)


cl <- makeCluster(120) # base R only recognizes 128 cores and about 5 need to be left for OS
registerDoParallel(cl)

# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = number,
                       repeats = repeats,
                       seeds = seeds,
                       verbose = F)

## highlight and run everything from c1 to stopCluster(c1) to run RFE
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


# see list of predictors
predictors(rfe)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *


# take the accuracy and subtract the accuracySD. look in the results of rf.RFE and find the accuracy that is > or = to this value. this is the number of covariates to use below
#predictors(rfeLand) # top number of covariates

# look at the top number of covariates that are equal to greater than the accuracy minus the accuracySD
predictors(rfe)#[c(1:60)]

# assign this to a variable
a <- predictors(rfe)#[c(1:60)]


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

subsets <- seq(length(names(rsm))-5, length(names(rsm))+5, 1)

#set number and repeats for cross validation
number = 10
repeats = 5

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=(number*repeats+1))
for(i in 1:(number*repeats)) seeds[[i]] <- sample.int(number*repeats+1)
seeds[[(number*repeats+1)]] <- sample.int(1000, 1)


# set up the train control
fitControl <- trainControl(method = "repeatedcv", 
                           number = number,
                           repeats = repeats,
                           p = 0.8, #30% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)
# Random Forest - Parallel process
cl <- makeCluster(120)
registerDoParallel(cl)
set.seed(48)
rfm = train(x = comp.sub[,-c(1)],
            y = comp.sub$class,
            "rf", 
            trControl = fitControl, 
            ntree = 500, #number of trees default is 500, which seems to work best anyway. 
            tuneLength=10, 
            #metric = 'Kappa', 
            na.action=na.pass,
            keep.forest=TRUE, # added this line and the next for partial dependence plots
            nodesize =  8,
            importance=TRUE)





stopCluster(cl)
gc()


# Inspect rfFit
rfm$finalModel



#look at overall accuracy, kappa, and SD associated with each mtry value
print(rfm)
rfm$results

rfmCaret <- rfm
# Convert caret wrapper into randomforest model object for raster prediction
rfm <- rfm$finalModel

rfm

varImp(rfm)

plot(rfm)


# getrfm# get confusion matrix from model
cm <- confusionMatrix(rfm$predicted, rfm$y)
# confusion matrix as a table 
as.table(cm)
write.csv((as.table(cm)), "~/data/results/ConMat.csv")
# get overall accuracy metrics
as.matrix(cm, what="overall")
write.csv((as.matrix(cm, what="overall")), "~/data/results/Overall.csv")
# get class wise accuracy metrics
as.matrix(cm, what ="classes")
write.csv((as.matrix(cm, what ="classes")), "~/data/results/ClassAccuracy.csv")


# make predictions

# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
setwd("~/data/tiles")
tl <- list.files(getwd(), pattern=".tif$", full.names=T)
tlnames <- list.files("~/data/tiles", pattern=".tif$", full.names = F)
tlnames <- gsub(".tif", "", tlnames)

# read in raster file names as a list
#rList=list.files(getwd(), pattern="tif$", full.names = FALSE)


cl <- parallel::makeCluster(40)
registerDoParallel(cl)

foreach(i = 1:length(tl), .packages = c("terra", "randomForest")) %dopar% {
  wrap(terra::predict(subset(rast(tl[i]), a), rfm, na.rm=T,# type="prob", 
                      filename = paste0("~/data/results/tiles/",
                                        tlnames[i],".tif"), 
                      gdal=c("TFW=YES"),
                      overwrite=T,
                      steps=4))
}

stopCluster(cl)
gc()

setwd("~/data/results/tiles/")
tl <- list.files(getwd(), pattern=".tif$", full.names=T)
# make a virtual raster dataset from the tiles
vrtfile <- paste0(tempfile(), ".vrt")
v <- vrt(tl, vrtfile, filename="~/data/results/class.vrt", overwrite=T, set_names = T)

#pred <- foreach(i = 1:length(tl), .packages = c("terra", "randomForest")) %dopar% {
#  pred <- wrap(terra::predict(subset(rast(tl[i]), a),rfm, na.rm=T,steps=4))#wopt=list(steps=40)
#  pred <- do.call(terra::merge,lapply(pred,terra::rast))
#  return(pred)
#}

#
pred <- rast("~/data/results/class.vrt")
plot(pred)

setwd("~/data/results")
# write rasters
writeRaster(pred, overwrite = TRUE, filename = "class.tif", gdal=c("TFW=YES"),datatype='INT1U')
# write raster attribute table
#library(foreign)
levels(pred)[[1]]
write.dbf(levels(pred)[[1]], file='class.tif.vat.dbf') # make sure the first part of the file name is exactly the same as the predicted raster
stopCluster(cl)

gc()




save.image(file = "~/data/results/data72925.RData")











#################################################################################################

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
