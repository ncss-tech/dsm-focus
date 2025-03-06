# wy630-carbon-wy737-sweetwater-counties modeling
# 6/2024
# Jessica Philippe

#load and install packages
# load and install package"s
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

#bring in covariate stack
setwd("~/wy/cov30")
rStack <- rast("rStack.tif")

names(rStack) #check to make sure all makes sense

#bring in training points
setwd("~/wy/data")
pts <- read_sf("trainpts_117.shp")

names(pts)

#extract raster covariate values to training points
pts.cov <- terra::extract(rStack, pts, xy=F, bind=TRUE,na.rm=TRUE)

#convert to sf
pts.cov <- sf::st_as_sf(pts.cov)

#remove unwanted columns
names(pts.cov)
pts.cov <- pts.cov[-c(1:5,360)]
names(pts.cov)
pts.cov$class <- as.factor(pts.cov$class)
levels(pts.cov$class)

#convert to dataframe
pts.cov <- as.data.frame(pts.cov)[-c(355)] #remove geometry
names(pts.cov)

#remove NAs
comp <- pts.cov[complete.cases(pts.cov),]

is.factor(comp$class) #check that class is a factor
levels(comp$class)
summary((comp$class))

#remove classes <2 observations

nlevels(comp$class)
comp = comp[comp$class %in% names(table(comp$class)) [table(comp$class) >2],]
levels(comp$class)
comp <- droplevels(comp)
levels(comp$class)
summary(comp$class)

length(levels(comp$class))
gc()

# Recursive Feature Selection
###--------------Recursive Feature Selection---------------------------###
# Recursive feature selection for covariate reduction
# Run in parallel in the make cluster function

#set up
terraOptions()
mem_info(rStack)
free_RAM()
terraOptions(memfrac=0.9, memmax = 7e+08)

#change the subsets to match the number of covariates set numbers to cover the range
length(comp) #is number of covariates plus the class column
#subsets <- c(1:(length(comp.mst)-1))
subsets <-c(1,5:15,20,25,50, 75, 100, 125, 150,175,200,225,250,275,300,325,353)

#Set seed to get reproducible results when running in parallel
set.seed(615)
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

## highlight and run everything from c1 to stopCluster(c1) to run RFE in parallel

c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(615)
rfe <- rfe(x = comp[,-1],
              y = comp$class,
              sizes = subsets,
              rfeControl = ctrl.RFE,
              allowParallel = TRUE
)
stopCluster(c1)             

gc()

#look at results
rfe
plot(rfe)

rfe$fit$confusion
rfe$results
plot(rfe, metric="Kappa", main='RFE Kappa')

#list of predictors
predictors(rfe)

#look at highest accuracy
rfe #highest is 20 but 15 is super close 2nd

#assign top covariates from RFE to a varial

p <- predictors(rfe)[c(1:15)]

#subset the raster stack to only those predictors

rStackP <- subset(rStack, p)

names(rStackP)

#subset the data frame points to only include those predictors
names(comp)
comp.sub <- (comp[,c("class",p)])
names(comp.sub)


##### Random Forest #####
#set up
terraOptions()
mem_info(rStackP)
free_RAM()
terraOptions(memfrac=0.9, memmax = 7e+08)

subsets <- 15 
set.seed(615)
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

# run RF in parallel - highlight c1 to c1 to run
cl <- makeCluster(detectCores()-12)
registerDoParallel(cl)
set.seed(615)
rf1 = train(x = comp.sub[,-c(1)],
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

#look at model
rf1$finalModel
print(rf1)
rf1$results
str(rf1)
rf1$resample

#convert to randomforest model object
rf <-rf1$finalModel
#look at variable importance plot
varImpPlot(rf)

#predict
#make predictions
#set directory for prediction tiles
setwd("~/wy/tiles")
detectCores()
cores <- 123
numTiles <- round(sqrt(cores),digits=0)

x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(rStackP))
t1 <- makeTiles(rStackP, x, overwrite=T, extend=T)

cl <- parallel::makeCluster(length(t1))
registerDoParallel(cl)

pred <- foreach(i = 1:length(t1),
                .packages = c("terra", "randomForest")) %dopar% {
                  pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T))
                  return(pred)
                }
pred <- do.call(terra::merge,lapply(pred,terra::rast)) 
plot(pred)

setwd("~/wy/results")

#write raster
writeRaster(pred, overwrite = TRUE, filename = "WYclass1172024.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype="INT1U")
#write raster attribute table
library(foreign)
write.dbf(levels(pred)[[1]],file='WYclass1172024.tif.vat.dbf') #exactly match the tif file name, add .vat.dbf, put in same place as raster

#Probability stacks
setwd("~/wy/results/probs1172024")
length(t1)
predProb <- foreach(i = 1:20, #1:lenght(t1)
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob1.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 21:40,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob2.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 41:60,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob3.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 61:80,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob4.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 81:100,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob5.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
predProb <- foreach(i = 101:121,
                    .packages = c("terra", "randomForest")) %dopar% {
                      pred <- wrap(terra::predict(rast(t1[i]),rf, na.rm=T, type="prob"))
                      return(pred)
                    }
predProb <- do.call(terra::merge,lapply(predProb,terra::rast))
writeRaster(predProb, overwrite = TRUE, filename = "prob6.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

stopCluster(c1)

rm(predProb)

gc()
#####START 1182024#####
#Make smaller tiles for shannon entropy (otherwise run into memory issues)
setwd("~/wy/results/probs1172024")
rList=list.files(getwd(),pattern="tif$", full.names=FALSE)
rList

#build VRT file
v <- vrt(rList, "probv.vrt", overwrite=TRUE)

names(v) 

#check to be sure each vrt layer is whole area
oneband <- rast(v[[1]])

plot(oneband)

#make tiles for parallel shannon entropy
setwd("~/wy/results/probs1172024/ent_tiles")

cores <- 123
numTiles <- round(sqrt(cores),digits=0)

x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(v))
entiles <- makeTiles(v, x, overwrite=T, extend=T)


setwd("~/wy/results/probs1172024/ent_tiles")
entiles=list.files(getwd(),pattern="tif$", full.names=FALSE)
vrtfile <- paste0(tempfile(), ".vrt")
v2 <- vrt(entiles, vrtfile)
b <- length(names(v2))

#run entropy in parallel

c1 <- parallel::makeCluster(length(20))
registerDoParallel(c1)

#shannon entropy
shan <- foreach(i = 1:length(entiles),
                .packages = c("terra", "aqp")) %dopar% {
                  shan <- wrap(aqp::shannonEntropy(terra::rast(entiles[i])))
                  return(shan)
                }
shan <- do.call(terra::merge,lapply(shan, terra::rast))
plot(shan)

gc()

#normalized shan entropy
shanNorm <- foreach(i = 1:length(entiles),
                .packages = c("terra", "aqp")) %dopar% {
                  shan <- wrap(aqp::shannonEntropy(terra::rast(entiles[i]), b=b))
                  return(shan)
                }
shanNorm <- do.call(terra::merge,lapply(shanNorm, terra::rast))
plot(shanNorm)

gc()

setwd("~/wy/results")
writeRaster(shan, overwrite = TRUE, filename = "WYshan1172024.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
writeRaster(shanNorm, overwrite = TRUE, filename = "WYshanNorm1172024.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

stopCluster(c1)

#get confusion matrix from model
cm <- confusionMatrix(rf$predicted, rf$y)
cm$byClass
