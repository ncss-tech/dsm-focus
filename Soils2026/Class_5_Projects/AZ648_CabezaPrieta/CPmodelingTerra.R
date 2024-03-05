# Cabeza Prieta Modeling 7/31/23




# load and install package"s
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "foreign")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# set working directory to spectral covariates
setwd("/mnt/disks/az648-cabeza-prieta-area/covsClip")
getwd()

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack of spectral covs
spectral <- rast(rList)

# set working directory for terrain covariates
setwd("/mnt/disks/az648-cabeza-prieta-area/covt")
getwd()

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack of spectral covs
terrain <- rast(rList)

# combine into one raster stack
#rStack <- spectral
rStack <- c(spectral, terrain)
names(rStack)

# Bring in training data points 
# read in shapefile 
# set working directory to points
setwd("/mnt/disks/az648-cabeza-prieta-area/data")
getwd()

# change the name to match your shapefile name 
pts <- st_read("landformPts_final.shp") 

# check names of attributes in training.pts (should just be class for course project; 'geometry' is inherent in sf object) 
names(pts) 

# remove cols that are unwanted
pts <- pts[-c(1:6)]
names(pts)

# extract raster covariate values at each training point from raster stack created on line 28 above and create a SpatVector 
pts.extract <- terra::extract(rStack, pts, xy=TRUE, bind=TRUE, na.rm=TRUE) 

# convert spatVector to a dataframe
names(pts.extract)

pts.df <- as.data.frame(pts.extract)
names(pts.df)

# remove xy cols
pts.df <- pts.df[-c(115,116)]
names(pts.df)
names(pts.df)[1]  <- "Class"
names(pts.df)



comp <- pts.df[complete.cases(pts.df),]
comp$Class <- as.factor(comp$Class)
levels(comp$Class)

# Recursive Feature Selection (RFE)
# this section is covariate reduction section


# This process is setup to run as a parallel
# in the make cluster function.

# Next, change the subsets
# to match the number of covariates that you have.

# check the number of covariates

length(pts.df) # number of covariates plus the class column
#subsets <- c(1:(length(comp)-1))
#subsets <- c(1,10:25, 50, 100, 120)
subsets <- c(1,20:50, 100, 120)

# set seeds to get reporducable results when running the process in parallel
set.seed(238)
seeds <- vector(mode = "list", length=112)
for(i in 1:111) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[112]] <- sample.int(1000, 1)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       seeds = seeds, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE

c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(9)
rf.RFE <- rfe(x = comp[,-1],
              y = comp$Class,
              sizes = subsets,
              rfeControl = ctrl.RFE,
              allowParallel = TRUE
)
stopCluster(c1)             

gc()

# 4.3  Look at the results
rf.RFE

rf.RFE$fit

summary(comp$Class)

rf.RFE$fit$confusion


rf.RFE$results

#plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

# see list of predictors
predictors(rf.RFE)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *

rf.RFE

# take the accuracy and subtract the accuracySD. look in the results of rf.RFE and find the accuracy that is > or = to this value. this is the number of covariates to use below

predictors(rf.RFE) # top number of covariates

# look at the top number of covariates that are equal to greater than the accuracy minus the accuracySD
predictors(rf.RFE)[1:33]

# assign this to a variable
a <- predictors(rf.RFE)[1:33]

# subset the covariate stack and the data frame by the selected covariates the variable a is your selected covariates

# subsed the raser stack to the selected covariates
r.stack.model <- subset(rStack, a)
# subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class", a)])
names(comp.sub)
levels(comp.sub$Class)
names(comp.sub)



######
#subsets max number should be the same or equal to the number of covariates being used
levels(comp.sub$Class)
names(comp.sub)
# determine number of covariates
length(a)

subsets <- length(a)
#subsets <- c(1,10, 25, 50, 100, 150, 193)
# set seeds to get reproducable results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=76)
for(i in 1:75) seeds[[i]] <- sample.int(1000, length(1:subsets) + 1)
seeds[[76]] <- sample.int(1000, 1)


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

#make.names(RFE.train.all$Class)
# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(48)
rfFit = train(Class ~ ., data = comp.sub,
              "rf", 
              trControl = fitControl, 
              ntree = 500, #number of trees default is 500, which seems to work best anyway. 
              tuneLength=10, 
              metric = 'Kappa', 
              na.action=na.pass,
              keep.forest=TRUE, # added this line and the next for partial dependance plots
              importance=TRUE)
stopCluster(c1)

gc()


# Inspect rfFit
rfFit$finalModel

#look at overall accuracy, kappa, and SD associated with each mtry value
print(rfFit)
rfFit$results

#look at accuracy associated with each CV fold
str(rfFit)
rfFit$resample


# Convert caret wrapper into randomforest model object for raster prediction
rf.model <- rfFit$finalModel


varImpPlot(rf.model)


# make predictions
setwd("/mnt/disks/az648-cabeza-prieta-area/results")

# class predicition
pred <- terra::predict(r.stack.model, rf.model, na.rm=T, cores = 2, cpkgs="randomForest")

# class predicition
#pred2 <- terra::predict(r.stack.model, rf.model, na.rm=T, cores = 8, cpkgs="randomForest")


plot(pred)







# write class predicion
writeRaster(pred, overwrite = TRUE, filename = "cpClass.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')

# write raster attribute table
#library(foreign)
write.dbf(levels(pred)[[1]], file='cpClass.tif.vat.dbf') # make sure the first part of the file name is exactly the same as the predicted raster


# probability stack prediciton
predProb <- predict(r.stack.model, rf.model, na.rm=T, type="prob", cores = 4, cpkgs="randomForest")
plot(predProb$dunes)

# write probability stack... large file
writeRaster(predProb, overwrite = TRUE, filename = "cpProb.tif")

# shannon entropy
#library(aqp)

# calculate shannon entropy
shan <- shannonEntropy(predProb)
plot(shan)
writeRaster(shan, overwrite = TRUE, filename = "cpShan.tif")

# calculate normalized shannon entropy
shanNorm <- shannonEntropy(predProb, b=14) # b parameter is the number of classes
plot(shanNorm)
writeRaster(shanNorm, overwrite = TRUE, filename = "cpShanNorm.tif")

