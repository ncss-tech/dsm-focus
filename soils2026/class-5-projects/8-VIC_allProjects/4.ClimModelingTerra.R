# 8-VIC climate and landform modeling 

# 3/13/24  
# Dave White

# load and install packages
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow", "foreign", "foreach")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# bring in covariate data and create a stack

# set working directory to dem covs
setwd("~/data/8-vic/cov30")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# create a raster stack of dem covs
demStack <- rast(rList)

# set working directory to spectral covs
setwd("~/data/8-vic/specCov30/clip")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# create a raster stack of covs
specStack <- rast(rList)

# combine into one stack
rStack <- c(demStack,specStack)


# bring in pedon data

#set working directory to training data.
setwd("~/data/8-vic/data")

pts <- read_sf("pedons.shp")

# check names of attributes in training.pts (should just be class for course project; 'geometry' is inherent in sf object) 
names(pts) 

# remove duplicate points
pts <- unique(pts)

names(pts)
# remove all cols except those to be modeled
pts <- pts[,-c(1:11,15:17)]
names(pts)

# extract covariate values
pts <- terra::extract(rStack, pts, xy=F, bind=TRUE, na.rm=TRUE) 

# convert to data frame
pts.df <- as.data.frame(pts)
names(pts.df)

# check for missing data
pts.df <- pts.df[complete.cases(pts.df),]


# convert modeling cols to a factor
names(pts.df)[1:3]
pts.df$landform <- as.factor(pts.df$landform)
levels(pts.df$landform)
pts.df$mst <- as.factor(pts.df$mst)
levels(pts.df$mst)
pts.df$temp <- as.factor(pts.df$temp)
levels(pts.df$temp)

#---------------------------------------------------------------------------
# Recursive Feature Selection (RFE)

#subsets <- c(1,10,25,50, 75, 100, 150, 200)
subsets <- c(1,5,10, 15:25,50, 150, 200)

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

c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(9)
rfeLand <- rfe(x = pts.df[,-c(1:3)],
              y = pts.df$landform,
              sizes = subsets,
              rfeControl = ctrl.RFE,
              allowParallel = TRUE
)
stopCluster(c1)             
gc()

c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(9)
rfeMST <- rfe(x = pts.df[,-c(1:3)],
               y = pts.df$mst,
               sizes = subsets,
               rfeControl = ctrl.RFE,
               allowParallel = TRUE
)
stopCluster(c1)             
gc()
c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(9)
rfeTemp <- rfe(x = pts.df[,-c(1:3)],
               y = pts.df$temp,
               sizes = subsets,
               rfeControl = ctrl.RFE,
               allowParallel = TRUE
)
stopCluster(c1)             
gc()





# 4.3  Look at the results
rfeLand #25
rfeMST
rfeTemp #20




rfeLand$fit$confusion
rf.RFE$results
plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

# see list of predictors
predictors(rfeLand)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *
rfeLand

# take the accuracy and subtract the accuracySD. look in the results of rf.RFE and find the accuracy that is > or = to this value. this is the number of covariates to use below
predictors(rfeLand) # top number of covariates

# look at the top number of covariates that are equal to greater than the accuracy minus the accuracySD
#predictors(rf.RFE)[c(1:9,24:26)]

# assign this to a variable
lcovs <- predictors(rfeLand)[c(1:25)]
mcovs <- predictors(rfeMST)
tcovs <- predictors(rfeTemp)[c(1:20)]

# subset the covariate stack and the data frame by the selected covariates the variable a is your selected covariates

# subsed the raser stack to the selected covariates
rsmLand <- subset(rStack, lcovs)
rsmMST <- subset(rStack, mcovs)
rsmTemp <- subset(rStack, tcovs)

# subset the data frame points with the number of covariates selected
names(pts.df)
comp.subL <- (pts.df[,c("landform", lcovs)])
comp.subM <- (pts.df[,c("mst", mcovs)])
comp.subT <- (pts.df[,c("temp", tcovs)])


names(comp.subL)
is.factor(comp.subL$landform)
is.factor(comp.subM$mst)
is.factor(comp.subT$temp)


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
                           #summaryFunction = twoClassSummary) #in most cases a better summary for two class problems )

#make.names(RFE.train.all$Class)
# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(48)
rfLF = train(x = comp.subL[,-1],
              y = comp.subL$landform,
#  Class ~ ., data = comp.sub[,-1],
              "rf", 
              trControl = fitControl, 
              ntree = 500, #number of trees default is 500, which seems to work best anyway. 
              tuneLength=10, 
              metric = 'Kappa', 
              na.action=na.pass,
              keep.forest=TRUE, # added this line and the next for partial dependence plots
              importance=TRUE)

rfMST = train(x = comp.subM[,-1],
             y = comp.subM$mst,
             #  Class ~ ., data = comp.sub[,-1],
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE)

rfTEMP = train(x = comp.subT[,-1],
             y = comp.subT$temp,
             #  Class ~ ., data = comp.sub[,-1],
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE)


stopCluster(c1)
gc()


# Inspect rfFit
rfLF$finalModel


#look at overall accuracy, kappa, and SD associated with each mtry value
print(rfLF)
rfLF$results


# Convert caret wrapper into randomforest model object for raster prediction
rfmLF <- rfLF$finalModel
rfmMST <- rfMST$finalModel
rfmTemp <- rfTEMP$finalModel




# make predictions

# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
setwd("~/data/8-vic/data/tiles/")


#predict Landform
r <- rsmLand
rfm <- rfmLF
startTime <- Sys.time()
numTiles <- 10
x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(r))
tl <- makeTiles(r, x, overwrite=T, extend=T)
cl <- parallel::makeCluster(length(tl))
registerDoParallel(cl)
predicted <- foreach(i = 1:length(tl),
                     .packages = c("terra", "randomForest")) %dopar% {
                       pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T))
                       return(pred)
                     }
stopCluster(cl)
predLF <- do.call(terra::merge,lapply(predicted,terra::rast))
endTime <- Sys.time()
# prints recorded time 
print(endTime - startTime)
plot(predLF)


#predict mst
r <- rsmMST
rfm <- rfmMST
startTime <- Sys.time()
numTiles <- 10
x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(r))
tl <- makeTiles(r, x, overwrite=T, extend=T)
cl <- parallel::makeCluster(length(tl))
registerDoParallel(cl)
predicted <- foreach(i = 1:length(tl),
                     .packages = c("terra", "randomForest")) %dopar% {
                       pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T))
                       return(pred)
                     }
stopCluster(cl)
predMST <- do.call(terra::merge,lapply(predicted,terra::rast))
endTime <- Sys.time()
# prints recorded time 
print(endTime - startTime)
plot(predMST)


#predict temp
r <- rsmTemp
rfm <- rfmTemp
startTime <- Sys.time()
numTiles <- 10
x <- rast(ncol=numTiles, nrow=numTiles, extent=ext(r))
tl <- makeTiles(r, x, overwrite=T, extend=T)
cl <- parallel::makeCluster(length(tl))
registerDoParallel(cl)
predicted <- foreach(i = 1:length(tl),
                     .packages = c("terra", "randomForest")) %dopar% {
                       pred <- wrap(terra::predict(rast(tl[i]),rfm, na.rm=T))
                       return(pred)
                     }
stopCluster(cl)
predTemp <- do.call(terra::merge,lapply(predicted,terra::rast))
endTime <- Sys.time()
# prints recorded time 
print(endTime - startTime)
plot(predTemp)

# bring in pedon data again
#set working directory to training data.
setwd("~/data/8-vic/data")

pts <- read_sf("pedons.shp")

names(pts)
# remove all cols except those to be modeled
pts <- pts[,-c(1:17)]
names(pts)

# stack modeled results
preds <- c(predMST, predTemp, predLF)

names(preds)
str(preds)
names(preds) <- c('mst', 'temp', 'lndf')
names(preds)
plot(preds)

#extract values to points
pts.ext <- terra::extract(preds, pts, xy=F, bind=TRUE, na.rm=TRUE)

head(pts.ext)

#create class col
pts.ext$class <- as.factor(paste0(pts.ext$mst,pts.ext$temp,pts.ext$lndf))

head(pts.ext)
levels(pts.ext$class)

setwd("~/data/8-vic/data")
pts.ext.sf <- st_as_sf(pts.ext)
write_sf(pts.ext.sf, "trainPTS.shp", delete_layer = T)

