# 8-VIC modeling 10/16/23


# 3/13/24
# 8/26/24
# Dave White
start.time <- Sys.time()

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

rm(specStack, demStack, rList)

# Bring in training data points 
# read in shapefile 
# bring in pedon data

#set working directory to training data.
setwd("~/data/8-vic/data")

pts <- read_sf("pedons.shp")

# extract raster covariate values at each training point for the all.pts dataset
pts.sv <- terra::extract(rStack, pts, xy=F, bind=TRUE, na.rm=TRUE) 

# convert SpatVector to sf 
all.pts <- sf::st_as_sf(pts.sv) 

names(all.pts)

# remove unwanted cols
all.pts <- all.pts[-c(1:14)]
colnames(all.pts)[1] <- "class"
names(all.pts)
all.pts$class <- as.factor(all.pts$class)
names(all.pts)
#
levels(all.pts$class)


# convert sf to a dataframe
pts.all <- as.data.frame(all.pts)[-c(172)]
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
subsets <- c(1,10,30,50,70,95,100,110,150,180)

# set seeds to get reproducible results when running the process in parallel
set.seed(238)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 5, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE

set.seed(9)
rfe <- rfe(x = comp[,-c(1)],
           y = comp$class,
           sizes = subsets,
           rfeControl = ctrl.RFE
)

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
a <- predictors(rfe)#[c(1:50)]


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



# set up the train control
fitControl <- trainControl(#method = "repeatedcv", 
  #number = 10,
  #repeats = 5,
  p = 0.8, #30% used for test set, 70% used for training set
  selectionFunction = 'best', 
  classProbs = T,
  savePredictions = T, 
  returnResamp = 'final',
  search = "random")

# Random Forest - Parallel process

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

gc()


# Inspect rfFit
rfm$finalModel


#look at overall accuracy, kappa, and SD associated with each mtry value
print(rfm)
rfm$results


# Convert caret wrapper into randomforest model object for raster prediction
rfm <- rfm$finalModel







# make predictions
# make predictions

# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
setwd("~/data/8-vic/results/917")

# predict and writout class raster
terra::predict(rsm, rfm, na.rm=T, filename = "class.tif", overwrite=T, wopt=list(gdal=c("COMPRESS=DEFLATE", "TFW=YES", datatype='INT1U')))
write.dbf(levels(pred)[[1]], file='class.tif.vat.dbf') # make sure the first part of the file name is exactly the same as the predicted raster

# predict and writeout probability stack
terra::predict(rsm, rfm, na.rm=T, filename = "classProb.tif", type="prob", overwrite=T)

gc()



# get confusion matrix from model
cm <- confusionMatrix(rfm$predicted, rfm$y)
cm$byClass

end.time <- Sys.time()

time.taken <- end.time - start.time

time.taken
saveRDS(time.taken, "timeTaken.rds")

