# 8-VIC modeling


# 10/31/24

# Dave White
start.time <- Sys.time()

# load and install packages
required.packages <- c( "caret", "sf", "terra", "randomForest", "aqp", "foreign","doParallel", "parallel")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)



# bring in covariate data and create a stack

# set working directory to dem covs
setwd("~/data/8-vic/cov")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)

# create a raster stack of dem covs
rStack <- rast(rList)
names(rStack) <- gsub(".tif", "",rList)
names(rStack) <- gsub("-", "", names(rStack))
names(rStack) <- gsub("_", "", names(rStack))
names(rStack)


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
pts.all <- as.data.frame(all.pts)
names(pts.all)

comp <- pts.all[,!(colnames(pts.all)%in% c("geometry"))]
names(comp)



# remove any observations with NAs
comp <- comp[complete.cases(comp),]

# convert Class col to a factor
#comp$Class <- as.factor(comp$Class)
is.factor(comp$class)
levels(comp$class)
summary((comp$class))

# There are a few classes with < 2 observations remove those classes

nlevels(comp$class)

comp = droplevels(comp[comp$class %in% names(table(comp$class)) [table(comp$class) >2],])

nlevels(comp$class)

levels(comp$class)
summary(comp$class)


length(levels(comp$class))
gc()

#---------------------------------------------------------------------------
# Recursive Feature Selection (RFE)


subsets <- seq(50, length(names(rStack)), 5)

#set number and repeats for cross validation
number = 10
repeats = 1

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=(number*repeats+1))
for(i in 1:(number*repeats)) seeds[[i]] <- sample.int(1000, length(subsets)+1)
seeds[[(number*repeats+1)]] <- sample.int(1000, 1)


cl <- makeCluster(detectCores()-1) # base R only recognizes 128 cores and about 5 need to be left for OS
registerDoParallel(cl)

# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = number,
                       repeats = repeats,
                       seeds = seeds, 
                       verbose = F)

## highlight and run everything from c1 to stopCluster(c1) to run RFE
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



subsets <- seq(1, length(names(rsm)), 5)

#set number and repeats for cross validation
number = 10
repeats = 5

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=(number*repeats+1))
for(i in 1:(number*repeats)) seeds[[i]] <- sample.int(1000, length(subsets)+1)
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

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
# Random Forest - Parallel process
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
terra::predict(rsm, rfm, wopt=list(steps=100), na.rm=T, overwrite = TRUE, filename = "~/data/8-vic/results/class.tif", gdal=c("TFW=YES"),datatype='INT1U')

r <- rast("~/data/8-vic/results/class.tif")
rat <- levels(r)[[1]]
write.dbf(rat, file='class.tif.vat.dbf') # make sure the first part of the file name is exactly the same as the predicted raster

gc()


# predict probability stacks
terra::predict(rsm, rfm, wopt=list(steps=100), na.rm=T, type="prob", filename="~/data/8-vic/results/predProb.tif", overwrite=T)


gc()



# read in prob raster
predProb <- rast("~/data/8-vic/results/predProb.tif")

## shannon entropy keeps failing, maybe write prob rasters to disk then bring in as vrt and calc shan

writeRaster(shannonEntropy(predProb), filename="~/data/8-vic/results/shan.tif", overwrite=T,gdal=c("TFW=YES", wopt=list(steps=100)))



gc()

#normalized shan entropy
b <- length(names(predProb))

writeRaster((shannonEntropy(predProb, b=b)), filename="~/data/8-vic/results/shanNorm.tif", overwrite=T,gdal=c("TFW=YES", wopt=list(steps=100)))



# get confusion matrix from model
cm <- confusionMatrix(rfm$predicted, rfm$y)


end.time <- Sys.time()

time.taken <- end.time - start.time

time.taken
saveRDS(time.taken, "timeTaken.rds")

## Transfer the script to the storage bucket:
## Copy the line below and run in the terminal
## change the name of file and path as necessary
##gsutil -m cp ~/data/8-vic/scripts/modeling_notParallel.R gs://sbs-aws-migration-s2026-8-vic/8-vic/scripts/
