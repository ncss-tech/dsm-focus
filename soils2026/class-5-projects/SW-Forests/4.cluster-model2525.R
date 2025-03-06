#
#   SW Forests Soil cluster modeling
#
# 2/5/25

# Dave White


# load and install packages
required.packages <- c("Boruta","caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow", "foreign", "foreach")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)



# bring in covariate data and create a stack
rStack <- rast("~/data/sw-forests/covs/fullStack/covs.vrt")

names(rStack)

#set working directory to training data.
setwd("~/data/sw-forests/data/nasis-export/")

obs <- read_sf("cluster2525.shp")
names(obs)
names(obs)[83]

pts <- obs[83]
names(pts)[1]

names(pts)[1] <- "class"

names(pts)

# extract raster covariate values at each training point for the all.pts dataset
pts.sv <- terra::extract(rStack, pts, xy=F, bind=TRUE, na.rm=TRUE) 


# remove points with duplicate coordinates
pts.sv <- terra::unique(pts.sv)


# convert to data frame and save object
all.pts <- st_drop_geometry(st_as_sf(pts.sv))
#saveRDS(all.pts, "allpts.rds")


names(all.pts)
names(rStack)

names(all.pts[,-1]) == names(rStack)




# inspect the soil climate classes
levels(as.factor(all.pts$class))

# make names since each cluster is a number
all.pts$class <- make.names(all.pts$class)

head(all.pts$class)

# convert cols for modeling into a factor
all.pts$class <- as.factor(all.pts$class)
names(all.pts)

#
levels(all.pts$class)
names(all.pts)

# convert sf to a dataframe
comp <- (all.pts)

head(comp)

# remove any observations with NAs
#comp <- comp[complete.cases(comp).]
comp <- comp[,colSums(is.na(comp))==0] # removes columns that contain NA values

names(comp)

nm <- names(comp[-1])


rStack <- subset(rStack, nm)

# convert Class col to a factor
#comp$Class <- as.factor(comp$Class)
is.factor(comp$class)


# There are a few classes with < 9 observations remove those classes

length(levels(comp$class))

summary(comp$class)


#comp <- subset(comp, !(class %in% c("X14","X15")))


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
repeats = 5#1

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=(number*repeats+1))
for(i in 1:(number*repeats)) seeds[[i]] <- sample.int(1000, number*repeats+1)
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


# Convert caret wrapper into randomforest model object for raster prediction
rfm <- rfm$finalModel



varImp(rfm)


# get confusion matrix from model
cm <- confusionMatrix(rfm$predicted, rfm$y)
# confusion matrix as a table 
as.table(cm)
write.csv((as.table(cm)), "~/data/sw-forests/results/ConMat.csv")
# get overall accuracy metrics
as.matrix(cm, what="overall")
write.csv((as.matrix(cm, what="overall")), "~/data/sw-forests/results/Overall.csv")
# get class wise accuracy metrics
as.matrix(cm, what ="classes")
write.csv((as.matrix(cm, what ="classes")), "~/data/sw-forests/results/ClassAccuracy.csv")


# make predictions

# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
# set wd to store tiles for prediction - tiles are written to hard drive, make sure there is enough room
setwd("~/data/sw-forests/covs/fullStack/tiles")
tl <- list.files(getwd(), pattern=".tif$", full.names=T)


# read in raster file names as a list
#rList=list.files(getwd(), pattern="tif$", full.names = FALSE)


cl <- parallel::makeCluster(120)
registerDoParallel(cl)

pred <- foreach(i = 1:length(tl), .packages = c("terra", "randomForest")) %dopar% {
  pred <- wrap(terra::predict(subset(rast(tl[i]), a),rfm, na.rm=T,steps=4))#wopt=list(steps=40)
  return(pred)
}
pred <- do.call(terra::merge,lapply(pred,terra::rast))
plot(pred)


setwd("~/data/sw-forests/results")
# write rasters
writeRaster(pred, overwrite = TRUE, filename = "clust2525.tif", gdal=c("TFW=YES"),datatype='INT1U')
# write raster attribute table
#library(foreign)
levels(pred)[[1]]
write.dbf(levels(pred)[[1]], file='clust2525.tif.vat.dbf') # make sure the first part of the file name is exactly the same as the predicted raster
stopCluster(cl)









#stop


save.image(file='~/data/sw-forests/results/clustdata.RData')
gc()


names(rsm)

tl <- list.files("~/data/sw-forests/covs/fullStack/tiles", pattern=".tif$", full.names = T)

tlnames <- list.files("~/data/sw-forests/covs/fullStack/tiles", pattern=".tif$", full.names = F)
tlnames <- gsub(".tif", "", tlnames)

cl <- parallel::makeCluster(120)
registerDoParallel(cl)

foreach(i = 1:length(tl), .packages = c("terra", "randomForest")) %dopar% {
  wrap(terra::predict(subset(rast(tl[i]), a), rfm, na.rm=T, type="prob", 
                      filename = paste0("~/data/sw-forests/results/prob/",
                      tlnames[i],".tif"), 
                      gdal=c("TFW=YES"),
                      overwrite=T,
                      steps=4))
}

stopCluster(cl)
gc()




#set directory to prob tiles
setwd("~/data/sw-forests/results/prob")

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)



cl <- parallel::makeCluster(120)
registerDoParallel(cl)
shan <- foreach(i = 1:length(rList),
                .packages = c("terra", "aqp")) %dopar% {
                  shan <- wrap(aqp::shannonEntropy(terra::rast(rList[i])))
                  return(shan)
                }
shan <- do.call(terra::merge,lapply(shan, terra::rast))
plot(shan)



#normalized shan entropy
b <- levels(pred)[[1]]
b <- nrow(b)
shanNorm <- foreach(i = 1:length(rList),
                    .packages = c("terra", "aqp")) %dopar% {
                      shanNorm1 <- wrap(aqp::shannonEntropy(terra::rast(rList[i]), b=b))
                      return(shanNorm1)
                    }
shanNorm <- do.call(terra::merge,lapply(shanNorm, terra::rast))
plot(shanNorm)

setwd("~/data/sw-forests/results")
writeRaster(shan, overwrite = TRUE, filename = "clustShan.tif", gdal=c("TFW=YES"))
writeRaster(shanNorm, overwrite = TRUE, filename = "clustShanNorm.tif", gdal=c("TFW=YES"))
stopCluster(cl)

gc()

save.image(file='~/data/sw-forests/results/clustdata.RData')


## Transfer the script to the storage bucket:
## Copy the line below and run in the terminal
## change the name of file and path as necessary
##gsutil -m cp ~/data/8-vic/scripts/modeling.R gs://sbs-aws-migration-s2026-8-vic/8-vic/scripts/