# 8-VIC modeling 10/18/23



# load and install package"s
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# set working directory to 100m covariates
setwd("/mnt/disks/8-vic-class5/cov100")
getwd()

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack of covs
rStack <- rast(rList)


names(rStack)

# Bring in training data points 
# read in shapefile 
# set working directory to points
setwd("/mnt/disks/8-vic-class5/data")
getwd()

# change the name to match your shapefile

# all data no splitting
all.pts <- st_read("pedons_ext.shp")

# training points 70/30 split
train70 <- st_read("train_7030.shp")
poly70_10 <- st_read("poly7030_10.shp")
poly70_20 <- st_read("poly7030_20.shp")
poly70_30 <- st_read("poly7030_30.shp")


# training points 80/20 split
train80 <- st_read("train_8020.shp")
poly80_10 <- st_read("poly8020_10.shp")
poly80_20 <- st_read("poly8020_20.shp")
poly80_30 <- st_read("poly8020_30.shp")


# extract raster covariate values at each training point for the all.pts dataset
#pts.sv <- terra::extract(rStack, all.pts, xy=TRUE, bind=TRUE, na.rm=TRUE) 

# convert SpatVector to sf 
#all.pts <- sf::st_as_sf(pts.sv) 

names(all.pts)
all.pts <- all.pts[-c(1:14, 176, 177)]
all.pts$Class <- as.factor(all.pts$Class)
names(all.pts)
#
levels(all.pts$Class)




# check names of attributes and remove unwanted cols *keep cov and class cols
names(train70)
train70 <- train70[-c(1)]

names(poly70_10)
poly70_10 <- poly70_10[-c(1,2,4)]
names(poly70_20)
poly70_20 <- poly70_20[-c(1,2,4)]
names(poly70_30)
poly70_30 <- poly70_30[-c(1,2,4)]

names(train80)
train80 <- train80[-c(1)]

names(poly80_10)
poly80_10 <- poly80_10[-c(1,2,4)]
names(poly80_20)
poly80_20 <- poly80_20[-c(1,2,4)]
names(poly80_30)
poly80_30 <- poly80_30[-c(1,2,4)]





# merge to create data sets for modeling
pts.all <- all.pts
pts.70 <- train70
pts.70_10 <- rbind(train70, poly70_10)
pts.70_20 <- rbind(train70, poly70_20)
pts.70_30 <- rbind(train70, poly70_30)
pts.80 <- train80
pts.80_10 <- rbind(train80, poly80_10)
pts.80_20 <- rbind(train80, poly80_20)
pts.80_30 <- rbind(train80, poly80_30)


# convert sf to a dataframe
pts.all <- as.data.frame(pts.all)[,-ncol(pts.all)]
pts.70 <- as.data.frame(pts.70)[,-ncol(pts.70)]
pts.70_10 <- as.data.frame(pts.70_10)[,-ncol(pts.70_10)]
pts.70_20 <- as.data.frame(pts.70_20)[,-ncol(pts.70_20)]
pts.70_30 <- as.data.frame(pts.70_30)[,-ncol(pts.70_30)]
pts.80 <- as.data.frame(pts.80)[,-ncol(pts.80)]
pts.80_10 <- as.data.frame(pts.80_10)[,-ncol(pts.80_10)]
pts.80_20 <- as.data.frame(pts.80_20)[,-ncol(pts.80_20)]
pts.80_30 <- as.data.frame(pts.80_30)[,-ncol(pts.80_30)]



# remove any observations with NAs
comp <- pts.all[complete.cases(pts.all),]
comp.70 <- pts.70[complete.cases(pts.70),]
comp.70_10 <- pts.70_10[complete.cases(pts.70_10),]
comp.70_20 <- pts.70_20[complete.cases(pts.70_20),]
comp.70_30 <- pts.70_30[complete.cases(pts.70_30),]
comp.80 <- pts.80[complete.cases(pts.80),]
comp.80_10 <- pts.80_10[complete.cases(pts.80_10),]
comp.80_20 <- pts.80_20[complete.cases(pts.80_20),]
comp.80_30 <- pts.80_30[complete.cases(pts.80_30),]


# convert Class col to a factor
comp$Class <- as.factor(comp$Class)
comp.70$Class <- as.factor(comp.70$Class)
comp.70_10$Class <- as.factor(comp.70_10$Class)
comp.70_20$Class <- as.factor(comp.70_20$Class)
comp.70_30$Class <- as.factor(comp.70_30$Class)
comp.80$Class <- as.factor(comp.80$Class)
comp.80_10$Class <- as.factor(comp.80_10$Class)
comp.80_20$Class <- as.factor(comp.80_20$Class)
comp.80_30$Class <- as.factor(comp.80_30$Class)



length(levels(comp$Class))
length(levels(comp.70$Class))
length(levels(comp.70_10$Class))
length(levels(comp.70_20$Class))
length(levels(comp.70_30$Class))
length(levels(comp.80$Class))
length(levels(comp.80_10$Class))
length(levels(comp.80_20$Class))
length(levels(comp.80_30$Class))




#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Recursive Feature Selection (RFE)
# this section is covariate reduction section


# This process is setup to run as a parallel
# in the make cluster function.

# Next, change the subsets
# to match the number of covariates that you have.

# check the number of covariates

length(comp) # number of covariates plus the class column
#subsets <- c(1:(length(comp)-1))
subsets <- c(1,10:50, 75, 100, 130, 160)

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length = 51)# length is n_repeats*nresampling +1 10 folds and 5 repeats = 50
for(i in 1:50) seeds[[i]]<- sample.int(n=1000, length(subsets)) #The 10 in for(i in 1:10) is (n_repeats*nresampling). in your case it is 10 because you're using 10-fold CV. Similarly, if you were using repeatedcv with number=10 and repeats = 5 it would be for(i in 1:50).
#for the last model
seeds[[51]]<-sample.int(1000, 1)

# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       seeds = seeds, 
                       verbose = FALSE,
                       allowParallel = TRUE)


## highlight and run everything from c1 to stopCluster(c1) to run RFE

c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(9)
rfe.all <- rfe(x = comp[,-1],
               y = comp$Class,
               sizes = subsets,
               rfeControl = ctrl.RFE,
               allowParallel = TRUE)
rfe70 <- rfe(x = comp.70[,-1],
             y = comp.70$Class,
             sizes = subsets,
             rfeControl = ctrl.RFE,
             allowParallel = TRUE)
rfe70_10 <- rfe(x = comp.70_10[,-1],
                y = comp.70_10$Class,
                sizes = subsets,
                rfeControl = ctrl.RFE,
                allowParallel = TRUE)
rfe70_20 <- rfe(x = comp.70_20[,-1],
                y = comp.70_20$Class,
                sizes = subsets,
                rfeControl = ctrl.RFE,
                allowParallel = TRUE)
rfe70_30 <- rfe(x = comp.70_30[,-1],
                y = comp.70_30$Class,
                sizes = subsets,
                rfeControl = ctrl.RFE,
                allowParallel = TRUE)
rfe80 <- rfe(x = comp.80[,-1],
             y = comp.80$Class,
             sizes = subsets,
             rfeControl = ctrl.RFE,
             allowParallel = TRUE)
rfe80_10 <- rfe(x = comp.80_10[,-1],
                y = comp.80_10$Class,
                sizes = subsets,
                rfeControl = ctrl.RFE,
                allowParallel = TRUE)
rfe80_20 <- rfe(x = comp.80_20[,-1],
                y = comp.80_20$Class,
                sizes = subsets,
                rfeControl = ctrl.RFE,
                allowParallel = TRUE)
rfe80_30 <- rfe(x = comp.80_30[,-1],
                y = comp.80_30$Class,
                sizes = subsets,
                rfeControl = ctrl.RFE,
                allowParallel = TRUE)
stopCluster(c1)             

gc()


#Look at the results
#rf.RFE
#plot(rf.RFE)



# see list of predictors
#predictors(rf.RFE)
# see list of top 10
#predictors(rf.RFE)[1:10]

# assign this to a variable
a <- predictors(rfe.all)
a7 <- predictors(rfe70)
a71 <- predictors(rfe70_10)
a72 <- predictors(rfe70_20)
a73 <- predictors(rfe70_30)
a8 <- predictors(rfe80)
a81 <- predictors(rfe80_10)
a82 <- predictors(rfe80_20)
a83 <- predictors(rfe80_30)

# subset the covariate stack and the data frame by the selected covariates the variable a is your selected covariates
# subset the raster stack to the selected covariates
rsm <- subset(rStack, a)
rsm7 <- subset(rStack, a7)
rsm71 <- subset(rStack, a71)
rsm72 <- subset(rStack, a72)
rsm73 <- subset(rStack, a73)
rsm8 <- subset(rStack, a8)
rsm81 <- subset(rStack, a81)
rsm82 <- subset(rStack, a82)
rsm83 <- subset(rStack, a83)



# subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class", a)])
names(comp.sub)
comp.sub.7 <- (comp.70[,c("Class", a7)])
comp.sub.71 <- (comp.70_10[,c("Class", a71)])
comp.sub.72 <- (comp.70_20[,c("Class", a72)])
comp.sub.73 <- (comp.70_30[,c("Class", a73)])
comp.sub.8 <- (comp.80[,c("Class", a8)])
comp.sub.81 <- (comp.80_10[,c("Class", a81)])
comp.sub.82 <- (comp.80_20[,c("Class", a82)])
comp.sub.83 <- (comp.80_30[,c("Class", a83)])


#subsets <- c(1:(length(comp)-1))
subsets <- c(1,10:50, 75, 100, 130, 160)

# set seeds to get reproducible results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length = 51)# length is n_repeats*nresampling +1 10 folds and 5 repeats = 50
for(i in 1:50) seeds[[i]]<- sample.int(n=1000, length(subsets)) #The 10 in for(i in 1:10) is (n_repeats*nresampling). in your case it is 10 because you're using 10-fold CV. Similarly, if you were using repeatedcv with number=10 and repeats = 5 it would be for(i in 1:50).
#for the last model
seeds[[51]]<-sample.int(1000, 1)


# set up the train control
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10,
                           repeats = 5,
                           p = 0.8, #20% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)


# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(48)
rfAll = train(Class ~ ., data = comp,
              "rf", 
              trControl = fitControl, 
              ntree = 500, #number of trees default is 500, which seems to work best anyway. 
              tuneLength=10, 
              metric = 'Kappa', 
              na.action=na.pass,
              keep.forest=TRUE, # added this line and the next for partial dependence plots
              importance=TRUE,
              savePredictions = "final")
rfAllrfe = train(Class ~ ., data = comp.sub,
                 "rf", 
                 trControl = fitControl, 
                 ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                 tuneLength=10, 
                 metric = 'Kappa', 
                 na.action=na.pass,
                 keep.forest=TRUE, # added this line and the next for partial dependence plots
                 importance=TRUE,
                 savePredictions = "final")
rf7 = train(Class ~ ., data = comp.70,
            "rf", 
            trControl = fitControl, 
            ntree = 500, #number of trees default is 500, which seems to work best anyway. 
            tuneLength=10, 
            metric = 'Kappa', 
            na.action=na.pass,
            keep.forest=TRUE, # added this line and the next for partial dependence plots
            importance=TRUE,
            savePredictions = "final")
rf7rfe = train(Class ~ ., data = comp.sub.7,
               "rf", 
               trControl = fitControl, 
               ntree = 500, #number of trees default is 500, which seems to work best anyway. 
               tuneLength=10, 
               metric = 'Kappa', 
               na.action=na.pass,
               keep.forest=TRUE, # added this line and the next for partial dependence plots
               importance=TRUE,
               savePredictions = "final")
rf71 = train(Class ~ ., data = comp.70_10,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE,
             savePredictions = "final")
rf71rfe = train(Class ~ ., data = comp.sub.71,
                "rf", 
                trControl = fitControl, 
                ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                tuneLength=10, 
                metric = 'Kappa', 
                na.action=na.pass,
                keep.forest=TRUE, # added this line and the next for partial dependence plots
                importance=TRUE,
                savePredictions = "final")
rf72 = train(Class ~ ., data = comp.70_20,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE,
             savePredictions = "final")
rf72rfe = train(Class ~ ., data = comp.sub.72,
                "rf", 
                trControl = fitControl, 
                ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                tuneLength=10, 
                metric = 'Kappa', 
                na.action=na.pass,
                keep.forest=TRUE, # added this line and the next for partial dependence plots
                importance=TRUE,
                savePredictions = "final")
rf73 = train(Class ~ ., data = comp.70_30,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE,
             savePredictions = "final")
rf73rfe = train(Class ~ ., data = comp.sub.73,
                "rf", 
                trControl = fitControl, 
                ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                tuneLength=10, 
                metric = 'Kappa', 
                na.action=na.pass,
                keep.forest=TRUE, # added this line and the next for partial dependence plots
                importance=TRUE,
                savePredictions = "final")
rf8 = train(Class ~ ., data = comp.80,
            "rf", 
            trControl = fitControl, 
            ntree = 500, #number of trees default is 500, which seems to work best anyway. 
            tuneLength=10, 
            metric = 'Kappa', 
            na.action=na.pass,
            keep.forest=TRUE, # added this line and the next for partial dependence plots
            importance=TRUE,
            savePredictions = "final")
rf8rfe = train(Class ~ ., data = comp.sub.8,
               "rf", 
               trControl = fitControl, 
               ntree = 500, #number of trees default is 500, which seems to work best anyway. 
               tuneLength=10, 
               metric = 'Kappa', 
               na.action=na.pass,
               keep.forest=TRUE, # added this line and the next for partial dependence plots
               importance=TRUE,
               savePredictions = "final")
rf81 = train(Class ~ ., data = comp.80_10,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE,
             savePredictions = "final")
rf81rfe = train(Class ~ ., data = comp.sub.81,
                "rf", 
                trControl = fitControl, 
                ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                tuneLength=10, 
                metric = 'Kappa', 
                na.action=na.pass,
                keep.forest=TRUE, # added this line and the next for partial dependence plots
                importance=TRUE,
                savePredictions = "final")
rf82 = train(Class ~ ., data = comp.80_20,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE,
             savePredictions = "final")
rf82rfe = train(Class ~ ., data = comp.sub.82,
                "rf", 
                trControl = fitControl, 
                ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                tuneLength=10, 
                metric = 'Kappa', 
                na.action=na.pass,
                keep.forest=TRUE, # added this line and the next for partial dependence plots
                importance=TRUE,
                savePredictions = "final")
rf83 = train(Class ~ ., data = comp.80_30,
             "rf", 
             trControl = fitControl, 
             ntree = 500, #number of trees default is 500, which seems to work best anyway. 
             tuneLength=10, 
             metric = 'Kappa', 
             na.action=na.pass,
             keep.forest=TRUE, # added this line and the next for partial dependence plots
             importance=TRUE,
             savePredictions = "final")
rf83rfe = train(Class ~ ., data = comp.sub.83,
                "rf", 
                trControl = fitControl, 
                ntree = 500, #number of trees default is 500, which seems to work best anyway. 
                tuneLength=10, 
                metric = 'Kappa', 
                na.action=na.pass,
                keep.forest=TRUE, # added this line and the next for partial dependence plots
                importance=TRUE,
                savePredictions = "final")
stopCluster(c1)

gc()



#save workspace

setwd("/mnt/disks/8-vic-class5/data")
save.image(file = "data.RData")

print(rfAll)
rfAll$results

print(rfAllrfe)
rfAllrfe$results

print(rf7)
rf7$results

print(rf7rfe)
rf7rfe$results

print(rf71)
rf71$results

print(rf71rfe)
rf71rfe$results

print(rf72)
rf72$results

print(rf72rfe)
rf72rfe$results

print(rf73)
rf73$results

print(rf73rfe)
rf73rfe$results

print(rf8)
rf8$results

print(rf8rfe)
rf8rfe$results

print(rf81)
rf81$results

print(rf81rfe)
rf81rfe$results

print(rf82)
rf82$results

print(rf82rfe)
rf82rfe$results

print(rf83)
rf83$results

print(rf83rfe)
rf83rfe$results



# Convert caret wrapper into random forest model object for raster prediction
rfmAll <- rfAll$finalModel
rfmAllrfe <- rfAllrfe$finalModel
rfm7 <- rf7$finalModel
rfm7rfe <- rf7rfe$finalModel
rfm71 <- rf71$finalModel
rfm71rfe <- rf71rfe$finalModel
rfm72 <- rf72$finalModel
rfm72rfe <- rf72rfe$finalModel
rfm73 <- rf73$finalModel
rfm73rfe <- rf73rfe$finalModel
rfm8 <- rf8$finalModel
rfm8rfe <- rf8rfe$finalModel
rfm81 <- rf81$finalModel
rfm81rfe <- rf81rfe$finalModel
rfm82 <- rf82$finalModel
rfm82rfe <- rf82rfe$finalModel
rfm83 <- rf83$finalModel
rfm83rfe <- rf83rfe$finalModel



# class prediction


# class prediction

predAll <- predict(rStack, rfmAll, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
predAllrfe <- predict(rsm, rfmAllrfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred7 <- predict(rStack, rfm7, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred7rfe <- predict(rsm7, rfm7rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred71 <- predict(rStack, rfm71, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred71rfe <- predict(rsm71, rfm71rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred72 <- predict(rStack, rfm72, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred72rfe <- predict(rsm72, rfm72rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred73 <- predict(rStack, rfm73, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred73rfe <- predict(rsm73, rfm73rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred8 <- predict(rStack, rfm8, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred8rfe <- predict(rsm8, rfm8rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred81 <- predict(rStack, rfm81, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred81rfe <- predict(rsm81, rfm81rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred82 <- predict(rStack, rfm82, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred82rfe <- predict(rsm82, rfm82rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred83 <- predict(rStack, rfm83, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()
pred83rfe <- predict(rsm83, rfm83rfe, na.rm=T, cores = parallel::detectCores()/2, cpkgs ="randomForest")
gc()

setwd("C:/8-VIC-class5Projects")
save.image(file = "data.RData")

setwd("C:/8-VIC-class5Projects/results")
writeRaster(predAll, overwrite = TRUE, filename = "predAll.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(predAllrfe, overwrite = TRUE, filename = "predAllrfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred7, overwrite = TRUE, filename = "pred7.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred7rfe, overwrite = TRUE, filename = "pred7rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred71, overwrite = TRUE, filename = "pred71.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred71rfe, overwrite = TRUE, filename = "pred71rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred72, overwrite = TRUE, filename = "pred72.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred72rfe, overwrite = TRUE, filename = "pred72rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred73, overwrite = TRUE, filename = "pred73.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred73rfe, overwrite = TRUE, filename = "pred73rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred8, overwrite = TRUE, filename = "pred8.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred8rfe, overwrite = TRUE, filename = "pred8rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred81, overwrite = TRUE, filename = "pred81.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred81rfe, overwrite = TRUE, filename = "pred81rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred82, overwrite = TRUE, filename = "pred82.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred82rfe, overwrite = TRUE, filename = "pred82rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred83, overwrite = TRUE, filename = "pred83.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')
writeRaster(pred83rfe, overwrite = TRUE, filename = "pred83rfe.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')



# accuracy assessment


#create a stack of predictions
preds7 <- (c(pred7, pred7rfe, pred71, pred71rfe, pred72, pred72rfe, pred73, pred73rfe))
preds8 <- (c(pred8, pred8rfe, pred81, pred81rfe, pred82, pred82rfe, pred83, pred83rfe))

names(preds7)
names(preds7) <- c("pred7", "pred7rfe", "pred71", "pred71rfe", "pred72", "pred72rfe", "pred73", "pred73rfe")
names(preds7)

names(preds8)
names(preds8) <- c("pred8", "pred8rfe", "pred81", "pred81rfe", "pred82", "pred82rfe", "pred83", "pred83rfe")
names(preds8)



#load in validation set

setwd("C:/8-VIC-class5Projects")

valid7pts <- st_read("valid_7030.shp")
valid8pts <- st_read("valid_8020.shp")


# extract values to points
valid7 <- extract(preds7, valid7pts, bind=T)
head(valid7)
names(valid7)

valid7 <- valid7[,-c(1,3:162)]
names(valid7)
head(valid7)

valid8 <- extract(preds8, valid8pts, bind=T)
head(valid8)
names(valid8)

valid8 <- valid8[,-c(1,3:162)]
names(valid8)



head(valid7)
valid7$Class <- as.factor(valid7$Class)
valid7$pred7 <- as.factor(valid7$pred7)
valid7$pred7rfe <- as.factor(valid7$pred7rfe)
valid7$pred71 <- as.factor(valid7$pred71)
valid7$pred71rfe <- as.factor(valid7$pred71rfe)
valid7$pred72 <- as.factor(valid7$pred72)
valid7$pred72rfe <- as.factor(valid7$pred72rfe)
valid7$pred73 <- as.factor(valid7$pred73)
valid7$pred73rfe <- as.factor(valid7$pred73rfe)

head(valid8)
valid8$Class <- as.factor(valid8$Class)
valid8$pred8 <- as.factor(valid8$pred8)
valid8$pred8rfe <- as.factor(valid8$pred8rfe)
valid8$pred81 <- as.factor(valid8$pred81)
valid8$pred81rfe <- as.factor(valid8$pred81rfe)
valid8$pred82 <- as.factor(valid8$pred82)
valid8$pred82rfe <- as.factor(valid8$pred82rfe)
valid8$pred83 <- as.factor(valid8$pred83)
valid8$pred83rfe <- as.factor(valid8$pred83rfe)




levels(valid7$Class)
levels(valid7$pred7rfe)
levels(valid8$Class)
levels(valid8$pred8)

cm7 <- confusionMatrix(valid7$Class, valid7$pred7)
cm7rfe <- confusionMatrix(valid7$Class, valid7$pred7rfe)
cm71 <- confusionMatrix(valid7$Class, valid7$pred71)
cm71rfe <- confusionMatrix(valid7$Class, valid7$pred71rfe)
cm72 <- confusionMatrix(valid7$Class, valid7$pred72)
cm72rfe <- confusionMatrix(valid7$Class, valid7$pred72rfe)
cm73 <- confusionMatrix(valid7$Class, valid7$pred73)
cm73rfe <- confusionMatrix(valid7$Class, valid7$pred73rfe)
cm8 <- confusionMatrix(valid8$Class, valid8$pred8)
cm8rfe <- confusionMatrix(valid8$Class, valid8$pred8rfe)
cm81 <- confusionMatrix(valid8$Class, valid8$pred81)
cm81rfe <- confusionMatrix(valid8$Class, valid8$pred81rfe)
cm82 <- confusionMatrix(valid8$Class, valid8$pred82)
cm82rfe <- confusionMatrix(valid8$Class, valid8$pred82rfe)
cm83 <- confusionMatrix(valid8$Class, valid8$pred83)
cm83rfe <- confusionMatrix(valid8$Class, valid8$pred83rfe)



cm7
cm7rfe
cm71
cm71rfe
cm72 
cm72rfe
cm73
cm73rfe 
cm8
cm8rfe
cm81
cm81rfe 
cm82
cm82rfe
cm83
cm83rfe





# shannon entropy
predProb <- predict(r.stack.model, rf.model, na.rm=T, type="prob", cores = 12, cpkgs="randomForest")
plot(predProb$dunes)
writeRaster(predProb, overwrite = TRUE, filename = "cpProb.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')



# shannon entropy
library(aqp)

shan <- shannonEntropy(predProb)
plot(shan)
writeRaster(shan, overwrite = TRUE, filename = "cpShan.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')

shanNorm <- shannonEntropy(predProb, b=14)
plot(shanNorm)
writeRaster(shanNorm, overwrite = TRUE, filename = "cpShanNorm.tif", gdal=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U')

