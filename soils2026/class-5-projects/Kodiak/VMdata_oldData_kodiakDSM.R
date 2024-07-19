## Kodiak Island DSM Modeling 

# Load and install packages
required.packages <- c("caret", "UBL", "corrplot", "rgdal", "raster", "doParallel", "psych", "maptools", "dplyr", "sp", "snow", "snowfall", "plyr", "randomForest", "fmsb", "sf")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# Load Raster Data
# set working directory
setwd("c:/cov/")
getwd()

# read in raster layer names
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack
rStack <- stack(rList)

## load in shapefile with points, if points are in a different folder change the dsn="." to the directory that it is in
shp.pts <- readOGR(dsn=".", layer="MLRA220Cor")

# the points and the raster stack need to be in the same projection
rStack@crs # projection information for rster stack

shp.pts@proj4string # proj info for shapefile

all.equal(rStack@crs, shp.pts@proj4string) #True - same projection

# if you need to convert the projection of the shape file, it would be done as follows:
# 
shp.pts <- spTransform(shp.pts, CRSobj =  rStack@crs)


# r brings in a points shp file in as a SpatialPointsDataFrame (SPDF) object
# check the names of your SPDF
names(shp.pts)

# we only need the column that we are modeling remove all other columns
# remove columns 1 through 12 from shp.pts and rewrite it to shp.pts
shp.pts <- shp.pts[-c(1:30)]

# check the names of shp.pts to see if it worked you should only have the column with your modeling class
names(shp.pts)

## Plot to ensure alignment bw points and rasters
plot(rStack$bl3)
plot(shp.pts, add=TRUE)
#plot(shp.pts)


# this step gets the names of the rasters for use later
# convert raster stack to list of single raster layers
rList
# remove the .tif
rList <- gsub(".tif", "", rList)
names(rStack) <- rList
names(rStack)

r.stack <- rStack


# this step gets the names of the rasters for use later
# convert raster stack to list of single raster layers
r.stack.list <- unstack(r.stack)
names(r.stack.list) <- names(r.stack)
# this following lines are the extract function this allows you to
# extract the covariate values at each data point collected

## Parallelized extract: (larger datasets)
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-4)
sfLibrary(raster)
sfLibrary(rgdal)
# run parallelized 'extract' 
e.df <- sfSapply(r.stack.list, extract, y=shp.pts)
sfStop()
# clean memory
gc()
# now we need to assign the names of the covariates to the extracted values
DF <- as.data.frame(e.df)
names(DF) = tools::file_path_sans_ext(basename(names(rStack)))
names(DF)
# head() looks at the top five rows of a data frame
head(DF)
# create ID field in both dataframe and shp
DF$ID <- seq.int(nrow(DF))
shp.pts$ID <- seq.int(nrow(shp.pts))

# create the training points by merging the extracted covariate values with the shape file
train.pts = merge(shp.pts, DF, by="ID")

#check the names to ensure merge
names(train.pts)

#Inspecting the dataframe
str(train.pts)

pts.df <- as.data.frame(train.pts, stringsAsFactors = T, spatial=T)
names(pts.df)

pts.df <-pts.df[complete.cases(pts.df),]

train.pts <- pts.df
names(train.pts)
train.pts <- train.pts[-c(1,200:202)]
names(train.pts)
train.pts$Class <- as.factor(train.pts$Class)
levels(train.pts$Class)

#####################################################################3
# Data Splitting - this splits all of the data into one set of training and one set of test data
splitIndex <- createDataPartition(train.pts$Class, ### this is split on your moldeld map units
                                  p=.85,
                                  list= F,
                                  times = 1)


# now using the index we split our data
# first is the training data set which will have 70% of the records; the index refers to .7 and is in the rows position of the dataframe reference, so we'll keep 70% of the rows and all columns
trainData <- train.pts[splitIndex,]


# next is the validation data set which will have 30% of the records; the index refers to .7 so now we use a minus (-) before the index in the rows position of the dataframe reference, so we'll keep only 30% of the rows and all columns
validationData <- train.pts[-splitIndex,]


# a quick check is to see how many points are in each data set and make sure it adds to the total number of points you started with and check you've got class and covariate values attached
str(trainData)
str(validationData)


# rename trainData to train.pts for script below
train.pts <- trainData
levels(train.pts$Class)

train.pts$Class <- as.factor(train.pts$Class)
levels(train.pts$Class)

#####################################################################
# clean junk out of memory
gc()


###########################################################################
#modeling within the new domains
## Prep for Random Forest
cov.grids.names<- names(rStack)
formulaStringRF <- as.formula(paste('Class ~', paste(gsub(".tif","", cov.grids.names), collapse="+")))# put in dep variable name
ptsc <- subset(train.pts, train.pts$Class != "NA")
ptsc <- subset(ptsc, ptsc$Class != "")
ptsc <- na.omit(ptsc)# Remove any record with NA's (in any column - be careful)

## Examine class balaance
summary(as.factor(ptsc$Class),maxsum=200)

#remove any classes with only 1 obs
#names(ptsc)
#ptsc2 <- subset(ptsc, Class!= "mtssc" & Class!="mtssu" & Class!="mtsu" & Class!="sd" & Class!= "sm")


ptscc <- ptsc
# ## Pull out duplicate locations
# ptscc <- subset(ptscc, !duplicated(ptscc[c("LocID")])) #removes duplicates
# # ## Clean up class names for use in lists
# ptscc$Class <- as.character(ptscc$Class)
# ptscc$Class <- gsub(" ","", ptscc$Class)
# ptscc$Class <- gsub("&","", ptscc$Class)
# ptscc$Class <- as.factor(ptscc$Class)
## Split into train/test
# nfolds <- 5
# ptscc$sets <- sample.int(nfolds,size =length(ptscc[,1]),replace=T)
# pts_rf <- subset(ptscc, ptscc$sets == 1 | ptscc$sets == 2 | ptscc$sets==3 | ptscc$sets == 4)
# pts_test <- subset(ptscc, ptscc$sets == 5)
# pts_test$Class <- as.character(pts_test$Class)
# summary.factor(pts_test$Class)
pts_rf <- ptscc

# clean up pts_rf to remove any unwanted cols
names(pts_rf)

names(pts_rf)
## Pull out just regression matrix for SMOTE
covnames <- gsub(".tif","",cov.grids.names)
regmxnames <- c("Class",covnames)
pts_rgmtx <- pts_rf#[,regmxnames]
pts_rgmtx$Class <- as.character(pts_rgmtx$Class)
## Smote percentage calculations
classnumb.rf <- summary(as.factor(as.character(pts_rgmtx$Class)), maxsum=200)
classnumb.rf


## Set up class weights for RF or synthetic oversampling
#classwts <- max(classnumb.rf)/classnumb.rf # to get a fully balance oversampling

# If we want to tweak 
#classwts <- (1 - (classnumb.rf/max(classnumb.rf))+1)^2.5 # conservative oversampling
 classwts <- 1-(classnumb.rf^(1.2))/nrow(pts_rgmtx) ## For using classwts in RF
 classwts

## Class weights list for use in smote call
classwtslst <- lapply(split(classwts, names(classwts)), unname)

# ## Adjust weights based on expert reasoning
# classwtslst$Riparian <- 0.74
# classwtslst$Outcrops <- 6.5
# classwtslst$LoamyUplands <- 1.75
# classwtslst$SalineHills <- 2.4
# classwtslst$DeepRocky <- 3.6
# classwtslst$SalineBottoms <- 8.25
# classwtslst$Gypsum <- 7.5
# classwtslst$SandyBottoms <- 12.5
# classwtslst$Breaks <- 3.7
# classwtslst$SandyUplands <- 2.25
# classwtslst$Bottoms <- 3.9
# classwtslst$VeryShallow <- 2.5
# classwtslst$Shallow <- 2.55
# classwtslst$FinerUplands <- 2.3
# classwtslst$ClayUplands <- 1.25
# classwtslst$SalineUplands <- 3.35

## Now create new balanced training set
pts_rgmtx$Class <- as.factor(pts_rgmtx$Class)
balpts <- SmoteClassif(formulaStringRF, pts_rgmtx, C.perc = classwtslst, k=3)
# balpts <- SmoteClassif(formulaStringRF, pts_rgmtx, C.perc = "balance", k=3)
summary(balpts$Class,maxsum=200)
summary(train.pts$Class)
#begins modeling part
# read in RDS file with points that include the extracted covariate values
#comp <- as.data.frame(readRDS("points.rds"))
comp <- rbind(train.pts, balpts)

names(comp)
summary(comp$Class)
# check the levels (classes) of comp class
levels(comp$Class)

# make sure Class is a factor

#comp$Class <- as.factor(comp$Class)

# check again the levels (classes) of comp class
#levels(comp$Class)

# remove NA values from class
#comp <- comp[complete.cases(comp), ]



##################################################
# Recursive Feature Selection (RFE)
# this section is covariate reduction section


# This process is setup to run as a parallel
# in the make cluster function.

# Next, change the subsets
# to match the number of covariates that you have.

# check the number of covariates

length(comp) # number of covariates plus the class column

subsets <- c(1:(length(comp)-1))

# set seeds to get reporducable results when running the process in parallel
set.seed(238)
seeds <- vector(mode = "list", length=105)
for(i in 1:104) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[105]] <- sample.int(1000, 1)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 15,
                       repeats = 5,
                       seeds = seeds, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE

c1 <- makeCluster(detectCores()-1)
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

#confusion matrix
rf.RFE$fit


#plotting rFE
plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

# see list of predictors
predictors(rf.RFE)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *

rf.RFE

#if RFE selects over 25 covariates, use one standard error (OSE) by finding the value of (accuracy - accuracySD at the *), look for the value of accuracy closest to this, this is the number of covariates to use
#a <- predictors(rf.RFE)[1:18]

#OR to pick and choose which covariates, assign combined covariates to b, then change a to b in subset of model in lines 288 and 293
a <- predictors(rf.RFE)
b <- a[c(1:75)]

#add this layer to vector b "clim"
#if adding more than one, use append(a, c("name", "name", "etc"))
#c <- append(b,"clim")

#OR assigns the number of predictors to a, no matter how many

#a<-predictors(rf.RFE) # top number of covariates

names(rStack)<- gsub(".tif","", names(rStack))
names(rStack)

# subset the raser stack to the selected covariates
r.stack.model <- subset(rStack,b)

names(r.stack.model)

# subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class", b)])
names(comp.sub)
comp.sub

###################################################

## run the random forest model using the caret pkg


# set seeds to get reproducable results when running the process in parallel, dave says not to change this for future runs
set.seed(12)
seeds <- vector(mode = "list", length=(76))
for(i in 1:75) seeds[[i]] <- sample.int(1000, length(1:75) + 1)
seeds[[76]] <- sample.int(1000, 1)


# set up the train control, differs from DSM script, more robust, don't change in future runs
fitControl <- trainControl(method = "repeatedcv", 
                           number = 15,
                           repeats = 5,
                           p = 0.7, #30% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)
#make.names(RFE.train.all$Class)
# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-4)
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

#save model to run in a new area without points, run on the exact same covariates as ID703
save(rf.model,file = 'genLF.RData')

## below are a couple of different methods for looking and saving the confusion matrix

# set working directory to save file outputs
setwd("D:/dsm/FL_field_week/data/modeling/results")

## confusion matrix method one
confmatrix <- rf.model$confusion[1:length(rf.model$classes),1:length(rf.model$classes)]#Must match number of classes 
OOBkappa <- Kappa.test(confmatrix, conf.level = 0.95)
OOBkappa # Summary of Kappa result
write.table(confmatrix, file = paste("conf_matr_oob.txt",sep="_"), sep = "\t", row.names = TRUE) ## need to add one space to first row in excel


# confusion matrix method two
confusion.mat <- as.data.frame(rfFit$finalModel$confusion)

confusion.mat

# save confusion matrix as tab delimited txt file to import into excel
#set working directory on where to save raster layer
write.table(confusion.mat, "confusion_matrix.txt", sep = "\t")



####################################################

## Parallelized predict
rasterOptions(maxmemory = 5e+08,chunksize = 3e+06)


# prediction of classes - this part takes a while, be prepared  !!!THis is only for quick viewing in R, do not write this raster
library(randomForest)
beginCluster(detectCores()-4)
pred <- clusterR(r.stack.model, predict, args=list(model=rf.model),progress="text")
endCluster()
gc()
print(pred)
plot(pred)

# predict probabilities - this is the longest step

beginCluster(detectCores()-4)
## predprob makes a huge temp file on c drive, make sure there's room...
# Classwise probabilities for every pixel
predprob <- clusterR(r.stack.model, predict, args=list(model=rf.model, type="prob", index = 1:length(rf.model$classes)),progress="text")
## Summarize probability stack
maxfn <- function(probstk){ind <- max(probstk)
return(ind)
} # Function to find max probability
predprob_stk <- stack(predprob)
## Render max probability layer (this is the same as the raster prediction, but more efficient)
probmax <- clusterR(predprob_stk, calc, args=list(fun=maxfn),progress='text')
names(probmax) <-"probmax"
names(probmax)
intfn <- function(pmax){ind <- pmax*100
return(ind)
}
## Render integer version of max probability raster for smaller file size
probmaxint <- clusterR(probmax, calc, args=list(fun=intfn),progress='text') # not produced yet for 2018 field week
## Render integer version of probability raster stack for smaller file size
predprobint <- clusterR(predprob, calc, args=list(fun=intfn),progress='text') # not produced yet for 2018 field week
endCluster()

# save rasters
# class prediction raster
#writeRaster(pred, overwrite = TRUE, filename = "big_model_20220325.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
# probability rasters
writeRaster(predprobint, overwrite=TRUE,filename="probmatrix_sf.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(probmaxint, overwrite=TRUE,filename="probmax.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")

plot(pred)




# create the raster attribute table
# Create lookup table to match with values in raster
lookup_tab <- as.data.frame(rf.model$classes)
lookup_tab$value <- as.numeric(lookup_tab$`rf.model$classes`)
lookup_tab$predicted <- as.character(lookup_tab$`rf.model$classes`)
lookup_tab <- lookup_tab[c("value","predicted")]
write.table(lookup_tab, file = "PredRAT.txt", sep = "\t", row.names = FALSE)

#####################################################################
#2 ucmr
## Parallelized extract: (larger datasets)
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-4)
sfLibrary(raster)
sfLibrary(rgdal)
# run parallelized 'extract' 
e.df <- sfSapply(r.stack.list, extract, y=ucmr.spf) #change this one
sfStop()
# clean memory
gc()
# now we need to assign the names of the covariates to the extracted values
DF <- as.data.frame(e.df)
names(DF) = tools::file_path_sans_ext(basename(names(r.stack.list)))
names(DF)
# head() looks at the top five rows of a data frame
head(DF)
# create ID field in both dataframe and shp
DF$ID <- seq.int(nrow(DF))
ucmr.spf$ID <- seq.int(nrow(ucmr.spf)) #change these two

# create the training points by merging the extracted covariate values with the shape file
train.pts = merge(ucmr.spf, DF, by="ID") #change this one

#check the names to ensure merge
names(train.pts)

#Inspecting the dataframe
str(train.pts)

#creating dataframe from points
pts.df <- as.data.frame(train.pts, stringsAsFactors = T, spatial=T)
names(pts.df)

pts.df <-pts.df[complete.cases(pts.df),]

ucmr.train.pts <- pts.df #change this one

## Save points as a RDS file - this is a R file that contains an R object - save this for use later
saveRDS(ucmr.train.pts, file = "points.rds") #change this one

###SMOTE
## Prep for Random Forest
cov.grids.names<- names(rStack)
formulaStringRF <- as.formula(paste('Class ~', paste(gsub(".tif","", cov.grids.names), collapse="+")))# put in dep variable name
ptsc <- subset(ucmr.train.pts, ucmr.train.pts$Class != "NA")
ptsc <- subset(ptsc, ptsc$Class != "")
ptsc <- na.omit(ptsc)# Remove any record with NA's (in any column - be careful)

## Examine class balaance
summary(as.factor(ptsc$Class),maxsum=200)

#remove any classes with only 1 obs
names(ptsc)
ptsc2 <- subset(ptsc, Class!= "mtssc" & Class!="mtssu" & Class!="mtsu" & Class!="sd" & Class!= "sm")


ptscc <- ptsc2
# ## Pull out duplicate locations
# ptscc <- subset(ptscc, !duplicated(ptscc[c("LocID")])) #removes duplicates
# # ## Clean up class names for use in lists
# ptscc$Class <- as.character(ptscc$Class)
# ptscc$Class <- gsub(" ","", ptscc$Class)
# ptscc$Class <- gsub("&","", ptscc$Class)
# ptscc$Class <- as.factor(ptscc$Class)
## Split into train/test
# nfolds <- 5
# ptscc$sets <- sample.int(nfolds,size =length(ptscc[,1]),replace=T)
# pts_rf <- subset(ptscc, ptscc$sets == 1 | ptscc$sets == 2 | ptscc$sets==3 | ptscc$sets == 4)
# pts_test <- subset(ptscc, ptscc$sets == 5)
# pts_test$Class <- as.character(pts_test$Class)
# summary.factor(pts_test$Class)
pts_rf <- ptscc

# clean up pts_rf to remove any unwanted cols
names(pts_rf)
pts_rf <- pts_rf[,-c(1,3,4,6,7,84:86)]
names(pts_rf)

names(pts_rf)[c(2)]<- c('lith')
pts_rf <- pts_rf[,-c(3,4)]
names(pts_rf)

## Pull out just regression matrix for SMOTE
covnames <- gsub(".tif","",cov.grids.names)
regmxnames <- c("Class",covnames)
pts_rgmtx <- pts_rf[,regmxnames]
pts_rgmtx$Class <- as.character(pts_rgmtx$Class)
## Smote percentage calculations
classnumb.rf <- summary(as.factor(as.character(pts_rgmtx$Class)), maxsum=200)
classnumb.rf

## Set up class weights for RF or synthetic oversampling
#classwts <- max(classnumb.rf)/classnumb.rf # to get a fully balance oversampling

# If we want to tweak 
classwts <- (1 - (classnumb.rf/max(classnumb.rf))+1)^2.5 # conservative oversampling
# classwts <- 1-(classnumb.rf^(1.2))/nrow(pts_rgmtx) ## For using classwts in RF
# classwts

## Class weights list for use in smote call
classwtslst <- lapply(split(classwts, names(classwts)), unname)

# ## Adjust weights based on expert reasoning
# classwtslst$Riparian <- 0.74
# classwtslst$Outcrops <- 6.5
# classwtslst$LoamyUplands <- 1.75
# classwtslst$SalineHills <- 2.4
# classwtslst$DeepRocky <- 3.6
# classwtslst$SalineBottoms <- 8.25
# classwtslst$Gypsum <- 7.5
# classwtslst$SandyBottoms <- 12.5
# classwtslst$Breaks <- 3.7
# classwtslst$SandyUplands <- 2.25
# classwtslst$Bottoms <- 3.9
# classwtslst$VeryShallow <- 2.5
# classwtslst$Shallow <- 2.55
# classwtslst$FinerUplands <- 2.3
# classwtslst$ClayUplands <- 1.25
# classwtslst$SalineUplands <- 3.35

## Now create new balanced training set
pts_rgmtx$Class <- as.factor(pts_rgmtx$Class)
balpts <- SmoteClassif(formulaStringRF, pts_rgmtx, C.perc = classwtslst, k = 3)
# balpts <- SmoteClassif(formulaStringRF, pts_rgmtx, C.perc = "balance", k=3)
summary(balpts$Class,maxsum=200)

#begins modeling part
# read in RDS file with points that include the extracted covariate values
#comp <- as.data.frame(readRDS("points.rds"))
comp <- balpts

names(comp)

# check the levels (classes) of comp class
levels(comp$Class)

# make sure Class is a factor

#comp$Class <- as.factor(comp$Class)

# check again the levels (classes) of comp class
#levels(comp$Class)

# remove NA values from class
#comp <- comp[complete.cases(comp), ]



##################################################
# Recursive Feature Selection (RFE)
# this section is covariate reduction section


# This process is setup to run as a parallel
# in the make cluster function.

# Next, change the subsets
# to match the number of covariates that you have.

# check the number of covariates

length(comp) # number of covariates plus the class column

subsets <- c(1:(length(comp)-1))

# set seeds to get reporducable results when running the process in parallel
set.seed(238)
seeds <- vector(mode = "list", length=105)
for(i in 1:104) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[105]] <- sample.int(1000, 1)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 15,
                       repeats = 5,
                       seeds = seeds, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE

c1 <- makeCluster(detectCores()-4)
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

#confusion matrix
rf.RFE$fit


#plotting rFE
plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

# see list of predictors
predictors(rf.RFE)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *

rf.RFE

#if RFE selects over 25 covariates, use one standard error (OSE) by finding the value of (accuracy - accuracySD at the *), look for the value of accuracy closest to this, this is the number of covariates to use
#a <- predictors(rf.RFE)[1:18]

#OR to pick and choose which covariates, assign combined covariates to b, then change a to b in subset of model in lines 288 and 293
a <- predictors(rf.RFE)
b <- a[c(1:13)]

#add this layer to vector b "clim"
#if adding more than one, use append(a, c("name", "name", "etc"))
#c <- append(b,"clim")

#OR assigns the number of predictors to a, no matter how many

#a<-predictors(rf.RFE) # top number of covariates

names(rStack)<- gsub(".tif","", names(rStack))
names(rStack)

# subset the raser stack to the selected covariates
r.stack.model <- subset(rStack,b)

names(r.stack.model)

r.stack.model$pisr_dir_2021.12  <- rStack$pisr_dir_2021.12.22

# subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class", b)])
names(comp.sub)
comp.sub

###################################################

## run the random forest model using the caret pkg


# set seeds to get reproducable results when running the process in parallel, dave says not to change this for future runs
set.seed(12)
seeds <- vector(mode = "list", length=(76))
for(i in 1:75) seeds[[i]] <- sample.int(1000, length(1:75) + 1)
seeds[[76]] <- sample.int(1000, 1)


# set up the train control, differs from DSM script, more robust, don't change in future runs
fitControl <- trainControl(method = "repeatedcv", 
                           number = 15,
                           repeats = 5,
                           p = 0.7, #30% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)
#make.names(RFE.train.all$Class)
# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-4)
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

#save model to run in a new area without points, run on the exact same covariates as ID703
save(rf.model,file = 'lbig_model_ID703_20220325.RData')

## below are a couple of different methods for looking and saving the confusion matrix

# set working directory to save file outputs
setwd("D:/dsm/FL_field_week/data/modeling/results")

## confusion matrix method one
confmatrix <- rf.model$confusion[1:length(rf.model$classes),1:length(rf.model$classes)]#Must match number of classes 
OOBkappa <- Kappa.test(confmatrix, conf.level = 0.95)
OOBkappa # Summary of Kappa result
write.table(confmatrix, file = paste("conf_matr_oob.txt",sep="_"), sep = "\t", row.names = TRUE) ## need to add one space to first row in excel


# confusion matrix method two
confusion.mat <- as.data.frame(rfFit$finalModel$confusion)

confusion.mat

# save confusion matrix as tab delimited txt file to import into excel
#set working directory on where to save raster layer
write.table(confusion.mat, "confusion_matrix.txt", sep = "\t")



####################################################

## Parallelized predict
#rasterOptions(maxmemory = 5e+08,chunksize = 3e+06)

names(r.stack.model)
names

# prediction of classes - this part takes a while, be prepared
library(randomForest)
beginCluster(detectCores()-4)
pred <- clusterR(r.stack.model, predict, args=list(model=rf.model),progress="text")
endCluster()
gc()
print(pred)
plot(pred)

# predict probabilities - this is the longest step
beginCluster()
## predprob makes a huge temp file on c drive, make sure there's room...
# Classwise probabilities for every pixel
predprob <- clusterR(r.stack.model, predict, args=list(model=rf.model, type="prob", index = 1:length(rf.model$classes)),progress="text")
## Summarize probability stack
maxfn <- function(probstk){ind <- max(probstk)
return(ind)
} # Function to find max probability
predprob_stk <- stack(predprob)
## Render max probability layer
probmax <- clusterR(predprob_stk, calc, args=list(fun=maxfn),progress='text')
names(probmax) <-"probmax"
names(probmax)
intfn <- function(pmax){ind <- pmax*100
return(ind)
}
## Render integer version of max probability raster for smaller file size
probmaxint <- clusterR(probmax, calc, args=list(fun=intfn),progress='text') # not produced yet for 2018 field week
## Render integer version of probability raster stack for smaller file size
predprobint <- clusterR(predprob, calc, args=list(fun=intfn),progress='text') # not produced yet for 2018 field week
endCluster()

# save rasters
# class prediction raster
writeRaster(pred, overwrite = TRUE, filename = "GenLandform.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
# probability rasters
writeRaster(predprobint, overwrite=TRUE,filename="probmatrix_sf.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(probmaxint, overwrite=TRUE,filename="probmax.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")

plot(pred)




# create the raster attribute table
# Create lookup table to match with values in raster
lookup_tab <- as.data.frame(rf.model$classes)
lookup_tab$value <- as.numeric(lookup_tab$`rf.model$classes`)
lookup_tab$predicted <- as.character(lookup_tab$`rf.model$classes`)
lookup_tab <- lookup_tab[c("value","predicted")]
write.table(lookup_tab, file = "PredRAT.txt", sep = "\t", row.names = FALSE)

