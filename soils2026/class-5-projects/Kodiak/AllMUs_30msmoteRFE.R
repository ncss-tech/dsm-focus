# modeling Kodiak island map units for mlra All map units

# load and install package"s
required.packages <- c("UBL", "caret", "rgdal", "raster", "doParallel", "psych", "maptools", "dplyr", "sp", "snow", "snowfall", "plyr", "randomForest", "fmsb")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

# set the working directory to where the covariate and pedon observation data are located
setwd('C:/cov30m')
getwd() # check to make sure the working directory is set

# read in the raster layers
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack
rStack <- stack(rList)
names(rStack)


# this step gets the names of the rasters for use later
# convert raster stack to list of single raster layers
rList
# remove the .tif
rList <- gsub(".tif", "", rList)
rList
names(rStack)
names(rStack) <- rList
names(rStack)

###########################
setwd('C:/cov30m')
## load in shapefile with points, if points are in a different folder change the dsn="." to the directory that it is in
shp.pts <- readOGR(dsn=".", layer="AllMUs")

# the points and the raster stack need to be in the same projection
crs(rStack)
crs(rStack) <- "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m
+no_defs "
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
shp.pts <- shp.pts[-c(1:31)]

# check the names of shp.pts to see if it worked you should only have the column with your modeling class
names(shp.pts)
names(shp.pts) <- "Class" ## changed name to Class for script below

#convert to data.frame
shp.df <- as.data.frame(shp.pts)
names(shp.df)
colnames(shp.df) <- c("Class", "x", "y")
names(shp.df)

#### split data into training and validation
# data splitting
#####################################################################3
# Data Splitting - this splits all of the data into one set of training and one set of test data

splitIndex <- createDataPartition(shp.df$Class, ### this is split on your moldeld map units
                                  p=.9,
                                  list= F,
                                  times = 1)


# now using the index we split our data
# first is the training data set which will have 70% of the records; the index refers to .7 and is in the rows position of the dataframe reference, so we'll keep 70% of the rows and all columns
trainData <- shp.df[splitIndex,]


# next is the validation data set which will have 30% of the records; the index refers to .7 so now we use a minus (-) before the index in the rows position of the dataframe reference, so we'll keep only 30% of the rows and all columns
validationData <- shp.df[-splitIndex,]

# convert data to shp file
names(trainData)
print(shp.pts@proj4string)
prjinfo <- CRS("+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

xy <- trainData[,c(2,3)]
train.pts <- SpatialPointsDataFrame( coords = xy, data = trainData, 
                                     proj4string = prjinfo)


names(validationData)
xy <- validationData[,c(2,3)]
val.pts <- SpatialPointsDataFrame(coords= xy, data = validationData,
                                  proj4string = prjinfo)

#save the validation poijnts for use later
setwd("C:/cov30m/All_results")
writeOGR(val.pts, dsn = "C:/cov30m/All_results" , "validationPts", driver="ESRI Shapefile", overwrite_layer = T)
#val.pts <- readOGR(dsn = "C:/cov/resample/225_results", "validationPts")


## Plot to ensure alignment bw points and rasters
# convert train.pts to shp.pts for script below.
shp.pts <- train.pts
plot(rStack$bl3)
plot(shp.pts, add=TRUE)
#plot(shp.pts)

# this step gets the names of the rasters for use later
# convert raster stack to list of single raster layers
r.stack.list <- unstack(rStack)
names(r.stack.list) <- names(rStack)
names(r.stack.list)
# this following lines are the extract function this allows you to
# extract the covariate values at each data point collected
rasterOptions(maxmemory = 6e+09,chunksize = 8e+09, memfrac = .9)
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
str(pts.df)


pts.df <-pts.df[complete.cases(pts.df),]

train.pts <- pts.df
names(train.pts)
train.pts <- train.pts[-c(1,3,4,214:216)]
names(train.pts)
train.pts$Class <- as.factor(train.pts$Class)
levels(train.pts$Class)
#rStack$gmph100 <- as.factor(rStack$gmph100)
#rStack$gmph500 <- as.factor(rStack$gmph500)
# clean junk out of memory
gc()




# a quick check is to see how many points are in each data set and make sure it adds to the total number of points you started with and check you've got class and covariate values attached
str(trainData)
str(validationData)


####Insert Smote


#comp <- train.pts # when not smoting use this to assign trainpts to comp for script below


#######################################################################

# make sure Class is a factor

#comp$Class <- as.factor(comp$Class)

# check again the levels (classes) of comp class
#levels(comp$Class)

# remove NA values from class
#comp <- comp[complete.cases(comp), ]



# this section gets the data ready for covariate reduction, where we select the covariates that are most usefule for modeling

# read in RDS file with points that include the extracted covariate values
#comp <- as.data.frame(readRDS("points.rds"))
# remove the ID column and change the first column name to class
#comp <- comp[,-c(1,107,108)] # removes the ID column
#names(comp) # check the column names to make sure ID is removed
#names(comp)[c(1)] <- c('Class') # change name of first col to class
#names(comp) # check names of comp
#comp <- as.data.frame(comp)
comp <- as.data.frame(train.pts)
names(comp)
names(comp)
summary(comp$Class)
# check the levels (classes) of comp class
levels(comp$Class)
names(comp)
# check the levels (classes) of comp class
#names(comp)
#levels(comp$Class)

# remove NA values from class
#comp <- comp[complete.cases(comp), ]

##################################################

#train.pts$Class <- as.factor(train.pts$Class)
#levels(train.pts$Class)
# Recursive Feature Selection (RFE)
# this section is covariate reduction section


# This process is setup to run as a parallel
# in the make cluster function.

# Next, change the subsets
# to match the number of covariates that you have.

# check the number of covariates

length(comp) # number of covariates plus the class column
#subsets <- c(1:(length(comp)-1))
subsets <- c(1,10:25, 50, 100, 150, 211)

# set seeds to get reporducable results when running the process in parallel
set.seed(238)
seeds <- vector(mode = "list", length=112)
for(i in 1:111) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[112]] <- sample.int(1000, 1)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 5,
                       repeats = 3,
                       seeds = seeds, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE

c1 <- makeCluster(detectCores())
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

confusion.mat <- as.data.frame(rf.RFE$fit$confusion)
write.table(confusion.mat, "allRFEconfusion_matrix.txt", sep = "\t")

rf.RFE$results

#plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
#plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

# see list of predictors
predictors(rf.RFE)

# the rfe function retuns the covariates with the highest accuracy for the rf model
# view the highest accuracy noted by the *

rf.RFE

# take the accuracy and subtract the accuracySD. look in the results of rf.RFE and find the accuracy that is > or = to this value. this is the number of covariates to use below

predictors(rf.RFE) # top number of covariates

# look at the top number of covariates that are equal to greater than the accuracy minus the accuracySD
predictors(rf.RFE)[1:50]

# assign this to a variable
a <- predictors(rf.RFE)[1:50]

# subset the covariate stack and the data frame by the selected covariates the variable a is your selected covariates

# subsed the raser stack to the selected covariates
r.stack.model <- subset(rStack, a)
# subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class", a)])
names(comp.sub)
levels(comp.sub$Class)
names(comp.sub)

## run the random forest model using the caret pkg
######
######
##########################################################
# smote
train.pts <- comp.sub # assign comp to train.pts to smote
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
classwts <- (1 - (classnumb.rf/max(classnumb.rf))+1)^2.5 # conservative oversampling
#classwts <- 1-(classnumb.rf^(1.2))/nrow(pts_rgmtx) ## For using classwts in RF
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
comp.sub <- balpts # when smoting use pal pts as comp

######
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
                           number = 15,
                           repeats = 3,
                           p = 0.8, #30% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)

#make.names(RFE.train.all$Class)
# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-1)
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



## below are a couple of different methods for looking and saving the confusion matrix

# set working directory to save file outputs
setwd("C:/cov30m/All_results")

## confusion matrix method one
confmatrix <- rf.model$confusion[1:length(rf.model$classes),1:length(rf.model$classes)]#Must match number of classes 
OOBkappa <- Kappa.test(confmatrix, conf.level = 0.95)
OOBkappa # Summary of Kappa result
write.table(confmatrix, file = paste("Allconf_matr_oob.txt",sep="_"), sep = "\t", row.names = TRUE) ## need to add one space to first row in excel


# confusion matrix method two
confusion.mat <- as.data.frame(rfFit$finalModel$confusion)

confusion.mat

# save confusion matrix as tab delimited txt file to import into excel
#set working directory on where to save raster layer
write.table(confusion.mat, "Allconfusion_matrix.txt", sep = "\t")



####################################################

## Parallelized predict
#rasterOptions(maxmemory = 5e+08,chunksize = 3e+06)
rasterOptions(maxmemory = 6e+09,chunksize = 8e+04, memfrac = .9)

# prediction of classes
library(randomForest)
beginCluster(detectCores()-4)
pred <- clusterR(r.stack.model, predict, args=list(model=rf.model),progress="text")
#pred <- clusterR(rStack, predict, args=list(model=rf.model),progress="text")
endCluster()
gc()

# predict probabilities
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
writeRaster(pred, overwrite = TRUE, filename = "AllMUscomps.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
# probability rasters
writeRaster(predprobint, overwrite=TRUE,filename="probmatrix_sf.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(probmaxint, overwrite=TRUE,filename="probmax.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")

plot(pred)




# create the raster attribute table
# Create lookup table to match with values in raster
lookup_tab <- as.data.frame(rf.model$classes)
lookup_tab$predicted <- as.character(lookup_tab$`rf.model$classes`)
lookup_tab$value <- as.numeric(as.factor(lookup_tab$predicted))
lookup_tab <- lookup_tab[c("value","predicted")]
lookup_tab
write.table(lookup_tab, file = "220MUsPredRAT.txt", sep = "\t", row.names = FALSE)














#####generating a confusion matrix using a table with observed and predicted classes at point locations

###set up session
#load necessary libraries
library(caret)
library(lattice)
library(raster)
library(rgdal)
library(plyr)


#check working directory (put the path to your modeling folder)
# make sure the the slashes are /
setwd("C:/cov/resample/All_results")
getwd()

#read in class raster (output from prediction step)
r.class <- raster("AllMUscomp_class1.tif")


#read in validation points
#
validation <- readOGR("validationPts.shp")


#plot to check alignment of class raster and validation points
plot(r.class)
points(validation, pch=19, col="red")

#extract the predicted class values
validation$predicted <- extract(r.class, validation)

#check
head(validation)
names(validation)

#change name of observed values column from class to observed
colnames(validation@data)[5] <- "observed"

names(validation) #check column names

#subset the validation object to keep only the observed and predicted columns
x <- validation@data #convert validation data to data.frame
x <- subset(x, select=c('observed', 'predicted')) #remove all columns except for observed and predicted

#verify data.frame to ensure you have one column of observed values and one column of predicted values
str(x)
head(x)


#utilize the raster attribute table to map values and convert the predicted column to match the data type of the observed column
x$predicted <- mapvalues(x$predicted, from =RAT$Value, to = RAT$Class)

#check to ensure both columns are the same data type
head(x)


###IMPORTANT###
#Your observed and predicted columns MUST contain the same number of classes or levels (e.g. if 5 classes are represented in observed then the same 5 classes must be represented in predicted) otherwise the confusionmatrix command will fail; you cannot have a class present in your observed data that is not present in your predicted data, and vice versa

#columns need to be changed to a factor so R does not treat it as integer
x$observed <- as.factor(x$observed)
x$predicted <- as.factor(x$predicted)

#check number of levels in each factor to make sure they are the same
str(x)

#check the levels of observed
levels(x$observed)
#check the levels of predicted
levels(x$predicted)

#IF str(x) SHOWS THAT YOU HAVE THE SAME NUMBER OF LEVELS SKIP THIS SECTION AND PROCEED ON LINE 90; IF THE LEVELS DO NOT HAVE THE SAME NUMBER PROCEED WITH THE FOLLOWING LINES OF CODE:

#select and remove observed values that are not present in the predicted data
x <- subset(x, (observed %in% x$predicted)) 
#drop the unused levels
x$observed <- droplevels(x$observed)
#check the levels
levels(x$observed)
levels(x$predicted)
#check the data.frame
head(x)


###generate the confusion matrix and other accuracy metrics
#if you don't understand some of the terms in the output...then Google to learn more
confusionMatrix(x$observed, x$predicted, positive = NULL, prevalence = NULL, mode = "everything")





















##########################################################
# smote
train.pts <- comp.sub # assign comp to train.pts to smote
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
classwts <- (1 - (classnumb.rf/max(classnumb.rf))+1)^2.5 # conservative oversampling
#classwts <- 1-(classnumb.rf^(1.2))/nrow(pts_rgmtx) ## For using classwts in RF
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
comp.sub <- balpts # when smoting use pal pts as comp
