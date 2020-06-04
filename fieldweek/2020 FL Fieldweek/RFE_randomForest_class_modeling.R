#  2/4/2020 

# extract covariate data at observation points
#
# covariate data reduction using recursive feature elimination
#
# random forest model of basic ecosites implemented using the caret package
#
#
# Load and install packages
required.packages <- c("caret", "rgdal", "raster", "doParallel", "psych", "maptools", "dplyr", "sp", "snow", "snowfall", "plyr", "randomForest", "fmsb")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)



# set directory that contains the covariates and point data
# *** pay attention to the / below
setwd("D:/dsm/FL_field_week/data/modeling")
getwd()

# read in raster layers to create raster stack
r.list=list.files(getwd(), pattern="tif$", full.names = FALSE)
r.list

#create raster stack of covariates
r.stack <- stack(r.list)

# check the names of the raster stack
names(r.stack)


## load in shapefile with points, if points are in a different folder change the dsn="." to the directory that it is in
shp.pts <-readOGR(dsn=".", layer="field_data_0206")


# the points and the raster stack need to be in the same projection

# r brings in a points shp file in as a SpatialPointsDataFrame (SPDF) object
# check the names of your SPDF
names(shp.pts)

# we only need the column that we are modeling remove all other columns
# remove columns 1 through 12 from shp.pts and rewrite it to shp.pts
shp.pts <- shp.pts[-c(1:12,14:18)]

# check the names of shp.pts to see if it worked you should only have the column with your modeling class
names(shp.pts)

## Plot to ensure alignment bw points and rasters
plot(r.stack$rockdry)
plot(shp.pts, add=TRUE)


# this step gets the names of the rasters for use later
# convert raster stack to list of single raster layers
r.stack.list <- unstack(r.stack)
names(r.stack.list) <- names(r.stack)

# this following lines are the extract function this allows you to
# extract the covariate values at each data point collected

## Parallelized extract: (larger datasets)
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)
sfLibrary(raster)
sfLibrary(rgdal)
# run parallelized 'extract' 
e.df <- sfSapply(r.stack.list, extract, y=shp.pts)
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
shp.pts$ID <- seq.int(nrow(shp.pts))

# create the training points by merging the extracted covariate values with the shape file
train.pts = merge(shp.pts, DF, by="ID")

#check the names to ensure merge
names(train.pts)

## Save points as a RDS file - this is a R file that contains an R object - save this for use later
#saveRDS(train.pts, file = "points.rds")

# clean junk out of memory
gc()

###################################################################
# this section gets the data ready for covariate reduction, where we select the covariates that are most usefule for modeling

# read in RDS file with points that include the extracted covariate values
#comp <- as.data.frame(readRDS("points.rds"))
comp <- as.data.frame(train.pts)

names(comp)

# remove the ID column and change the first column name to class
comp <- comp[,-c(1,107,108)] # removes the ID column
names(comp) # check the column names to make sure ID is removed
names(comp)[c(1)] <- c('Class') # change name of first col to class
names(comp) # check names of comp

# check the levels (classes) of comp class
levels(comp$Class)

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

rf.RFE$fit

rf.RFE$results

plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
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
predictors(rf.RFE)[1:5]

# assign this to a variable
a <- predictors(rf.RFE)[1:5]

# subset the covariate stack and the data frame by the selected covariates the variable a is your selected covariates

# subsed the raser stack to the selected covariates
r.stack.model <- subset(r.stack, a)
# subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class", a)])
names(comp.sub)

             
## run the random forest model using the caret pkg

#subsets max number should be the same or equal to the number of covariates being used

# determine number of covariates
length(a)

subsets <- length(a)

# set seeds to get reproducable results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=76)
for(i in 1:75) seeds[[i]] <- sample.int(1000, length(1:subsets) + 1)
seeds[[76]] <- sample.int(1000, 1)


# set up the train control
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


# prediction of classes
library(randomForest)
beginCluster()
pred <- clusterR(r.stack.model, predict, args=list(model=rf.model),progress="text")
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
writeRaster(pred, overwrite = TRUE, filename = "comp_class.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
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










