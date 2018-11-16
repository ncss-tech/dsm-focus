# Random Forest model building and predictions for parent material
# GRSM field week 
# Dave White with code chunks from Travis Nauman



## Load and install packages
required.packages <- c("caret", "rgdal", "raster", "doParallel", "psych", 
                       "maptools", "amap", "dplyr", "sp", "snow", "snowfall", 
                       "plyr", "randomForest")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)


# load raster data from the covariate preporcessing and create a rastet stack
# set working directory for the covariates
setwd("C:/rwork/cov/")

# read in raster layers to a list
r.list=list.files(getwd(), pattern="tif$", full.names = FALSE)

r.list

#create raster stack
r.stack <- stack(r.list)

#check visually
plot(r.stack$dem10m)


# bring in training dataset created earlier
# set working directory
setwd("C:/rwork/")

# read data
train <- readRDS("train_pts.rds")

# inspect train object
str(train)
names(train)

# need to clean up the train dataframe
# keep only the column that we want to model and the covariate columns
# for parent material : remove cols 1 to 4 and cols 6 to 8
all.df <- train[, -c(1:4, 6:8)]

# inspect dataframe
names(all.df)

# there are NAs in the pmkind col
# remove all rows containing NA
all.df <- all.df[complete.cases(all.df), ]




# clean up pmkind classes

# inspecting factors levels of pmkind
levels(all.df$pmkind)

# change "alluvium & alluvium"
all.df$pmkind[all.df$pmkind == "alluvium & alluvium"] <- "alluvium" #changes factor level to an existing one
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "alluvium & colluvium" 
all.df$pmkind <- revalue(all.df$pmkind, c("alluvium & colluvium" = "allcol")) #changes factor level to a new one
levels(all.df$pmkind)

# change "colluvium & alluvium"
all.df$pmkind[all.df$pmkind == "colluvium & alluvium"] <- "allcol"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "colluvium & residuum" 
all.df$pmkind <- revalue(all.df$pmkind, c("colluvium & residuum" = "colres"))
levels(all.df$pmkind)

# change "creep deposits & residuum" 
all.df$pmkind[all.df$pmkind == "creep deposits & residuum"] <- "colres"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "creep deposits & residuum & residuum"
all.df$pmkind[all.df$pmkind == "creep deposits & residuum & residuum"] <- "colres"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "residuum & colluvium"  
all.df$pmkind[all.df$pmkind == "residuum & colluvium"] <- "colres"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "residuum & creep deposits"   
all.df$pmkind[all.df$pmkind == "residuum & creep deposits"] <- "colres"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "residuum & residuum"    
all.df$pmkind[all.df$pmkind == "residuum & residuum"] <- "residuum"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "residuum & residuum & creep deposits"  
all.df$pmkind[all.df$pmkind == "residuum & residuum & creep deposits"] <- "colres"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "sandstone"   
all.df$pmkind[all.df$pmkind == "sandstone"] <- "residuum"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "saprolite"   
all.df$pmkind[all.df$pmkind == "saprolite"] <- "residuum"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# change "slate"   
all.df$pmkind[all.df$pmkind == "slate" ] <- "residuum"
all.df$pmkind <- factor(all.df$pmkind)
levels(all.df$pmkind)

# remove organic....
all.df <- all.df[which(all.df$pmkind != "organic, unspecified & residuum"), ]
all.df$pmkind <- factor(all.df$pmkind)

# check factor levels on last time
levels(all.df$pmkind)



# Prepare data for feature selection and model training

# change name of first column to Class
names(all.df)[c(1)] <- c('Class')  

# Inspect all.df, Class should be the first column, followed by the covariates
names(all.df)




# Recursive Feature Selection (RFE) 
#
# Unsupervised covariate selection process.
# 
# If data include levels that have only 1 cases,  it does not work... 
# Gives  Error in { : task 3 failed - "Can't have empty classes in y."

####  Select levels that have more than 1 cases to proceed with the rfe 
# Select components that have > 1 cases
subset(table(all.df$Class), table(all.df$Class) > 1)


names(subset(table(all.df$Class), table(all.df$Class) > 1))



# Use this to get names of factors that have more than one case
cat(paste(shQuote(names(subset(table(all.df$Class), table(all.df$Class) > 1)), type="cmd"), collapse=", "))

# manually paste the result here and add the 'c(' and the ')' to bookend the list
# manually paste the new list here for subsetting the dataframe
all.sub <- subset(all.df, Class %in% c("allcol", "alluvium", "colluvium", "colres", "residuum"))

# the number of observations we are left with to make predictions
nrow(all.sub)

# CRITICAL STEP! You must assign the new subset as a factor for this to work
all.sub$Class <- factor(all.sub$Class)


# Run Recursive Feature Eliminataion. This selects the covariates that will build the best RF model

# This process is setup to run as a parallel process. Set you number of cpu cores
# in the make cluster function. This example uses detect cores to use all available. To set
# the number of cores just type the number in place of detect cores(). Next, change the subsets
# to match the number of covariates that you have. In this case we had 43. 
cpus <- detectCores()-1

#The simulation will fit models with subset sizes of 43, 42, 41.....1. 
subsets <- c(1:50, 80, 100)

# set seeds to get reporducable results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=76)
for(i in 1:75) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[76]] <- sample.int(1000, 1)


# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 15,
                       repeats = 5,
                       seeds = seeds, 
                       verbose = FALSE)

## implementing rfe parallel process
c1 <- makeCluster(cpus, type='SOCK')
registerDoParallel(c1)
set.seed(9)
rf.RFE <- rfe(x = all.sub[,-1],
              y = all.sub$Class,
              sizes = subsets,
              rfeControl = ctrl.RFE,
              allowParallel = TRUE
)
stopCluster(c1)              

gc()

# Look at the results
rf.RFE # rfe selected 22 of 105 covariates

rf.RFE$fit

rf.RFE$results

plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

# See list of predictors
predictors(rf.RFE)


# 4.5 Variables selected using RFE, put into a formula for easy modelling later. 
head(all.sub[,c("Class",predictors(rf.RFE))])
RFE.train.all <- (all.sub[,c("Class",predictors(rf.RFE))])


# subset raster stack based on covaiates selected from rfe
# look at selected covariates
predictors(rf.RFE)

# output from above
> predictors(rf.RFE)
[1] "relht16"     "relht32"     "relht8"      "twi"         "relht64"     "relht4"      "nopen"       "sagawi"     
[9] "mca"         "relht2"      "rockon"      "ls8off1"     "tri"         "relht1"      "aspect"      "slopedeg"   
[17] "nd4t3on"     "ls8on3"      "relmeanht32" "tpi2000"     "mrvbf"       "ls8on4"


# create new raster stack to for model prediction later on, from the predictors above
r.stack.model <- subset(r.stack, predictors(rf.RFE))

# inspect new raster stack to see if matches rfe results
names(r.stack.model)

# clean up working environment
to.remove <- ls()
matches <- c("RFE.train.all", "r.stack.model", "cpus") #variables to keep
to.remove <- c(to.remove[!grepl(paste0(matches, collapse = "|"), to.remove)], "to.remove")
rm(list=to.remove)
gc()


#####################################################################
#The simulation will fit models with subset sizes of 43, 42, 41.....1. 
subsets <- c(1:50, 80, 100)

# set seeds to get reporducable results when running the process in parallel
set.seed(12)
seeds <- vector(mode = "list", length=76)
for(i in 1:75) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[76]] <- sample.int(1000, 1)


# set up the train control
fitControl <- trainControl(method = "repeatedcv", 
                           number = 15,
                           repeats = 5,
                           p = 0.8, #30% used for test set, 70% used for training set
                           selectionFunction = 'best', 
                           classProbs = T,
                           savePredictions = T, 
                           returnResamp = 'final',
                           search = "random",
                           seeds = seeds)


# 5.1  Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(cpus, type='SOCK')
registerDoParallel(c1)
set.seed(48)
rfFit = train(Class ~ ., data = RFE.train.all,
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


# 5.1.2 Inspect rfFit
rfFit$finalModel

## Convert caret model into randomforest model for raster prediction
rf.model <- rfFit$finalModel

plot(rf.model)
legend("topright", colnames(rf.model$err.rate),col=1:6,cex=0.8,fill=1:6)


# run again with 260 trees
c1 <- makeCluster(cpus, type='SOCK')
registerDoParallel(c1)
set.seed(48)
rfFit = train(Class ~ ., data = RFE.train.all,
              "rf", 
              trControl = fitControl, 
              ntree = 260, #number of trees default is 500, which seems to work best anyway. 
              tuneLength=10, 
              metric = 'Kappa', 
              na.action=na.pass,
              keep.forest=TRUE, # added this line and the next for partial dependance plots
              importance=TRUE)
stopCluster(c1)

gc()

# 5.1.2 Inspect rfFit
rfFit$finalModel

## Convert caret model into randomforest model for raster prediction
rf.model <- rfFit$finalModel

plot(rf.model)
legend("topright", colnames(rf.model$err.rate),col=1:6,cex=0.8,fill=1:6)


## Parallelized predict
rasterOptions(maxmemory = 5e+08,chunksize = 3e+07)
beginCluster(cpus, type='SOCK')
pred <- clusterR(r.stack.model, predict, args=list(model=rf.model),progress="text")
endCluster()
gc()

plot(pred)

# set working directory on where to save raster layer
setwd("C:/rwork/model_outputs/")

# save raster layer
writeRaster(pred, filename = "PM_rf_GRSM.tif", format="GTiff", options = c("COMPRESS=DEFLATE", "TFW=YES"), datatype='INT1U', overwrite=TRUE, progress = 'text')

# create raster attribute tabel for use in arcgis
rat <- as.data.frame(rf.model$classes)
rat$value <- as.numeric(rat$'rf.model$classes')
rat$predicted <- as.character(rat$'rf.model$classes')
rat <- rat[c("value", "predicted")]
rat
write.csv(rat, "PM_rf_RAT.csv")

# create probability predictions - From Travis Nauman
# claswise probabilities for every pixel
beginCluster(cpus, type = 'SOCK')
predprob <- clusterR(r.stack.model, predict, args = list(model=rf.model, type = "prob",
                                                         index = 1:length(rf.model$classes)), progress = "text")
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
probmaxint <- clusterR(probmax, calc, args=list(fun=intfn),progress='text')
## Render integer version of probability raster stack for smaller file size
predprobint <- clusterR(predprob, calc, args=list(fun=intfn),progress='text')
endCluster()
gc()

## Now save rasters
writeRaster(predprobint, overwrite=TRUE,filename="PM_rf_GRSM_5plusprobmatrix_sf.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(probmaxint, overwrite=TRUE,filename="PM_rf_GRSM_5plus_probmax.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")

