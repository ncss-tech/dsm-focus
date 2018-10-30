######################
## Random Forest script that includes:
## Extraction of covariates to points
## Recursive Feature Elimination (RFE) to narrow down covariates
## Confustion matrix creation for out-of-bag (OOB) predictions
## Kappa calculation (OOB)
## Most steps parallelized
######################
## Travis Nauman
## 10/30/2018

# Workspace setup
# Install packages if not already installed
required.packages <- c("raster", "sp", "rgdal", "randomForest", "snow", "snowfall", "quantregForest","dplyr", "ggplot2", "doParallel","caret","fmsb")# might need snowfall
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase active memory useable by raster package: Windows only
memory.limit(110000) # Only increase if you have the capacity via RAM or SSD page space
rasterOptions(maxmemory = 5e+08, chunksize = 4e+07)
#options(scipen = 999)
#par(mar=c(0.3,0.3,0.3,0.3))

## Key Folder Locations
predfolder <- "C:/models/DSM_FieldWeek_2018/PSCmodel_share" # Folder where predictions will go
covfolder <- "C:/models/DSM_FieldWeek_2018/covariates" # Folder where covariates reside

######### Grid Prep #################
## Make list of grids
setwd(covfolder)
cov.grids <- list.files(pattern=".tif$")
## If points need to be matched up to grids ###
projgrid = raster(cov.grids[1])
## Or Make a stack of grids to extract all at once (for smaller datasets)
#cov.stack <- stack()
cov.proj <- projection(projgrid)
#shp.pts <- spTransform(shp.pts, CRS(cov.proj)) # project to match rasters

######## Clip field data to study extent ###########
setwd("C:/models/DSM_FieldWeek_2018/Boundary_10m") # Folder with study area data
polybound <- readOGR(".", "anakeesta_huc12s_diss") # Study area polygon
shp.pts <- readRDS("C:/models/DSM_FieldWeek_2018/PSCmodel_share/training_data.rds") # Soil pedons from NASIS via Steve Roecker
polybound <- spTransform(polybound, cov.proj)
shp.pts <- spTransform(shp.pts, cov.proj)

## Parallelized extraction of covariate values to field observation locations
cpus <- detectCores()-1 # Number of cpus to use: set to total # minus 1
setwd(covfolder) # Set working dir to location with covariate rasters
sfInit(parallel=TRUE, cpus=cpus)
sfExport("shp.pts", "cov.grids")
sfLibrary(raster)
sfLibrary(rgdal)
ov.lst <- sfLapply(cov.grids, function(i){try( raster::extract(raster(i), shp.pts) )})
snowfall::sfStop()
ov.lst <- as.data.frame(ov.lst)
names(ov.lst) = tools::file_path_sans_ext(basename(cov.grids))
ov.lst$DID <- seq.int(nrow(ov.lst))
shp.pts$DID <- seq.int(nrow(shp.pts))
pts.ext <- merge(as.data.frame(shp.pts),ov.lst, by="DID")

## Save points
setwd(predfolder)
saveRDS(pts.ext, "dsm18_psc.rds") # save pedons with extracted values for future use
## Extracted points for model reruns
# pts.ext <- readRDS("C:/models/DSM_FieldWeek_2018/PSCmodel/dsm18_psc.rds")

############
#### Run Recursive Feature Eliminataion. This selects the covariates that will build the best RF model

# This process is setup to run as a parallel process. Set you number of cpu cores
# in the make cluster function. This example uses detect cores to use all available. To set
# the number of cores just type the number in place of detect cores(). Next, change the subsets
# to match the number of covariates that you have. In this case we had 43. 


#The simulation will fit models with subset sizes of 43, 42, 41.....1. 
subsets <- c(1:length(cov.grids))

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

## highlight and run everything from c1 to stopCluster(c1) to run RFE
gc()
pts.ext$Class <- pts.ext$First_fam ## This set which variable is the dependent (y) variable, UPDATE EVERY TIME
ptspred.list <- gsub(".tif","", cov.grids)# Take .tif off of the grid list to just get the variable names
ptspred.list <- c("Class",ptspred.list) #Add dependent variable
comp.sub <- pts.ext[c(ptspred.list)]
comp.sub$Class <- as.factor(comp.sub$Class)
c1 <- makeCluster(detectCores()-1, type='PSOCK')
registerDoParallel(c1)
set.seed(9)
rf.RFE <- rfe(x = comp.sub[,-1],
              y = comp.sub$Class,
              sizes = subsets,
              rfeControl = ctrl.RFE,
              allowParallel = TRUE
)
stopCluster(c1) # Ends parallel operations             

gc()
setwd(predfolder)
saveRDS(rf.RFE, "rf.RFE_soilPSC.rds") # Save RFE object
# rf.RFE <- readRDS("rf.RFE_soilPSC.rds") # If using in new session

## Look at the results
rf.RFE
plot(rf.RFE) # default plot is for Accuracy, but it can also be changed to Kappa
plot(rf.RFE, metric="Kappa", main='RFE Kappa')
plot(rf.RFE, metric="Accuracy", main='RFE Accuracy')

## See list of predictors
predictors(rf.RFE)

## Variables selected using RFE, put into a formula for easy modelling later. 
head(comp.sub[,c("Class",predictors(rf.RFE))])
RFE.train.all <- (comp.sub[,c("Class",predictors(rf.RFE))])

## Setting up New Data raster stack of entire basin using only those rasters from rfe
## get list of covariates from rfe
predictors(rf.RFE)

## create new raster stack to for model development from the predictors above
## Reference covar rasters to use in prediction
setwd(covfolder)
rasters <- stack(cov.grids)
names(rasters)
r.stack.model <- subset(rasters, predictors(rf.RFE))
names(r.stack.model)

## Prep for Random Forest
classname <- "PSC" # Name for dependent variable for naming output files
prop <- "Class" ## Dependent variable

## Prep for Random Forest
rfe.list <- predictors(rf.RFE)
formulaStringRFE <-as.formula(paste('Class ~',paste(rfe.list, collapse="+"))) # Formula with reduced # of covariates
formulaStringRF <- as.formula(paste('Class ~', paste(gsub(".tif","", cov.grids), collapse="+"))) # Formula with all covariates
ptsc <- na.omit(pts.ext)# Remove any record with NA's (in any column - be careful)
ptsc <- subset(ptsc, ptsc$Class != "NA")
## Remove duplicate locations (not 3D model)
ptsc$LocID <- paste(ptsc$x, ptsc$y, sep = "")
pedonLocations <- unique(ptsc$LocID) # if length differs from # of rows in ptsc, there are duplicates
ptsc <- subset(ptsc, !duplicated(ptsc[c("LocID")])) #removes duplicates
## Examine to remove classes with very few instances (if desired)
classnumb <- summary(as.factor(as.character(ptsc$Class)), maxsum=200)
classnumb
ptsc$Class <- as.character(ptsc$Class)
# Steps to clean up classes (if needed) based on numer of observations in classes
classlessnum <- subset(classnumb, classnumb < 5) # Update minimum number of observations by class needed
classtodrop <- names(classlessnum)
ptsc <- ptsc[ ! ptsc$Class %in% classtodrop, ]
ptsc$Class <- as.factor(as.character(ptsc$Class))
## Class wts to help deal with class size differences in training
classnumb <- summary(as.factor(as.character(ptsc$Class)), maxsum=200)
classwts <- 1-classnumb/nrow(ptsc)
classwts <- as.vector(classwts) ## Vector that is used in random forest to help minority class performance in model by weighting them more in model


############### Build Random Forest
# Can play with randomforest parameters to minimize OOB error in the summary step: mtry, nodesize, and classwt seem most influential
soiclass <- randomForest(formulaStringRFE, data = ptsc, importance=TRUE, proximity=FALSE, ntree=200, keep.forest=TRUE, mtry=4,  nodesize=1,classwt=classwts)
soiclass # OOB Error summary for random forest
setwd(predfolder)
saveRDS(soiclass,paste(classname,"rf_clswts.rds",sep="_")) # Save model
varImpPlot(soiclass) # Plot showing most important variables in model
############### Create Confusion matrix (for categorical models)
## Need to strip last column e.g. confusion[1:9,1:10] in rf object would be confusion[1:9,1:9]
confmatrix <- soiclass$confusion[1:length(soiclass$classes),1:length(soiclass$classes)]#Must match number of classes 
OOBkappa <- Kappa.test(confmatrix, conf.level = 0.95)
OOBkappa # Summary of Kappa result
write.table(confmatrix, file = paste(classname,"conf_matr_oob.txt",sep="_"), sep = "\t", row.names = TRUE) ## need to add one space to first row in excel


############ Predict model onto covariate grid
setwd(predfolder)
## Parallelized predict
rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)
beginCluster(cpus,type='SOCK')
pred <- clusterR(rasters, predict, args=list(model=soiclass),progress="text")
## predprob makes a huge temp file on c drive (137+GB for COP), make sure there's room...
# Classwise probabilities for every pixel
predprob <- clusterR(rasters, predict, args=list(model=soiclass, type="prob", index = 1:length(soiclass$classes)),progress="text")
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
#probmaxint <- clusterR(probmax, calc, args=list(fun=intfn),progress='text') # not produced yet for 2018 field week
## Render integer version of probability raster stack for smaller file size
#predprobint <- clusterR(predprob, calc, args=list(fun=intfn),progress='text') # not produced yet for 2018 field week
endCluster()
## Now save rasters
writeRaster(pred, overwrite=TRUE,filename="PSC_DSM18_GRSM.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(predprobint, overwrite=TRUE,filename="PSC_DSM18_GRSM_5plusprobmatrix_sf.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
writeRaster(probmaxint, overwrite=TRUE,filename="PSC_DSM18_GRSM_5plus_probmax.tif", options=c("COMPRESS=DEFLATE", "TFW=YES"),datatype='INT1U', progress="text")
## Create lookup table to match with values in raster
lookup_tab <- as.data.frame(soiclass$classes)
lookup_tab$value <- as.numeric(lookup_tab$`soiclass$classes`)
lookup_tab$predicted <- as.character(lookup_tab$`soiclass$classes`)
lookup_tab <- lookup_tab[c("value","predicted")]
write.table(lookup_tab, file = "PSC_DSM18_GRSM_lookup.txt", sep = "\t", row.names = FALSE)


