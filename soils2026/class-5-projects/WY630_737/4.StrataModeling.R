## Strata modeling for Soils2026 class 5 projects WY630 and WY737
#Landform, Moisture Regime, and Temperature Regime
# Jessica Philippe
# 3/18/2024

# load and install package"s
required.packages <- c( "caret", "sf", "terra", "randomForest", "doParallel", "aqp", "parallel", "snow")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

#set working directory to spectral covs
setwd("~/wy/cov30s_WY630_737/clip")
getwd()

# read in raster file names as a list
sList=list.files(getwd(), pattern="tif$", full.names = FALSE)
sList

# stack
specStack <- rast(sList)

# repeat process to bring in terrain covs
set("~/wy/cov30t_WY630_737")

# read in raster file names as a list
tList=list.files(getwd(), pattern="tif$", full.names = FALSE)
tList

#stack
terStack <- rast(tList)

#check extents
ext(terStack)
ncol(terStack)
nrow(terStack)
ext(specStack)
ncol(specStack)
nrow(specStack)

#resample if needed
#specStack <- resample(specStack, terStack, method = 'bilinear', threads=T)

rStack <- c(specStack, terStack)

names(rStack)

#write stack to save
setwd("~/wy/cov30")
writeRaster(rStack, "rStack.tif", filetype="GTiff", overwrite = T)

#remove special characters from names
remove_txt <- gsub("_", "", names(rStack))

names(rStack) <- remove_txt

names(rStack)

remove_txt <- gsub("-", "", names(rStack))

names(rStack) <- remove_txt

names(rStack)

#Bring in training points
setwd("~/wy/data")
getwd()

#bring in shapefile 
pts <- st_read("WY630_737_pedons_lru.shp")

#check names of attributes in pts. Only need strata column.
names(pts)

#remove columns that are unwanted
#keep strata columns column
pts <- pts[-c(1:2)]
names(pts)

#extract raster covariate values at each point from raster stack created above and create a  SpatVector

pts.extract <- terra::extract(rStack, pts, xy=TRUE, bind=TRUE, na.rm=TRUE)

saveRDS(pts.extract, "ptsExtract.RDS")

#pts.extract <- readRDS("ptsExtract.RDS")

#Convert spatVector to a dataframe
names(pts.extract)

pts.df <- as.data.frame(pts.extract)
names(pts.df)

#create new data frames for each model
mst.df <- pts.df[-c(2,3,258,259)] #remove the other strata and any extra columns
names(mst.df)
names(mst.df)[1] <- "Class"   #rename strata to predict to "Class"

temp.df <- pts.df[-c(1,3,258,259)]
names(temp.df)
names(temp.df)[1] <- "Class"

lndfrm.df <- pts.df[-c(1,4,258,259)]
names(lndfrm.df)
names(lndfrm.df)[1] <- "Class"

#remove and observations with NAs
comp.mst <- mst.df[complete.cases(mst.df),]
comp.temp <- temp.df[complete.cases(temp.df),]
comp.lndfrm <-lndfrm.df[complete.cases(lndfrm.df)]

#convert class column to a factor 
#######also convert geomorphon column(s) to factor if using

comp.mst$Class <- as.factor(comp.mst$Class)
levels(comp.mst$Class)
summary(comp.mst$Class)

comp.temp$Class <- as.factor(comp.temp$Class)
levels(comp.temp$Class)
summary(comp.temp$Class)

comp.lndfrm$Class <- as.factor(comp.lndfrm$Class)
levels(comp.lndfrm$Class)
summary(comp.lndfrm$Class)

###----------------moisture regime-----------------------------###
# Recursive feature selection for covariate reduction
# Run in parallel in the make cluster function

#change the subsets to match the number of covariates
length(comp.mst) #is number of covariates plus the class column
subsets <- c(1:(length(comp.mst)-1))
#subsets <-c(1,10:50, 75, 100, 130, 161)

#Set seed to get reproducible results when running in parallel
set.seed(615)
seeds<- vector(mode = "list", length=112)
for(i in 1:111) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[112]] <- sample.int(1000, 1)

# set up the rfe control
ctrl.RFE <- rfeControl(functions = rfFuncs,
                       method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       seeds = seeds, 
                       verbose = FALSE)

## highlight and run everything from c1 to stopCluster(c1) to run RFE in parallel
c1 <- makeCluster(detectCores()-1)
registerDoParallel(c1)
set.seed(615)
rf.RFE <- rfe(x = comp[,-1],
              y = comp$Class,
              sizes = subsets,
              rfeControl = ctrl.RFE,
              allowParallel = TRUE
)
stopCluster(c1)             

gc()

# Look at the results
ref.RFE

rf.RFE$fit

summary(comp$Class)

rf.RFE$fit$confusion

rf.RFE$results

plot(rf.RFE) # default plot is for Accuracy but it can be changed to Kappa
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
predictors(rf.RFE)[1:50] #change the top number

#assign this to a variable
a<- predictors(rf.RFE) #[1:50]

#subset the covariate stack and teh data frame by the selected covariates
#the variable a is your selected covariates

#subset the raster stack to the selected covariates
r.stack.model <- subset(rStack, a)
names(r.stack.model)

#subset the data frame points with the number of covariates selected
comp.sub <- (comp[,c("Class",a)])

names(comp.sub)
levels(comp.sub$Class)
names(comp.sub)

###Modeling classes###

#change the subsets to match the number of covariates being used
levels(comp.sub$Class)
names(comp.sub)

#determine number of covariates
length(a)

subsets <- 161 #change to match number of covariates
#subsets <- c(1,10,25,50,100,161)

#set seed to get reproducible results when running in parallel
set.seed(615)
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

#make.names(RFE.train.all$Class)
# Random Forest - Parallel process, highlight and run from c1 to stopCluster(c1)
c1 <- makeCluster(detectCores()-12)
registerDoParallel(c1)
set.seed(48)
rfFit = train(Class ~ ., data = comp.sub,
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
