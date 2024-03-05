# Script for Data Exploration of veg classes/potential ESD's for the Florida Field week  in  Big Cypress National Preserve. 
# Load point data
# Set the working directory
setwd("D:/everglades/Martin")

# Load point data
pts <- readRDS("./points.rds")

# Correlation plots
library(car)
library(psych)
library(doParallel)
library(caret)
library(DMwR)
library(ROSE)

# These were the variables that were initally selected by recursive feature eliminationby Martin Figueroa, soil scientist in the MLRA 7FOR, in Ft. Meyer's Florida. 

# Change order of classes
pts$VegCLName <- factor(pts$VegCLName, levels = c("hammock", "prairie", "SB", "cypress", "isoswamp"))


# Create a vector of covariate (column) names. 
vars <- c("dev_from_mean_elev", "diff_from_mean_elev", "depth_in_sink", "dem", "nd7t5wet", "nd5t1wet", "Max_elev_dev", "ndviwet", "nd3t2dry") 

# Convert to dataframe (just makes things easier). 
df1 <- as.data.frame(pts[,which(names(pts) %in% vars)])
str(df1)

 

# Make a scatterplotMatrix. 
pairs.panels(df1[,-c(10,11)], # remove coordinates
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = FALSE)  # show density plots


# A simple way to print column number and column name which helps with indexing
cbind(names(pts))
  
# The above scatterPlotMatrix showed that dev_from_mean_elev and diff_from_mean_elev were highly correlated (https://jblindsay.github.io/wbt_book/available_tools/geomorphometric_analysis.html). After thinking about this, I think that depth_in_sink is maybe a better variable as it better matches the depth (and potentially length) of ponding.  

table(pts$VegCLName)

# diff_from_mean_elev seems to really pick out isoswamp and hammock
boxplot(diff_from_mean_elev~VegCLName,
        data=pts,
        main="diff_from_mean_elev",
        xlab="Class",
        col="orange",
        border="brown")

# This is very similar to diff_from_mean_elev
boxplot(dev_from_mean_elev~VegCLName,
        data=pts,
        main="dev_from_mean_elev",
        xlab="Class",
        col="orange",
        border="brown")

# Although Max_elev_dev is still highly correlated with depth_in_sink, it also shows two distinct clusters of points (on the scatterplot matrix), which this plot shows are probably isoswamps. I think it worth including.  
boxplot(Max_elev_dev~VegCLName,
        data=pts,
        main="Max_elev_dev",
        xlab="Class",
        col="orange",
        border="brown")

boxplot(depth_in_sink~VegCLName,
        data=pts,
        main="depth_in_sink",
        xlab="Class",
        col="orange",
        border="brown") 
# depth_in_sink seems to do a better job at highlighting the isoswamps and the hammocks. Cypress, parrie, and SB are more confused. Conceptually I think that depth_in_sink may be a better covariate than the other _from_mean_elev (that are highly correlated with depth_in_sink) because it matches the conceptual diagram of soil-landscape relationships and because it is easier to explain. 
# This plot also shows that there are some hammocks that have depth_in_sink that are non-zero. Maybe these are outliers and it would be good to reivew these manually, which could be done with the following code: which(pts$depth_in_sink > 0.1) # These observations would be worth revisiting in arcgis to see if they are mis-labeled or are actual valid observations. 


# The spectral variables are also highly correlated with each other. These covariates really seem to separate prarie and isoswamps. nd5t1wet seems to have less variability within each class and highlits isoswamp and prarie, nd5t1wet seems to have lower values of praries (makes sense) and slightly higher values for isoswamp. 
# nd5t1wet and nd3t2dry generally have the narrowest ranges between classes, and seem to be less correlated (meaning maybe max info). Interestingly, the prarie is the most variable, which makes sense.  

boxplot(ndviwet~VegCLName,
        data=pts,
        main="ndviwet",
        xlab="Class",
        col="orange",
        border="brown")

# Seems to highlight prarie and SB well.
boxplot(nd5t1wet~VegCLName,
        data=pts,
        main="nd5t1wet",
        xlab="Class",
        col="orange",
        border="brown")

boxplot(nd7t5wet~VegCLName,
        data=pts,
        main="nd7t5wet",
        xlab="Class",
        col="orange",
        border="brown")

# Seems to highlight prarie and SB well. 
boxplot(nd3t2dry~VegCLName,
        data=pts,
        main="nd3t2dry",
        xlab="Class",
        col="orange",
        border="brown")

# What happens if we only keep the variables that I think are most relevant from the above plots/thoughts. 

pts2 <- pts[,names(pts) %in% c("VegCLName", "nd5t1wet", "nd3t2dry", "depth_in_sink", "dem", "Max_elev_dev")]

# Fit a random forest model using only the relevant variables. 
# Initialize training control
fitControl <- trainControl(method = "cv", 
                           savePredictions = T,
                           classProbs = TRUE,
                           returnResamp = 'final',
                           allowParallel = TRUE,
                           selectionFunction='oneSE',
                           summaryFunction = multiClassSummary)

# Set a tune grid for manual control of tuning parameters
tunegrid <- expand.grid(mtry=c(1:5))

#Register parallel processing
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
set.seed(4801)
 model1 = train(VegCLName ~ ., 
                data = pts2@data, 
                method="rf", 
                trControl = fitControl, 
                tuneGrid=tunegrid)
stopCluster(cl)

# Print results
model1

# Review confusion matrix
# This suggests that cypress still has high class error, which is to be expected since there were none of the covariates that really seperated cypress. This also reveals that cypress is often confused with prairie. 
model1$finalModel$confusion


# What happens if we use RFE again, but exclude the variables from the inital rfe selection that I didn't think were entirely relevant? 
# The ! means 'negate' so this removes these variables
pts3 <- pts[,!(names(pts) %in% c("dev_from_mean_elev", "diff_from_mean_elev", "ndviwet", "nd7t5wet"))]

# Define subsets that equal the number of covariates
subsets <- c(1:96)

# 10-fold repeated cross validation, repeated 10 times
ctrl <- rfeControl(functions = rfFuncs,
                    method = "repeatedcv",
                    number = 10,
                    repeats = 10,
                    verbose = FALSE)

# Set up parallel processing parameters 
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
set.seed(4801)

rfeMod1 <- rfe(VegCLName ~ ., 
                 data = pts3[,-1], # this removes the id column
                 sizes = subsets,
                 rfeControl = ctrl)
stopCluster(cl)

save.image("./model2.7.2020.Rdata")
# Review model fit. 
# 90 variables chosen as optimal, but the one standard error rule suggests that 8 variables might be optimal. 
rfeMod1

# The plots show a similar pattern as before.
plot(rfeMod1)


# Class error. 
# Hmmm, cypress and SB class error drops compared to the single random forest (suggesting that there is a covariate, or set of covariates, that helps distinquish cypress forests and SB), but the error for all of the other classes increases. This would suggest to me that we need more training observations rather than a different covariate.   
rfeMod1$fit

# I would also investigate the following variables for modeling. 
# Id the top 8 predictors
top8 <- c('VegCLName', predictors(rfeMod1)[1:8])

# The plot indicates that the error really starts to decrease somewhere around 18, so also id the top 18 predictors
top20 <- c('VegCLName', predictors(rfeMod1)[1:20])



### Test the hypothesis that more observations will increase model performance. This uses the SMOTE algorithm.
# Create this
pts4 <- pts3@data[complete.cases(pts2@data),-1]

fitControl.s <- trainControl(method = "cv", 
                             savePredictions = T,
                             classProbs = TRUE,
                             returnResamp = 'final',
                             allowParallel = TRUE,
                             selectionFunction='oneSE',
                             summaryFunction = multiClassSummary, 
                             sampling = "smote")

cl <- makePSOCKcluster(2)
registerDoParallel(cl)
set.seed(4801)
smoted <- train(VegCLName ~ ., 
                data = pts4[,names(pts4) %in% top8], 
                method="rf", 
                trControl = fitControl, 
                metric = 'Accuracy')
stopCluster(cl)

# Look at model fit
smoted


#Compare confusion matricies between models
model1$finalModel$confusion
rfeMod1$fit$confusion
smoted$finalModel$confusion



# Predict training data to see exaclty which observations are mis-classified. 
# This is a half-baked idea, but it seems like there maybe some good information about which points the model can't predict. Does this suggest these should be double checked to see if these are correctly classified? 
 repredict <- predict(smoted, pts4)
 maybe_good <-  cbind(pts4$VegCLName, repredict)
 double_check <- pts3[which(maybe_good[,1] != maybe_good[,2]),] # This shows that row 182 is not predicted well. What is it about this observation? 

 
 
 
 
 