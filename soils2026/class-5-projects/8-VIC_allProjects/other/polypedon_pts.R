# load and install package"s
required.packages <- c("raster", "sp", "clhs", "progressr", "doFuture", "future", "dplyr", "cluster", "sf", "stats", "plyr")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

#function

polyped <- function (covs, pts, buffer, fac = NA, metric = "gower", stand = FALSE, cores=NULL, ...)  {
  
  start.time <- Sys.time()
  
  doFuture::registerDoFuture()
  future::plan("multisession", workers = cores)
  
  handlers(global = TRUE)
  
  # Iterate over every point
  # add .progress='text' if not running in parallel
  res_l <- plyr::llply(1:nrow(pts), function(i) {
    
    coords <- pts[i, ]
    
    # 2. Extract all cells within 'buffer' m of the sampling points. 
    buff_data <- raster::extract(
      x = covs, 
      y = coords, 
      buffer = buffer, 
      cellnumbers = TRUE, 
      method = 'simple', 
      df = TRUE
    )
    
    # 3. Apply Gower's similarity index to each element of list of extracted raster values
    
    # Get the cell numbers from each sample point to identity the right column in the similarity matrix. 
    cellnum <- cellFromXY(covs, coords)
    
    # 3.b Calculate Gower's similarity index around each point. 
    #   I used Gower's because it can handle categorical covariates, 
    #   but there could be other options. 
    
    # Only retain cases without NA values
    buff_data <- data.frame(buff_data[complete.cases(buff_data), ], stringsAsFactors = TRUE)
    
    # If there are some factor data
    if (!any(is.na(fac))) {
      buff_data[, fac + 1] <- lapply(buff_data[fac + 1], factor)
    }
    
    # Calculate gowers similarity index 
    gower_dissim <- daisy(x = buff_data[, names(covs)], metric = metric, stand = stand, warnBin = FALSE)
    # turn dissimilarity object to matrix
    gower_dissim <- cbind(buff_data$cell, as.matrix(gower_dissim)) 
    
    # Select the row of similarity indices with cell number equal to the cell number of the 
    # sample point and convert dissimilarity to similarity by subtracting from 1.  
    gower_sim <- 1 - gower_dissim[gower_dissim[, 1] == cellnum, ] 
    
    # Combine the cellnumbers of the raster to the similarity index. 
    res_df <- data.frame(cells = buff_data$cell, similarity = gower_sim[-1], stringsAsFactors = TRUE)
    
    # filter for all that are >= a similarity threshold
    # rather than a threshold I believe it is better to calculate everything as it provides a quicker way to choose a threshold.
    #res_df2 <- res_df[res_df$similarity >= thresh & res_df$similarity < 1,]
    
    # Join, which will keep only the covariate values that meet the threshold criteria
    # uncomment the following if I want to integrate a threshold into the code
    #res_filt_buff <- dplyr::inner_join(res_df2, buff_data[,-1], by = 'cells')
    res_filt_buff <- dplyr::inner_join(res_df, buff_data[,-1], by = 'cells')
    
    # if there there are no similarity values, skip appending appending 
    # the results. I would rather do this in the buff_data step to 
    # avoid calculating the distance matrix, but putting it here 
    # makes the function work 
    if(nrow(res_filt_buff) > 0) {
      
      # Get the xy coordinates
      resxy <- xyFromCell(covs, res_filt_buff$cell)
      
      # Join the observed class name, the covariate values and the xy coordinates 
      resall <- data.frame(uid=res_filt_buff[, 1], pts@data[i,1:2], res_filt_buff[, -1], resxy, stringsAsFactors = TRUE, row.names = NULL)
      names(resall)[2] <- names(pts)[1]
      names(resall)[3] <- names(pts)[2]
      resall
      
    } else {                       
      
      cat(paste("Location", pts@data[i, 1], "had 0 cells >= the threshold", "\n"))
      
    }
    
  }, .progress = "progressr", .parallel = TRUE, ...)
  end.time <- Sys.time()
  print(end.time - start.time)
  
  res_s <- dplyr::bind_rows(res_l)
  res_s
  
}


#bring in data
# set working directory to spectral covariates
setwd("C:/8-VIC-class5Projects/cov100_clip")
getwd()

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack of spectral covs
rstack <- stack(rList)
names(rstack)
# bring in point data
setwd("C:/8-VIC-class5Projects")
getwd()

# change the name to match your shapefile name 
pts <- st_read("train_8020.shp") 

# check names of attributes in training.pts need one col of unique site id and one class col 
names(pts) 

#remove cols
pts <- pts[-c(3:162)]
names(pts)

pts <- as_Spatial(pts)

names(pts)

levels(as.factor(pts$Class))

#pts$Class <- as.factor(pts$Class)

## polypedons
pped <- polyped(covs = rstack, pts = pts, buffer = 500, fac = NA, metric = 'gower', cores = 11)


#convert df to sf
library(sf)
pped.sf <- st_as_sf(pped, coords = c("x","y"))

sf.crs <- st_crs(rstack)

pped.sf <- st_set_crs(pped.sf, sf.crs)

pped.spdf <- as(pped.sf, "Spatial")

plot(rstack$ADIUCL5)
points(pped.spdf)


summary(as.factor(pped.sf$Class))

# select >.90% similarity
pped.pts <- pped.sf[pped.sf$similarity >= .89,]

#levels(as.factor(pped.sf$Class))
#levels(as.factor(pped.pts$Class))
summary(as.factor(pped.pts$Class))
#summary(as.factor(pped.pts$Class))



# select 10 observations per factor level
pped.10 = lapply(split(pped.pts, pped.pts$Class), function(x) x[sample(nrow(x), 10),])
pped.10 = do.call(rbind, pped.10)
summary(as.factor(pped.10$Class))

# select 10 observations per factor level
pped.20 = lapply(split(pped.pts, pped.pts$Class), function(x) x[sample(nrow(x), 20),])
pped.20 = do.call(rbind, pped.20)
summary(as.factor(pped.20$Class))

# select 30 observations per factor level
pped.30 = lapply(split(pped.pts, pped.pts$Class), function(x) x[sample(nrow(x), 30),])
pped.30 = do.call(rbind, pped.30)
summary(as.factor(pped.30$Class))

# write shapefile 
st_write(pped.pts, dsn='C:/8-VIC-class5Projects', layer='poly8020_all', driver='ESRI Shapefile', delete_layer = TRUE) 
# write shapefile 
st_write(pped.10, dsn='C:/8-VIC-class5Projects', layer='poly8020_10', driver='ESRI Shapefile', delete_layer = TRUE)
# write shapefile 
st_write(pped.20, dsn='C:/8-VIC-class5Projects', layer='poly8020_20', driver='ESRI Shapefile', delete_layer = TRUE)
# write shapefile 
st_write(pped.30, dsn='C:/8-VIC-class5Projects', layer='poly8020_30', driver='ESRI Shapefile', delete_layer = TRUE)



# bring in point data
setwd("C:/8-VIC-class5Projects")
getwd()

# change the name to match your shapefile name 
pts73 <- st_read("poly7030_all.shp") 
pts82 <- st_read("poly8020_all.shp")

length(levels(as.factor(pts73$Class)))
length(levels(as.factor(pts82$Class)))



