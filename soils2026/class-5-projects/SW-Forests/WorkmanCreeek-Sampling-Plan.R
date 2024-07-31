library(terra)
library(sf)

# retrieve covariates
elev <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/genelev_2.tif")
gmrph30 <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/gmrph_r_30.tif")
gmrph300 <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/gmrph_r_300.tif")
slp <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/sl_4.tif")
swi <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/swi_10.tif")
tri <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/tri_4.tif")
twi <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/twi.tif")


# create stack
rStack <- c(elev, gmrph30, gmrph300, slp, swi, tri, twi)

# plot
plot(rStack)
names(rStack)

# fcsc function
fscs <- function(x, vars = NULL, n = NULL, center = TRUE, scale = TRUE, iter.max = 1000) {
  
  if (class(x) == "SpatRaster") {
    df <- terra::as.data.frame(x, xy = TRUE)
  }
  if (class(x) == "RasterStack") {
    df <- as.data.frame(x, xy = TRUE)
  }
  rn <- row.names(df)
  row.names(df) <- NULL
  
  idx <- df[vars] |>
    complete.cases()
  df2 <- df[idx, vars] |>
    scale(center = center, scale = scale)
  
  cl <- df2 |> 
    kmeans(center = n, iter.max = iter.max)
  
  df2 <- cbind(df2, cl = cl$cluster)
  df$cluster <- NA
  df[row.names(df2), "cluster"] <- cl$cluster
  
  n_clus <- cl$centers |> nrow()
  
  d <- fields::rdist(cl$centers, df2[, vars])
  idx2 <- apply(d, 1, which.min)
  idx3 <- row.names(df2)[idx2]
  cl_cen <- df[idx3, ]
  # cl_cen
  
  # idx <- list()
  # for (i in 1:n_clus) {
  #   z <- rbind(cl$centers[i, ], df2[, vars])
  #   d <- dist(z)
  #   idx[[i]] <- which.min(as.matrix(d)[1, -1])
  # }
  # idx <- unlist(idx)
  # 
  # cl_cen <- df2[idx, ]
  # cl_cen
  
  return(cl_cen)
}

pts <- fscs(rStack, vars = c("gmrph_r_300", "swi_10"), n=50) 


# convert to spatial object
pts.sf <- st_as_sf(pts, coords = c("x","y"), crs = crs(rStack))

plot(swi)
points(pts.sf)

setwd("SETYOURWORKINGDIRECTORY")

st_write(pts.sf, "wcreekFSCSpts.shp")