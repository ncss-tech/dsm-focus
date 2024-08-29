library(terra)
library(rassta)
#library(sf)

# retrieve covariates
elev <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/genelev_2.tif")
gmrph30 <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/gmrph_r_30.tif")
gmrph300 <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/gmrph_r_300.tif")
slp <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/sl_4.tif")
swi <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/swi_10.tif")
tri <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/tri_4.tif")
twi <- rast("https://storage.googleapis.com/dw-sandbox-bucket/cov30/twi.tif")


# create stack
rStack <- c(elev, slp, swi, tri, twi)
#rStack <- c(gmrph300, swi, tri, slp)
# plot
plot(rStack)
names(rStack)

# scalr variables
rSs <- scale(rStack)

# self organizing map and gap statistic for optimum k
set.seed(234)
rSom <- som_gap(var.rast = rSs,
               xdim = 8,
               ydim = 8,
               #rlen = 150,
               K.max = 56
               )

# optimum k
rSom$Kopt

# PAM clustering of topographic SOMs codebook vectors
rPam <- som_pam(ref.rast = rStack[[1]],
                kohsom = rSom$SOM,
                k= rSom$Kopt)

plot(rPam$sompam.rast)

sompamRaster <- rPam$sompam.rast
str(sompamRaster)
library(viridis)
library(RColorBrewer)
cols <- brewer.pal(8, "Paired")
pal <- colorRampPalette(cols)
plot(sompamRaster$SOMPAM,
     axes=FALSE,
     type="classes",
     col=pal(8))
