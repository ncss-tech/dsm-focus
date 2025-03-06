# clustering with soil climate 2/4

library(ClustGeo)
library(terra)
library(sf)
library(cluster)

# hierarchical clustering with spatial constraints to develop modeling map units


# load the training data
train.sf <- st_read("D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/data/nasis-export/trainData2525.shp")

#train.data <- na.omit(train.sf)

# create covariate data table
train.df <- st_drop_geometry(train.sf)

#View(train.df)
#train.df <- train.df[train.df$evi != -9999,]
# need to split the out the groups for cluster analysis
names(train.df)


# cluster the following classes


# check col names to come up with subset
names(train.df)

#properties
names(train.df)[c(3:13,18:81)]
#covs
names(train.df)[c(14:17)]

#  col 6-16 property data  17-22 cov data
prop <- train.df[c(3:13,18:81)]
cov <- train.df[c(14:17)]




# cluster data 
# note D0 is feature space - cov.dat
#      D1 is contraint space - prop.dat

#----
# mesic aridic clustering 35 classes

prop.dat <- prop
cov.dat <- cov

#there is an observation with odd values 9999 etc remove from cov.dat

names(cov.dat)#[c(1,3)] # relative slope position and feoxide

D0 <- daisy((cov.dat), stand = T, metric = "euclidean")
D0 <- dist(D0)

dend <- hclust(D0, method = "ward.D2")
plot(dend, hang = -1, label = F)

rect.hclust(dend ,k = 4, border = c(4,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

# convert all colums to factor
names(prop.dat)
summary(prop.dat)


character.cols <- sapply(prop.dat, is.character)
prop.dat[character.cols] <- lapply(prop.dat[character.cols], as.factor)
str(prop.dat)
names(prop.dat)
summary(prop.dat)
integer.cols <- sapply(prop.dat, is.integer)
prop.dat[integer.cols] <- lapply(prop.dat[integer.cols], as.factor)
str(prop.dat)
names(prop.dat)
summary(prop.dat)


names(prop.dat)#[-c(3,4)]


D1 <- daisy(prop.dat, stand = T, metric = "gower")
D1 <- dist(D1)

dend <- hclust(D1, method = "ward.D2")#, members = siteiid)

plot(dend, hang = 0, label = F)
rect.hclust(dend ,k = 10, border = c(10,9,8,7,6,5,4,3,2,1))
legend("topright", legend = paste("cluster",1:10), 
       fill=1:10,bty= "n", border = "white")


range.alpha <- seq(0,1,0.05)

K <- 35

cr <- choicealpha(D0, D1, range.alpha, K, graph = T)

cr$Q



# modified partititon  with alpha = 0.1
tree <- hclustgeo(D0,D1, alpha=0.4)

#tree$labels
newK = 35
plot(tree, label = F, hang = -1)
rect.hclust(tree, k=newK, border = c(1:newK))


part <- cutree(tree, k=newK)
summary(as.factor(part))

train.df$clust <- part

# merge with train.sf
clust.sf <- merge(train.sf, train.df)

names(clust.sf)

st_write(clust.sf, "D:/soils_2026/class-5-projects/SW2026USFSlands/s2026-class5-sw-forests/data/nasis-export/cluster2525.shp", delete_layer = T)
