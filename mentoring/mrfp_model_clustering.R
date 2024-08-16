

library(aqp)
library(soilDB)
library(dplyr)
library(sf)
library(terra)



# NASIS project ----
# projectname = "MLRA 107B - Missouri River Floodplain Remap Project (Pilot Study-Phase 1)?
# projectname = "MLRA 107 - Missouri River Floodplain Remap Project (North Segment)"
# usiteid LIKE "2023IA085%"

# RIC vars ----
criteria <- c("draingcl", "taxpartsize", "well/oxyaquic/aquic sg/aeric sg/aquic so", "surface texture/thickness", "color", "mollic", "fluvent", "vertic", "pondfreqcl", "flodfreqcl")



# geodata ----
fp_geo <- "D:/geodata/project_data/mrfp"

aoi_mrfp <- read_sf(fp_geo, layer = "AOI_Entire") |>
  st_transform(crs = 5070)


rs_vars <- c(
  "Clusters_5.sdat",
  "dem_diff.sdat",
  "dem2020_10m_nosinks_saga-modca.sdat",
  "dem2020_10m_nosinks_tpi.sdat",
  "dem2020_10m_nosinks_so1plus_vdcn.sdat",
  "dem2020_10m_nosinks_so3plus_vdcn.sdat",
  "dem2020_10m_nosinks_so6plus_vdcn.sdat",
  "dem2020_10m_nosinks_acost-vcdn-so6plus-slope-pct.sdat",
  "US_6Band_ChangeFree_mosaic_clip-2-000000.sdat",
  "US_6Band_ChangeFree_mosaic_clip-3-000000.sdat",
  "US_6Band_ChangeFree_mosaic_clip-4-000000.sdat",
  "US_6Band_ChangeFree_mosaic_clip-5-000000.sdat"
)
rs <- rast(file.path(fp_geo, rs_vars))
names(rs) <- c(
  "clusters", "dem_diff",
  "saga_ca", "tpi", "so1_vdcn", "so3_vdcn", "so6_vdcn", "acost", "b2", "b3", "b4", "b5")


# soil obs ----
fp <- fetchNASIS(from = "pedon_report")

fp_sf <- fp |>
  site() |> 
  subset(complete.cases(x_std, y_std)) |>
  st_as_sf(
    coords = c("x_std", "y_std"),
    crs = 4326
  ) |>
  st_transform(crs = 5070)

fp_ex <- extract(rs, fp_sf) |>
  subset(!is.na(clusters)) |>
  within({clusters = as.factor(clusters)})

idx_fp <- which(site(fp)$peiid %in% fp_sf[fp_ex$ID, ]$peiid)
fp_aoi <- fp[idx_fp]
write_sf(fp_sf, "nasis-pedons_2023IA085.shp")


# transform ----

# apply weights to unrepresentative points
ur_peiid <- c(1431940, 1431941, 1431938)
idx <- ! site(fp_aoi)$peiid %in% ur_peiid
site(fp_aoi)$weights <- ifelse(idx, 1, 0.33)


# fix horizon error
h2 <- horizons(fp_aoi)
h2$hzdept[h2$peiid == "1431954" & h2$hzdept == 19] <- 17
horizons(fp_aoi) <- h2

s <- site(fp_aoi) |>
  cbind(fp_ex)
h <- horizons(fp_aoi)



## impute missing ssc and texcl ----
data(soiltexture)
h <- cbind(
  h, 
  texcl_to_ssc(h$texcl, h$claytotest)
)
h <- within(h, {
  clay2 = ifelse(
    !is.na(claytotest) & claytotest %in% soiltexture$values$clay[soiltexture$values$texcl %in% texcl], 
    claytotest, 
    clay
  )
  sand2 = ifelse(
    !is.na(sandtotest) & claytotest %in% soiltexture$values$clay[soiltexture$values$texcl %in% texcl],
    sandtotest, 
    sand
  )
})

idx <- with(h, complete.cases(texcl, clay, sand))
h$taxpartsize[idx] <- texture_to_taxpartsize(
  texcl = h$texcl[idx], 
  clay  = h$clay2[idx], 
  sand  = h$sand2[idx], 
  fragvoltot = rep(0, length(h$sand2[idx]))
)



## calculate contrasting pscs ----
h_t <- h |>
  hz_segment(
    intervals = c(25, 100), 
    depthcols = c("hzdept", "hzdepb"), 
    trim = TRUE) |>
  subset(select = c(peiid, hzdept, hzdepb))

psc_lu <- lookup_PSCS()

test2 <- hz_to_taxpartsize(h, h_t, idcol = "peiid", depthcols = c("hzdept", "hzdepb"), clay = "clay2", taxpartsize = "taxpartsize") |>
  within({
    taxpartsize = factor(taxpartsize, levels = levels(lookup_PSCS()$taxpartsize), ordered = TRUE)
    taxpartsize_rank = psc_lu$rank[taxpartsize]
    taxpartsize = droplevels(taxpartsize)
  })

test2$taxpartsize |> table()
merge(site(fp_aoi)[c("peiid", "taxpartsize")], test2, by = "peiid") |> 
  View()



## calculate sand thickness ----
h_sand <- h |>
  transform(sand = texcl %in% c("s", "fs", "vfs", "ls", "lfs", "lvfs")) |> #, "fsl", "vfsl")) |>
  hz_dissolve(by = "sand", idcol = "peiid", depthcols = c("hzdept", "hzdepb")) |>
  subset(hzdept == 0 & variable == "sand" & value == TRUE) |>
  merge(s[c("peiid")], by = "peiid", all.y = TRUE) |>
  within({
    sand_thk = hzdepb
    sand_thk = ifelse(is.na(sand_thk), 0, sand_thk)
  })



## calculate taxonomic soil moisture ----
s2 <- within(s, {
  drainage = NA
  drainage[grepl("aeric",    taxsubgrp)] = "aeric"
  drainage[grepl("aqu",      taxsubgrp)] = "aquic"
  drainage[grepl("oxyaquic", taxsubgrp)] = "oxyaquic"
  drainage[is.na(drainage)] = "udic"
  drainage = factor(drainage, levels = c("aquic", "oxyaquic", "aeric", "udic"), ordered = TRUE)
  
  drainage_rank = c(25, 62, 75, 125)[as.integer(drainage)]
  
  fluv   = ifelse(grepl("fluv", taxsubgrp), TRUE, FALSE)
  mollic = ifelse(grepl("mollic|olls", taxsubgrp), TRUE, FALSE)
})



## combine results ----
vars <- c("peiid", "drainage", "drainage_rank", "fluv", "mollic")
s2 <- merge(s2[vars], test2, by = "peiid", all.x = TRUE, sort = FALSE) |>
  merge(h_sand[c("peiid", "sand_thk")], by = "peiid", all.x = TRUE, sort = FALSE) |>
  merge(s[c("peiid", "taxsubgrp", names(rs))], by = "peiid", all.y = TRUE, sort = FALSE)


so <- s2
row.names(so) <- so$peiid

vars <- c("drainage", "drainage_rank", "taxpartsize", "taxpartsize_rank", "sand_thk", "mollic", "fluv")
rs_vars <- names(rs)
so <- so[c(vars, rs_vars)]
so <- so[complete.cases(so), ]


h <- within(h, {
  stratified = grepl("SR-", texture)
  FS         = grepl("FS", texture)
  effclass2   = factor(effclass, levels = c("none", "very slight", "slight", "strong", "violent"), ordered = TRUE)
  effclass[is.na(effclass)] = "none"
})
h <- merge(h, s2["peiid"], by = "peiid", all.y = TRUE, sort = FALSE)
horizons(fp_aoi) <- h[c("phiid", "clay2", "sand2", "FS", "effclass2", "stratified")]



# EDA ----

library(ggplot2)

table(so$taxpartsize, so$drainage) |> addmargins()
table(
  so$taxpartsize, 
  cut(so$sand_thk, breaks = c(0, 25, 50, 100, 205), include.lowest = TRUE)
) |> 
  addmargins()

ggplot(so, aes(dem_diff, sand_thk, col = sand_thk > 100)) +
  geom_point() +
  geom_smooth() +
  ylim(200, -25) +
  xlab("DEM Difference") +
  ggtitle("Sand Thickness vs Elevation Change")



# Correlate ----
psc_lu <- lookup_PSCS()

so <- so |>
  within({
    cor_psc = as.character(taxpartsize)
    cor_psc = ifelse(
      cor_psc == "fine-loamy over sandy or sandy-skeletal", 
      "fine-silty over sandy or sandy-skeletal",
      cor_psc
    )
    cor_psc = ifelse(
      cor_psc == "clayey over sandy or sandy-skeletal", 
      "sandy over clayey",
      cor_psc
    )
    cor_psc = ifelse(
      cor_psc == "fine", 
      "clayey over loamy",
      cor_psc
    )
    cor_psc = ifelse(
      cor_psc == "coarse-loamy over clayey", 
      "fine-silty over sandy or sandy-skeletal",
      cor_psc
    )
    cor_psc = factor(cor_psc, levels = levels(psc_lu$taxpartsize), ordered = TRUE)
    cor_psc_rank = psc_lu$rank[as.integer(cor_psc)]
    cor_psc = droplevels(cor_psc)
  })
table(so$cor_psc, so$drainage)



# Cluster ----
library(ClustGeo)

vars <- c("drainage_rank", "cor_psc_rank", "sand_thk", "mollic", "fluv") #
so_dm <- cluster::daisy(
  so[vars], 
  metric = "gower", 
  stand = TRUE,
  weights = c(2, 2, 1, 1, 0.5)
)

pred_dm <- cluster::daisy(so[rs_vars], metric = "gower", stand = TRUE)
D0_aqp <- aqp::NCSP(fp_aoi, vars = c("clay2", "sand2", "drainage", "mollic", "FS", "stratified", "effclass2"))

D0 <- as.dist(as.matrix(so_dm))
D1 <- as.dist(as.matrix(pred_dm))
D0_aqp <- as.dist(as.matrix(D0_aqp))

a <- lapply(2:10, function(i) choicealpha(D0, D1, range.alpha = seq(0, 1, 0.1), K = i))

idx_wt <- match(
  names(D0),
  site(fp_aoi)$peiid
)
all(site(fp_aoi)$peiid[idx_wt] == names(D0))
wt <- site(fp_aoi)$weights[idx_wt]
wt <- wt[complete.cases(wt)]
test <- hclustgeo(D0, D1, alpha = 0.6, wt = wt/length(wt))

clus <- lapply(2:10, function(i) data.frame(as.factor(cutree(test, i))))
clus <- do.call("cbind", clus)
names(clus) <- paste0("n", 2:10)
all(names(so_dm) ==  so$peiid[-50])
so2 <- cbind(so[-53], clus)

clus_lab <- paste0("n", 2:10)



# reverse engineer clusters ----
## compute cluster centers ----
vars <- c("drainage", "taxpartsize", "sand_thk", "mollic", "fluv")
clus_median <- lapply(clus_lab, function(x) {
  
  dat <- so2[vars]
  dat[1:length(vars)] <- lapply(dat, function(x) if (!inherits(x, "numeric")) x <- as.integer(x) else x )
  
  center <- aggregate(dat, by = list(so2[[x]]), median, na.rm = TRUE)
  
  idx <- 1:length(vars)
  center[idx + 1] <- lapply(idx, function(i) {
    
    if (inherits(so2[, vars[i]],  "factor")) {
      vals <- levels(so2[vars][[i]])[center[[i + 1]]]
    }
    if (inherits(so2[, vars[i]],  "numeric")) {
      vals <- center[[i + 1]]
    }
    if (inherits(so2[, vars[i]],  "logical")) {
      vals <- as.logical(round(center[[i + 1]]))
    }
    
    return(vals)
  })
  
  center$lab <- apply(center[-1], 1, function(x) paste(x, collapse = "-"))
  
  return(center)
})
names(clus_median) <- clus_lab
clus_median


# medoids
vars <- c("drainage", "taxpartsize", "sand_thk", "mollic", "fluv")
vars <- c("drainage_rank", "cor_psc_rank", "sand_thk", "mollic", "fluv")

idx_ur <- which(
  rownames(so2) %in% ur_peiid
)[c(1, 3)]

idx_ur <- match(
  rownames(so2) |> gsub(" ", "", x = _), ur_peiid
) #[c(1, 3)]

so2 <- so2[-idx_ur, ]
clus_medoids <- lapply(clus_lab, function(x) {
  cluster::medoids(so2[vars], so2[[x]] |> as.integer(), stand = TRUE)
})
names(clus_medoids) <- clus_lab


so3 <- so2
so3$rn <- 1:nrow(so3)
lapply(clus_lab, function(x) {
  peiid <- row.names(so2)[clus_medoids[[x]]]
  idx   <- match(peiid, s$peiid)
  cm <- cbind(
    so2[[x]][clus_medoids[[x]]], 
    s[idx, c("peiid", "taxonname")]
    )
  names(cm) <- c(x, paste0(x, "_", names(cm)[2:3]))
  
  so3 <<- merge(so3, cm, by = x, all.x = TRUE, sort = FALSE)

  return(NULL)
  })
so3 <- so3[order(so3$rn), ]
so3$rn <- NULL
so3 <- so3[, c(names(so2), names(so3)[31:48])]
so3 <- cbind(
  peiid = rownames(so2),
  so3
)
so3 <- merge(s[c("peiid", "upedonid", "taxonname")], so3, by = "peiid", all.x = TRUE, sort = FALSE)

write.csv(so3, "mrfp_classes_v2.csv", row.names = FALSE)


# Model ----
library(ranger)

rf_test <- lapply(clus_lab, function(x) {
  var <- x
  rs_vars <- names(rs)
  
  data <- so2[, c(var, rs_vars)]
  names(data)[1] <- "test"
  
  ranger(
    test ~ ., 
    data = data[
      , 
      # the model does better with dem_diff, but not much worse, its unclear whether this variable will be available for he whole study area
      ! names(data) %in% c("dem_diff")
    ], 
    importance = "permutation"
  )
})
names(rf_test) <- clus_lab

sapply(rf_test, function(x) 1 - x$prediction.error) |> round(2) #|> summary()

so2$pred2 <- rf_test[[6]]$predictions
tb <- table(obs = so2$n7, pred = so2$pred2)
tb |> caret::confusionMatrix()


table(so2$cor_psc, so2$n6) |> addmargins()
table(so2$taxpartsize, so2$n6) |> addmargins()
table(
  cut(so2$sand_thk, breaks = c(0, 25, 50, 100, 205), include.lowest = TRUE), 
  so2$n6, 
  useNA = "always"
) |> addmargins()



# Predict ----
rs$clusters <- as.factor(rs$clusters)
rs2 <- rs[[! names(rs) %in% c("dem_diff")]]
predfun <- function(model, ...) predict(model, ...)$predictions
predict(
  rs2, 
  rf_test[[6]], 
  fun = predfun, 
  index = 1, 
  progress = "text", 
  overwrite = TRUE, 
  filename = "C:/workspace2/dsm_n7_14_v2.tif", 
  na.rm = TRUE
)


