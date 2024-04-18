# load and install packages
required.packages <- c( "caret", "sf", "terra", "aqp", "stringr")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

# Bring in training data points 
# read in shapefile 
# set working directory to points
setwd("C:/8-VIC-class5Projects")
getwd()
# change the name to match your shapefile name 
pts <- st_read("All_pedons_prj.shp") 

# check names of attributes
names(pts)

#remove spatial duplicates
pts <- unique(pts)

###############
# climate correlation

#create new climate cols
pts$mst <- as.factor(pts$txmstcl)
pts$temp <- as.factor(pts$txtmprg)

names(pts)
summary(pts$mst)
summary(pts$temp)

#pts <- subset(pts, landform!="valley")
#pts$landform <- droplevels(pts$landform)
#levels(pts$landform)
#summary(pts$landform)

rep_st <- c('cryic'='frigid')
pts$temp <- as.factor(str_replace_all(pts$temp, rep_st))
levels(pts$temp)

pts <- subset(pts, mst!= 'aquic')
pts <- subset(pts, mst!= 'udic')
pts <- subset(pts, mst!= 'xeric')


rep_st <- c('aridic \\(torric\\)' = 'aridic')
pts$mst <- as.factor(str_replace_all(pts$mst, rep_st))
levels(pts$mst)
summary(pts$mst)


# landform correlation
#create a new landform colum from the lndfrm_ col

pts$landform <- as.factor(pts$lndfrm_)
names(pts)
summary(pts$landform)

levels(pts$landform)


# change aa lava flow to lava flow
rep_st <- c('aa lava flow'='lava flow',
            '^flow'= 'lava flow')
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change alluvial fan & to alluvial fan
rep_st <- c('alluvial fan & coppice mound'='alluvial fan', 
            'alluvial fan & fan apron'='alluvial fan', 
            'alluvial fan & fan piedmont'='alluvial fan', 
            'alluvial fan & fan remnant'='alluvial fan', 
            'alluvial fan & sand sheet'='alluvial fan')
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change alluvial flat & to alluvial flat
rep_st <- c('alluvial flat & lake plain'='alluvial flat')
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change bar & to just bar
rep_st <- c('bar & drainageway'='bar',
            'bar & fan piedmont'='bar',
            'bar & wash & terrace'='bar')
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change basin floor remnant to basin floor
rep_st <- c('basin-floor remnant'='basin floor')
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change drainageway & to drainageway
rep_st <- c("drainageway & alluvial fan"="drainageway",
            "drainageway & fan apron"="drainageway",
            "drainageway & fan piedmont"="drainageway",
            "drainageway & fan remnant"="drainageway",
            "drainageway & inset fan"="drainageway",
            "drainageway & terrace"="drainageway",
            "drainageway & valley"="drainageway")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change fan apron & to fan apron
rep_st <- c( "fan apron & drainageway" = "fan apron", 
             "fan apron & fan" = "fan apron",                                 
             "fan apron & fan piedmont" = "fan apron", 
             "fan apron & fan remnant" = "fan apron",                           
             "fan apron & pediment" = "fan apron",
             "fan apron & rock pediment" = "fan apron",                         
             "fan apron & sand sheet" = "fan apron")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

#change fan piedmont & to fan piedmont            
rep_st <- c( "fan piedmont & alluvial fan" = "fan piedmont", 
             "fan piedmont & bar & channel" = "fan piedmont",                      
             "fan piedmont & drainageway"  = "fan piedmont", 
             "fan piedmont & eroded fan remnant"  = "fan piedmont",                
             "fan piedmont & fan apron" = "fan piedmont",
             "fan piedmont & fan apron remnant" = "fan piedmont",                  
             "fan piedmont & fan collar" = "fan piedmont",
             "fan piedmont & fan remnant" = "fan piedmont",                        
             "fan piedmont & pediment" = "fan piedmont", 
             "fan piedmont & rock pediment" = "fan piedmont",                      
             "fan piedmont & sand sheet & fan apron" = "fan piedmont",
             "fan piedmont & sand sheet & fan collar" = "fan piedmont",            
             "fan piedmont & sand sheet & fan remnant" = "fan piedmont", 
             "fan piedmont & wash" = "fan piedmont" )
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change fan remnant & to fan remnant
rep_st <- c("fan remnant & channel" = "fan remnant", 
            "fan remnant & fan apron" = "fan remnant",                           
            "fan remnant & fan piedmont" = "fan remnant",  
            "fan remnant & intermontane basin" = "fan remnant",                  
            "fan remnant & sand sheet" = "fan remnant")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change hill & to hill
rep_st <- c( "high hill" = "hill",                                               
             "hill & fan piedmont" = "hill", 
             "hill & mountain" = "hill",                                   
             "hill & rock pediment" = "hill",
             "hillside" = "hill",                                          
             "hillside or mountainside" = "hill",
             "hillslope" = "hill",                                         
             "hillslope & fan remnant" = "hill",
             "hillslope & mountain slope" = "hill")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change mountain & to mountain
rep_st <- c("mountain & hill" = "mountain",                                    
            "mountain & mountain slope" = "mountain",                         
            "mountain slope" = "mountain",                                     
            "mountain valley" = "mountain")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change playa floor slope and step to playa
rep_st <- c("playa floor" = "playa",                                        
            "playa slope" = "playa",                                       
            "playa step" = "playa")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change sandsheet & to sandsheet
rep_st <- c("sand ramp" = "sand sheet",                                        
            "sand sheet & basin floor" = "sand sheet",                          
            "sand sheet & fan apron" = "sand sheet",                             
            "sand sheet & fan piedmont" = "sand sheet",                         
            "sand sheet & lake plain" = "sand sheet")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change hill & to hill
rep_st <- c("hill & fan remnant" = "hill",                                
            "hill & mountain" = "hill",                                    
            "hill or mountainside" = "hill")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change fan skirt & to fan skirt
rep_st <- c("fan skirt & dune field" = "fan skirt",                            
            "fan skirt & sand sheet" = "fan skirt")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)
summary(pts$landform)

# change inset fan & to inset fan
rep_st <- c("inset fan & fan apron" = "inset fan",                              
            "inset fan & fan piedmont" = "inset fan",                          
            "inset fan & fan remnant" = "inset fan")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


# change terrace & to terrace
rep_st <- c("terrace & drainageway" = "terrace",                              
            "terrace & valley" = "terrace",                                  
            "terrace & wash" = "terrace")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change dune & to dunes
rep_st <- c("dune & sand sheet" = "dunes",                                 
            "dune field" = "dunes",
            "shrub-coppice dunes & dunes" = "dunes",
            "dune" = "dunes")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)



# change dune & to dunes
rep_st <- c("eroded fan remnant sideslope" = "fan remnant",
            "eroded fan remnant" = "fan remnant")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("stream terrace" = "terrace",                                     
            "stream terrace & river valley" = "terrace",                                             
            "terrace & river valley" = "terrace")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)





rep_st <- c("lava field" = "lava flow")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("coppice mound" = "shrub-coppice dunes")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

rep_st <- c("flood-plain step" = "flood plain")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

rep_st <- c("low hill" = "hill",
            "swale"= "drainageway",
            "lake terrace" = "lake plain",
            "lakebed"="lake plain",
            "pediment & fan apron" = "pediment")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("lake plain (relict)" = "lake plain",
            "pediment & fan piedmont"="pediment")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)





# change fan remnant sideslope & to fan remnant
rep_st <- c("arroyo" = "river wash",                                       
            "axial stream" = "river wash",
            "channel" = "river wash",
            "braided stream & swale" = "river wash")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

# change fan remnant sideslope & to fan remnant
pts$landform <- as.factor(str_replace_all(pts$landform, "^wash", "river wash"))
levels(pts$landform)



rep_st <- c("river wash & river valley" = "river wash")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

rep_st <- c("river valley & shrub-coppice dunes & terrace" = "valley",                             
            "river wash & river valley" = "river wash")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

rep_st <- c("shrub-coppice dunes & duness" = "shrub-coppice dunes")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)

rep_st <- c("volcanic pressure ridge" = "ridge",
            "bar & drainageway" = "bar",
            "faceted spur" = "spur",
            "outwash plain" = "plain",
            "strath terrace" = "terrace",
            "scarp slope" = "ridge"
)
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("beach terrace" = "terrace",
            "braided stream & drainageway" = "river wash",
            "^duness" = "dunes",
            "fan remnant & ballena" = "fan remnant",      
            "fan remnant & hill"  = "fan remnant",         
            "fan remnant & mountain" = "fan remnant",
            "lava plateau" = "lava flow",
            "lake plain (relict)" = "lake plain",
            "homoclinal ridge" = "ridge",
            "partial ballena" = "ballena",
            "free face" = "ridge")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("lake plain \\(relict\\)" = "lake plain")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("bajada" =  "fan piedmont",
            "^fan$" = "alluvial fan",
            "shrub-coppice dunes" = "dunes",
            "fan apron piedmont" = "fan apron",
            "fan apron remnant" = "fan apron",
            "fan collar" = "alluvial fan",
            "^flat" = "alluvial flat",
            "flood plain" = "river wash",
            "mountainside" = "mountain",
            "saddle" = "mountain",
            "shrub-coppice dunes" = "dunes",
            "fan terrace" = "fan remnant",
            "fan piedmont remnant" = "fan remnant"
            )
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)


rep_st <- c("^bar$" = "river wash",
            "inselberg" = "hill"
)
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)
summary(pts$landform)





# correlation from modeling efforts


rep_st <- c("cinder cone" = "lava flow")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)
summary(pts$landform)

pts <- subset(pts, landform!="valley")
pts$landform <- droplevels(pts$landform)
levels(pts$landform)
summary(pts$landform)

rep_st <- c("spur" = "mountain",
            "drainageway" = "river wash",
            "mesa" = "plateau",
            "inset fan" = "fan remnant")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)
summary(pts$landform)

pts <- subset(pts, landform!="plain")
pts$landform <- droplevels(pts$landform)
pts <- subset(pts, landform!="bench")
pts$landform <- droplevels(pts$landform)
levels(pts$landform)
summary(pts$landform)


rep_st <- c("river wash" = "fan remnant")
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)
summary(pts$landform)






###### end of correlation

#review cols
summary(pts$mst)
summary(pts$temp)
summary(pts$landform)

#rename to shorter codes
rep_st <- c('aridic' = 'ar',
            'ustic' = 'us')
pts$mst <- as.factor(str_replace_all(pts$mst, rep_st))
levels(pts$mst)
summary(pts$mst)

rep_st <- c('frigid' = 'fr',
            'hyperthermic' = 'hy',
            'mesic' = 'me',
            'thermic' = 'th')
pts$temp <- as.factor(str_replace_all(pts$temp, rep_st))
levels(pts$temp)
summary(pts$temp)

rep_st <- c('alluvial fan' = 'allFan',
            'alluvial flat' = 'allFlat',
            'ballena' = 'ballena',
            'basin floor' = 'basin',
            'dunes' = 'dunes',
            'fan apron' = 'fanApron',
            'fan piedmont' = 'fanPied',
            'fan remnant' = 'fanRem',
            'fan skirt' = 'fanSkirt',
            'hill' = 'hill',
            'lake plain' = 'lakePlain',
            'lava flow' = 'lavaFlow',
            'mountain' = 'mountain',
            'pediment' = 'pediment',
            'plateau' = 'plateau',
            'playa' = 'playa',
            'ridge' = 'ridge',
            'rock pediment' = 'rockPed',
            'sand sheet' = 'sandsheet',
            'terrace' = 'terrace')
pts$landform <- as.factor(str_replace_all(pts$landform, rep_st))
levels(pts$landform)
summary(pts$landform)


# concat cols to form class col
#pts <- na.omit(pts)


#pts$class <- as.factor(paste(pts$mst, pts$temp, pts$landform, sep=""))

#levels(pts$class)
#summary(pts$class)
###################################################################

# bring in raster data for mst and temp regime
setwd("C:/8-VIC-class5Projects")
summary(pts$landform)

names(pts)
pts <- pts[-c(13,14)]

# read in raster file names as a list
rList=list.files(getwd(), pattern="tif$", full.names = FALSE)
rList

# create a raster stack of covs
rStack <- rast(rList)


names(rStack)
#names(rStack)<- c('mst','temp')
#names(rStack)

#extract values to pts
pts.extract <- terra::extract(rStack, pts, xy=F, bind=TRUE, na.rm=TRUE) 

pts <- sf::st_as_sf(pts.extract)
# combine cols concat


summary(as.factor(pts$mst))

rep_st <- c('1' = 'ar',
            '2' = 'us')
pts$mst <- as.factor(str_replace_all(pts$mst, rep_st))
levels(pts$mst)
summary(pts$mst)

rep_st <- c('1' = 'FR',
            '2' = 'HY',
            '3' = 'ME',
            '4' = 'TH')
pts$temp <- as.factor(str_replace_all(pts$temp, rep_st))
levels(pts$temp)
summary(pts$temp)






# concat cols to form class col
#pts <- na.omit(pts)


pts$Class <- as.factor(paste(pts$mst, pts$temp, pts$landform, sep=""))

levels(pts$Class)
summary(pts$Class)


#####################
rep_st <- c('usFRallFan' = 'usFR',
            'usFRmountain' = 'usFR',
            'usFRfanRem'= 'usFR',
            'usMEhill' = 'usME',
            'usMEmountain' = 'usME')
pts$Class <- as.factor(str_replace_all(pts$Class, rep_st))
levels(pts$Class)
summary(pts$Class)


# remove classes with <3 observations

pts <- subset(pts, Class!="arMEfanSkirt")
pts$Class <- droplevels(pts$Class)
pts <- subset(pts, Class!="arTHbasin")
pts$Class <- droplevels(pts$Class)
pts <- subset(pts, Class!="usTHmountain")
pts$Class <- droplevels(pts$Class)
levels(pts$Class)
summary(pts$Class)





# climate and landform splits - 49 classes RF model 60% accuracy





###############################

# split out arTHfanRem - 656 observations
pts.all <- subset(pts, Class != "arTHfanRem")
pts.all$Class <- droplevels(pts.all$Class)
levels(pts.all$Class)


pts.sub <- subset(pts, Class == "arTHfanRem")
pts.sub$Class <- droplevels(pts.sub$Class)
levels(pts.sub$Class)

names(pts.sub)

levels(as.factor(pts.sub$txsbgrp))

summary(as.factor(pts.sub$txsbgrp))
summary(as.factor(pts.sub$txprtsz))

pts.sub$txsbgrp <- as.factor(pts.sub$txsbgrp)

pts.sub <- subset(pts.sub, txsbgrp!="aquic torrifluvents")
pts.sub$txsbgrp <- droplevels(pts.sub$txsbgrp)
summary(as.factor(pts.sub$txsbgrp))


rep_st <- c('arenic calciargids' = 'arenic haplargids',
            'arenic paleargids' = 'arenic haplargids',
            'arenic ustic haplargids' = 'arenic haplargids'
            )
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)


rep_st <- c('calcic lithic petrocalcids' = 'calcic petrocalcids')
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

# remove classes with less than 3 observations
# remove classes with < 2 observations per class
pts.sub <- pts.sub[pts.sub$txsbgrp %in% names(which(table(pts.sub$txsbgrp)>2)),]
levels(pts.sub$txsbgrp)
pts.sub$txsbgrp <- droplevels(pts.sub$txsbgrp)
levels(pts.sub$txsbgrp)  #28 classes 4129 observations

summary(pts.sub$txsbgrp)

# renaming classes
#arTHfanRem

rep_st <- c("lithic ustic haplargids" = "UA",   
            "ustic calciargids" = "UA",
            "ustic haplargids"= "UA",
            "ustic haplocalcids"= "UA",
            "ustic haplocambids"= "UA",
            "ustic torriorthents"= "UA",
            "arenic haplargids" = "arHap",
            "argic petrocalcids" = "arPet",
            "argidic argidurids" = "tyHapdur",
            "cambidic haplodurids" = "tyHapdur",
            "typic haplodurids" = "tyHapdur",
            "calcic petrocalcids" = "caPet",
            "duric petroargids" = "tyPetarg","typic petroargids" = "tyPetarg",      
            "durinodic haplocalcids" = "tyHapcal",
            "petronodic haplocalcids" = "tyHapcal",
            "typic haplocalcids" = "tyHapcal",
            "petronodic calciargids" = "tyCalarg", "typic calciargids" = "tyCalarg",
            "typic argidurids"  = "tyHapdur",            
            "typic haplargids"  = "tyHaparg", 
            "typic haplocambids"  = "tyHapcamb",     
            "typic paleargids"  = "tyPalarg",              
            "typic petrocalcids"  = "tyPetcal",      
            "typic torriorthents"  = "tyTO",    
            "typic torripsamments"  = "tyTP")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

pts.sub$Class <- as.factor(pts.sub$txsbgrp)
summary(pts.sub$Class)



rep_st <- c("arPet" = "tyHaparg",
            "tyHapcamb" = "tyHaparg",
            "tyPetarg" = "tyHaparg",
            "tyPalarg" = "tyHaparg")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

rep_st <- c("arHap" = "tyHaparg",
            "tyPetcal" = "caPet")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)







pts.sub$Class <- as.factor(pts.sub$txsbgrp)

######################
# split out tyHaparg
pts.sub.all <- subset(pts.sub, Class != "tyHaparg")
pts.sub.all$Class <- droplevels(pts.sub.all$Class)
levels(pts.sub.all$Class)


pts.sub.sub <- subset(pts.sub, Class == "tyHaparg")
pts.sub.sub$Class <- droplevels(pts.sub.sub$Class)
levels(pts.sub.sub$Class)


# inspect fpsc
levels(as.factor(pts.sub.sub$txprtsz))
summary(as.factor(pts.sub.sub$txprtsz))

rep_st <- c("^coarse-loamy$" = "cl",                             
            "^coarse-loamy over sandy or sandy-skeletal$" = "cl",
            "^fine-loamy$" = "fl",                               
            "^loamy$" = "losk",                                   
            "^loamy-skeletal$" = "losk",
            "^sandy$" = "snd",                                    
            "^sandy-skeletal$" = "snd" )
pts.sub.sub$txprtsz <- as.factor(str_replace_all(pts.sub.sub$txprtsz, rep_st))


levels(pts.sub.sub$txprtsz)
summary(pts.sub.sub$txprtsz)


rep_st <- c("cl" = "tyHapargCL",
            "fl" = "tyHapargFL",
            "losk" = "tyHapargLosk",
            "snd" = "tyHapargSnd")
pts.sub.sub$txprtsz <- as.factor(str_replace_all(pts.sub.sub$txprtsz, rep_st))
levels(pts.sub.sub$txprtsz)
summary(pts.sub.sub$txprtsz)


pts.sub.sub$Class <- as.factor(pts.sub.sub$txprtsz)
summary(pts.sub.sub$Class)

#merge df back together

pts.sub <- rbind(pts.sub.all, pts.sub.sub)


#@### check names #arTHfanRem
levels(pts.sub$Class)
rep_st <- c("caPet" = "arTHfRcaPet",
            "tyCalarg" = "arTHfRtuCalarg", 
            "tyHapcal" = "arTHfRtyhapcal",
            "tyHapdur" = "arTHfRtyhapdur",
            "tyTO" = "arTHfRtyto",
            "tyTP" = "arTHfRtytp",        
            "UA" = "arTHfRUA",
            "tyHapargCL" = "arTHfRtyHapargCL",
            "tyHapargFL" = "arTHfRHapargFL",
            "tyHapargLosk" = "arTHfRtyHapargLosk",
            "tyHapargSnd" = "arTHfRtyHapargsnd")
pts.sub$Class <- as.factor(str_replace_all(pts.sub$Class, rep_st))
levels(pts.sub$Class)
summary(pts.sub$Class)


#merge df back together
pts <- rbind(pts.all, pts.sub)



######################
# split out arTHhill
pts.all <- subset(pts, Class != "arTHhill")
pts.all$Class <- droplevels(pts.all$Class)
levels(pts.all$Class)


pts.sub <- subset(pts, Class == "arTHhill")
pts.sub$Class <- droplevels(pts.sub$Class)
levels(pts.sub$Class)




# inspect fpsc
levels(as.factor(pts.sub$txprtsz))
summary(as.factor(pts.sub$txprtsz))

# inspect taxononmy
levels(as.factor(pts.sub$txsbgrp))
summary(as.factor(pts.sub$txsbgrp))



rep_st <- c("argic petrocalcids" = "petro",
            "calcic petrocalcids" = "petro",
            "lithic calciargids" = "lith",         
            "lithic haplargids" = "tyHaparg",
            "lithic haplocalcids" = "lith",
            "lithic haplocambids" = "lith",        
            "lithic haploxerolls" = "xeric",
            "lithic torriorthents" = "lith",
            "lithic torripsamments" = "lith",      
            "lithic ustic haplargids" = "UA",
            "lithic ustic haplocambids" = "UA",
            "lithic ustic torriorthents" = "UA", 
            "lithic xeric haplocambids" = "xeric",
            "torriorthentic haploxerolls" = "xeric",
            "typic calciargids" = "tyHaparg",
            "typic haplargids" = "tyHaparg",
            "typic haplocalcids" = "tyHapcal",
            "typic haplocambids"  = "tyHapcal",       
            "typic paleargids"  = "tyHaparg",
            "typic torriorthents"  = "tyTO",
            "typic torripsamments"  = "tyTP",      
            "ustic haplargids"   = "UA",
            "ustic haplocambids"  = "UA",
            "ustic torriorthents"  = "UA", 
            "ustic torripsamments"  = "UA",  
            "xeric haplargids"    = "xeric",
            "xeric haplocambids"  = "xeric",        
            "xeric torriorthents"  = "xeric",
            "xeric torripsamments"  = "xeric")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

rep_st <- c("petro" = "tyHapcal")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

# remove classes with < 2 observations per class
pts.sub <- pts.sub[pts.sub$txsbgrp %in% names(which(table(pts.sub$txsbgrp)>2)),]
levels(pts.sub$txsbgrp)
pts.sub$txsbgrp <- droplevels(pts.sub$txsbgrp)
levels(pts.sub$txsbgrp)  #28 classes 4129 observations

summary(pts.sub$txsbgrp)
levels(pts.sub$txsbgrp)

rep_st <- c("lith" = "arThHlith",
            "tyHaparg" = "arThHtyhaparg",
            "tyHapcal" = "arThHtyhapcal",
            "tyTO" = "arThHtyTO",
            "tyTP" = "arThHtyTP",
            "UA" = "arThHUA",
            "xeric" = "arThHxeric" )
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)


pts.sub$Class <- pts.sub$txsbgrp


#merge df back together
pts <- rbind(pts.all, pts.sub)


######################
# split out arHYfanRem
levels(pts$Class)
summary(pts$Class)
pts.all <- subset(pts, Class != "arHYfanRem")
pts.all$Class <- droplevels(pts.all$Class)
levels(pts.all$Class)


pts.sub <- subset(pts, Class == "arHYfanRem")
pts.sub$Class <- droplevels(pts.sub$Class)
levels(pts.sub$Class)




# inspect fpsc
levels(as.factor(pts.sub$txprtsz))
summary(as.factor(pts.sub$txprtsz))

# inspect taxononmy
levels(as.factor(pts.sub$txsbgrp))
summary(as.factor(pts.sub$txsbgrp))



rep_st <- c( "arenic calciargids" = "typic calciargids", 
             "arenic haplargids" = "typic haplargids", 
             "calcic petrocalcids" = "typic petrocalcids",  
             "duric haplocalcids" = "typic haplocalcids",     
             "duric petroargids" = "typic petrocalcids",  
             "durinodic haplocalcids" = "typic haplocalcids",  
             "lithic haplargids" = "lithic",  
             "lithic torriorthents" = "lithic", 
             "petronodic calciargids" = "typic calciargids", 
             "petronodic paleargids" = "typic haplargids",  
             "typic argidurids" = "argidic argidurids",     
             "typic petroargids" = "typic petrocalcids"
            )
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)


# remove classes with < 3 observations per class
pts.sub <- pts.sub[pts.sub$txsbgrp %in% names(which(table(pts.sub$txsbgrp)>3)),]
levels(pts.sub$txsbgrp)
pts.sub$txsbgrp <- droplevels(pts.sub$txsbgrp)
levels(pts.sub$txsbgrp)  #28 classes 4129 observations

summary(pts.sub$txsbgrp)

pts.sub$Class <- pts.sub$txsbgrp


levels(pts.sub$Class)

rep_st <- c("argidic argidurids" = "arHYfRarargidu",
            "cambidic haplodurids" = "arHYfRcahapdu",
            "typic calciargids" = "arHYfRtycalarg",
            "typic haplargids" = "arHYfRtyhapar",    
            "typic haplocalcids" = "arHYfRtyhapcal", 
            "typic haplocambids" = "arHYfRtyhapcam",
            "typic haplodurids" = "arHYfRtyhapdur",
           "typic petrocalcids"   = "arHYfRtypetro",
           "typic torriorthents" = "arHYfRtyto",
           "typic torripsamments" = "arHYfRtytp"
)
pts.sub$Class <- as.factor(str_replace_all(pts.sub$Class, rep_st))
levels(pts.sub$Class)
summary(pts.sub$Class)




#merge df back together
pts <- rbind(pts.all, pts.sub)



######################
# split out arTHfanApron
levels(pts$Class)
summary(pts$Class)
pts.all <- subset(pts, Class != "arTHfanApron")
pts.all$Class <- droplevels(pts.all$Class)
levels(pts.all$Class)


pts.sub <- subset(pts, Class == "arTHfanApron")
pts.sub$Class <- droplevels(pts.sub$Class)
levels(pts.sub$Class)




# inspect fpsc
levels(as.factor(pts.sub$txprtsz))
summary(as.factor(pts.sub$txprtsz))

# inspect taxononmy
levels(as.factor(pts.sub$txsbgrp))
summary(as.factor(pts.sub$txsbgrp))



rep_st <- c("arenic calciargids" = "typic calciargids",
            "arenic haplargids" = "typic haplargids",
            "arenic paleargids"  = "typic haplargids",          
            "argic petrocalcids" = "typic petroargids",
            "lithic calciargids"  = "lithic",         
            "lithic haplargids" = "lithic",
            "lithic torriorthents" = "lithic",
            "lithic torripsamments"  = "lithic",      
            "pachic haploxerolls" = "xeric",
            "petronodic haplocalcids" = "typic haplocalcids",
            "torripsammentic haploxerolls" = "xeric",
            "typic haplocambids" = "typic haplocalcids",
            "typic petroargids" = "typic petroargids",
            "typic petrocalcids"   = "typic petrocalcids",        
            "typic petrocambids"  = "typic petrocalcids",
            "ustic calciargids" = "ustic",
            "ustic haplargids"  = "ustic",
            "ustic haplocalcids"   = "ustic",        
            "ustic haplocambids" = "ustic",
            "ustic petroargids" = "ustic",
            "ustic petrocalcids"     = "ustic",      
            "ustic petrocambids" = "ustic",
            "ustic torriorthents"  = "ustic",
            "xeric torripsamments"  = "xeric"
)
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

rep_st <- c("typic calciargids" = "typic haplargids",
            "typic petroargids" = "typic petrocalcids")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

# remove classes with < 3 observations per class
pts.sub <- pts.sub[pts.sub$txsbgrp %in% names(which(table(pts.sub$txsbgrp)>3)),]
levels(pts.sub$txsbgrp)
pts.sub$txsbgrp <- droplevels(pts.sub$txsbgrp)
levels(pts.sub$txsbgrp)  #28 classes 4129 observations

summary(pts.sub$txsbgrp)


# split out arTHfanApron
rep_st <- c("lithic" = "arTHfAlith",
            "typic haplargids"  = "arTHfAhaparg",
            "typic haplocalcids"  = "arTHfAhapcal",
            "typic petrocalcids"  = "arTHfApetro",
            "typic torriorthents"  = "arTHfAtyto",
            "typic torripsamments" = "arTHfAtytp",
            "ustic"   = "arTHfAustic",
            "xeric" = "arTHfAxeric")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)


pts.sub$Class <- pts.sub$txsbgrp



#merge df back together
pts <- rbind(pts.all, pts.sub)

######################
# split out arHYfanApron
levels(pts$Class)
summary(pts$Class)
pts.all <- subset(pts, Class != "arHYfanApron")
pts.all$Class <- droplevels(pts.all$Class)
levels(pts.all$Class)


pts.sub <- subset(pts, Class == "arHYfanApron")
pts.sub$Class <- droplevels(pts.sub$Class)
levels(pts.sub$Class)




# inspect fpsc
levels(as.factor(pts.sub$txprtsz))
summary(as.factor(pts.sub$txprtsz))

# inspect taxononmy
levels(as.factor(pts.sub$txsbgrp))
summary(as.factor(pts.sub$txsbgrp))



rep_st <- c("arenic calciargids" = "typic calciargids" ,
            "arenic haplargids" = "typic calciargids",
            "arenic paleargids" = "typic calciargids",
            "argidic argidurids"  = "haplodurids",
            "calcic paleargids" = "typic calciargids",
            "cambidic haplodurids" = "haplodurids",
            "duric petroargids" = "typic petrocalcids",
            "typic haplargids"=  "typic calciargids",
            "typic haplocambids" = "typic haplocalcids"  
)
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

rep_st <- c("typic petrocalcids" = "typic haplocalcids")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

# remove classes with < 3 observations per class
pts.sub <- pts.sub[pts.sub$txsbgrp %in% names(which(table(pts.sub$txsbgrp)>3)),]
levels(pts.sub$txsbgrp)
pts.sub$txsbgrp <- droplevels(pts.sub$txsbgrp)
levels(pts.sub$txsbgrp)  #28 classes 4129 observations

summary(pts.sub$txsbgrp)




rep_st <- c("haplodurids" = "arHyFAhapdur",
            "typic calciargids" = "arHyFAtycalcarg", 
            "typic haplocalcids" = "arHyFAtyhapcal",
            "typic torriorthents"  = "arHyFAtyto",
            "typic torripsamments" = "arHyFAtytp")
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)






pts.sub$Class <- pts.sub$txsbgrp



levels(pts.sub$Class)


#merge df back together
pts <- rbind(pts.all, pts.sub)



#review of model correlation matrix correlating highly confused small observation classes

rep_st <- c('arHyFAhapdur' = 'arHyFAtyhapcal',
            'arHYfRtyhapdur' = 'arHYfRtyhapcam',
            'arMEfanApron' = 'arMEallFan',
            'arMEfanPied' = 'arMEallFan',
            'arTHdunes' = 'arTHsandsheet',
            'usMEfanRem' = 'usME',
            'arHYridge' = 'arHYmountain')
pts$Class <- as.factor(str_replace_all(pts$Class, rep_st))
levels(pts$Class)
summary(pts$Class)


rep_st <- c(
  'arHyFAtyhapcal'= 'arHyFAtycalcarg',
  'arHYfRtyhapcam'= 'arHYfRtyhapcal',
  'arMEfanRem'= 'arMEallFan',
  'arMElavaFlow'= 'arMEhill',
  'arTHfRHapargFL'= 'arTHfRtyHapargCL'
)
pts$Class <- as.factor(str_replace_all(pts$Class, rep_st))
levels(pts$Class)
summary(pts$Class)


rep_st <- c(
  'arHYfRcahapdu'= 'arHYfRarargidu'
)
pts$Class <- as.factor(str_replace_all(pts$Class, rep_st))
levels(pts$Class)
summary(pts$Class)

rep_st <- c(
  'arTHfRtyHapargLosk'= 'arTHfRtyHapargCL'
)
pts$Class <- as.factor(str_replace_all(pts$Class, rep_st))
levels(pts$Class)
summary(pts$Class)

####
# split out arTHfanSkirt
levels(pts$Class)
summary(pts$Class)
pts.all <- subset(pts, Class != "arTHfanSkirt")
pts.all$Class <- droplevels(pts.all$Class)
levels(pts.all$Class)


pts.sub <- subset(pts, Class == "arTHfanSkirt")
pts.sub$Class <- droplevels(pts.sub$Class)
levels(pts.sub$Class)

rep_st <- c(
  "sodic haplocalcids" = "arTHfAtycal",
  "typic haplocalcids" = "arTHfAtycal",
  "typic torriorthents" = "arTHfAtyto",
  "typic torripsamments" = "arTHfAtytp"
)
pts.sub$txsbgrp <- as.factor(str_replace_all(pts.sub$txsbgrp, rep_st))
levels(pts.sub$txsbgrp)
summary(pts.sub$txsbgrp)

pts.sub$Class <- as.factor(pts.sub$txsbgrp)

#merge df back together
pts <- rbind(pts.all, pts.sub)


###########
################### END
setwd("C:/8-VIC-class5Projects")

#combine data.frames
#pts <- rbind(pts.all, pts.sub)
st_write(pts, "pedons.shp")
#st_write(pts.sub, "pedons.shp")
