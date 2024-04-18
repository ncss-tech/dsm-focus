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

################### END
setwd("C:/8-VIC-class5Projects")


st_write(pts, "pedons.shp")
