#Landform correlation
#
# load and install packages
required.packages <- c( "caret", "sf", "terra", "aqp", "stringr")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

#Bring in training points
setwd("~/wy/data")
getwd()

#bring in shapefile 
pts <- st_read("WY630_737_pedons_lru_moistreg.shp")

#check names of attributes in pts., remove extra columns. Only need strata column.
names(pts)
pts <- pts[-c(1:66, 68:92, 94:97)]

#remove duplicate points
pts <- unique(pts)

#create new landform column to preserve pre-correlation information
pts$landform <-as.factor(pts$landfrm)
names(pts)
summary(pts$landform)
levels(pts$landform)




#hillslopes

cor <- c("anticlines on dip slopes on low hills" = "hillslopes",
         "anticlines on hogbacks on dip slopes on low hills" = "hillslopes",
         "anticlines on hogbacks on scarp slopes on low hills" = "hillslopes",
         "anticlines on homoclinal ridges on low hills" ="hillslopes",
         "anticlines on scarp slopes on low hills" = "hillslopes",
         "ballenas on hills" = "hillslopes",
         "cuestas on hillslopes" = "hillslopes",
         "drainageways on hills" = "drainageways",
         "escarpments on hillslopes" = "escarpments",
         "^high hills$" = "hillslopes",
         "high hills on hillsides" = "hillslopes",
         "high hills on hillslopes" = "hillslopes",
         "^hills$" = "hillslopes",
         "hills on benches" = "hillslopes",
         "hills on drainageways" = "drainageways",
         "hills on pediments" = "hillslopes",
         "^hills on saddles$" = "hillslopes",
         "^hillsides$" = "hillslopes",
         "hillsides or mountainsides" = "hillslopes",
         "hillslopes on cuestas" = "cuestas",
         "hillslopes on drainageways" = "drainageways",
         "hillslopes on interfluves" = "hillslopes",
         "hillslopes on intermontane basins" = "hillslopes",
         "hillslopes on swales" = "hillslopes",
         "^hogbacks on scarp slopes on low hills$" ="hillslopes",
         "interfluves on hillslopes" = "hillslopes",
         "interfluves on intermontane basins" = "hillslopes",
         "^interfluves$" = "hillslopes",
         "^low hills$" = "hillslopes",
         "low hills on saddles" = "hillslopes",
         "low hills on valley-floor remnants" = "hillslopes",
         "swales on hillslopes" = "hillslopes",
         "^saddles$" = "hillslopes",
         "^plains$" = "hillslopes",
         "^rises$" = "hillslopes",
         "^upland slopes$" = "hillslopes",
         "intermontane basins on interfluves" = "hillslopes",
         "^breaks$" = "hillslopes",
         "plateaus on rims" = "hillslopes")

pts$landform <- as.factor(str_replace_all(pts$landform, cor))

levels(pts$landform)


#questionable nesting
cor <- c("basin-floor remnants on anticlines on dip slopes on high hills" = "basin-floor remnants",
         "basin-floor remnants on anticlines on dip slopes on low hills" = "basin-floor remnants",
         "basin-floor remnants on anticlines on high hills" = "basin-floor remnants",
         "basin-floor remnants on anticlines on scarp slopes on high hills" = "basin-floor remnants",
         "basin-floor remnants on anticlines on scarp slopes on low hills" = "basin-floor remnants",
         "basin-floor remnants on hills" = "basin-floor remnants",
         "^basin-floor remnants on low hills$" = "basin-floor remnants",
         "basin-floor remnants on low hills on saddles" = "basin-floor remnants",
         "basin-floor remnants on mesas on hillslopes" = "basin-floor remnants",
         "basin-floor remnants on mesas on low hills" = "basin-floor remnants",
         "basin-floor remnants on outwash plains on low hills" = "basin-floor remnants",
         "intermontane basins on basin-floor remnants on low hills" = "basin-floor remnants",
         "intermontane basins on basin-floor remnants on low hills on saddles" = "basin-floor remnants",
         "outwash deltas on hillslopes" = "hillslopes",
         "plateaus on hills" = "hillslopes",
         "fan remnants on hillslopes" = "fan remnants",
         "high hills on alluvial fans" = "alluvial fans",
         "hills on dunes" = "dunes",
         "^hills on mesas$" = "mesas",
         "low hills on mesas" = "mesas",
         "mesas on hills" = "mesas",
         "^hills on ridges$" = "hillslopes",
         "hills on ridges on swales" = "hillslopes",
         "hills on valleys" = "hillslopes",
         "hillslopes on escarpments" = "escarpments",
         "^hillslopes on fan remnants$" = "fan remnants",
         "hillslopes on fan remnants on alluvial fans" = "fan remnants",
         "hillslopes on interdunes" = "dunes",
         "hillslopes on ridges" = "hillslopes",
         "hillslopes on terraces" = "terraces",
         "homoclinal ridges on escarpments" = "escarpments",
         "drainageways on fans" = "drainageways"
)

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#Ridges

cor <- c("homoclinal ridges on cuestas on hillslopes" = "cuestas",
         "^homoclinal ridges$" = "hillslopes",
         "^homoclinal ridges on cuestas$" = "cuestas",
         "homoclinal ridges on dip slopes" = "cuestas",
         "homoclinal ridges on hillslopes" = "hillslopes",
         "homoclinal ridges on scarp slopes" = "escarpments",
         "ridges on hills" = "hillslopes",
         "monoclines" = "hillslopes"
)


pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#benches

cor <- c("^structural benches$" = "structural benches",
         "^benches$" = "structural benches",
         "structural benches on cuestas on hillslopes" = "structural benches",
         "structural benches on hillslopes" = "structural benches"
)

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#alluvial fans

#cor <- c("alluvial fans on basin floor" = "basin-floor remnants")


#pts$landform <- as.factor(str_replace_all(pts$landform, cor))
#levels(pts$landform)


#basin floors

cor <- c("^basin-floor remnants$" = "basin-floor remnants",
         "^intermontane basins$" = "basin-floor remnants",
         "^intermontane basins on basin-floor remnants$" = "basin-floor remnants",
         "basin-floor remnants on anticlines on dip slopes on strike valleys" = "basin-floor remnants",
         "basin-floor remnants on anticlines on strike valleys" = "basin-floor remnants",              
         "basin-floor remnants on blowouts" = "basin-floor remnants",                                 
         "^basin-floor remnants on drainageways$" = "basin-floor remnants",                             
         "basin-floor remnants on eroded fan remnants" = "basin-floor remnants",                       
         "basin-floor remnants on interdunes" = "basin-floor remnants",      
         "basin-floor remnants on low hillslopes" = "basin-floor remnants",                            
         "basin-floor remnants on pediments" ="basin-floor remnants",                                 
         "basin-floor remnants on playas" = "basin-floor remnants",                                    
         "basin-floor remnants on stream terraces" = "basin-floor remnants",                           
         "basin-floor remnants on strike drainageways" = "basin-floor remnants",                      
         "^basin-floor remnants on strike valleys&" = "basin-floor remnants",                            
         "basin-floor remnants on terraces" = "basin-floor remnants",                                  
         "drainageways on basin-floor remnants" = "basin-floor remnants",
         "sinkholes on basin floors" = "basin-floor remnants",
         "terraces on basin floor" = "basin-floor remnants",
         "^basin floors$" = "basin-floor remnants")

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#dunes
#if it contains dune it becomes "dunes"

cor <- c("structural benches on dunes" = "dunes",
         "dunes on basin floor" = "dunes",
         "^basin-floor remnants on dunes$" = "dunes",
         "basin-floor remnants on dune fields on interdunes" = "dunes",
         "basin-floor remnants on dunes on blowouts" = "dunes",
         "basin-floor remnants on longitudinal dunes" = "dunes",
         "dune fields on dunes" = "dunes",
         "dune fields on interdunes" = "dunes",
         "dunes on alluvial fans" = "dunes",
         "dunes on basin-floor remnants" = "dunes",
         "dunes on dip slopes on cuestas" = "dunes",
         "dunes on fan remnants" = "dunes",
         "dunes on playa floors" = "dunes",
         "dunes on strath terraces" = "dunes",
         "dune fields on escarpments" = "dunes",
         "dune fields on fan remnants" = "dunes",
         "dune slacks" = "dunes",
         "^dune fields$" = "dunes",
         "fan remnants on dunes" = "dunes",
         "^interdunes$" = "dunes",
         "playas on dunes" = "dunes",
         "river valleys on dunes" = "dunes")

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)



#depressions
cor <- c("basin-floor remnants on depressions" = "basin-floor remnants",
         "basin floor on depressions" = "basin-floor remnants",
         "flood plains on depressions" = "basin-floor remnants",
         "alluvial flats on depressions" = "alluvial flats",
         "depressions on alluvial flats" = "alluvial flats",
         "depressions on valleys" = "flood plains")

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)

#change all terraces to terraces
cor <- c("^stream terraces$" = "terraces",
         "strath terraces" = "terraces",
         "outwash terraces" = "terraces",
         "^paleoterraces&" = "terraces",
         "basin-floor remnants on paleoterraces" = "terraces",
         "river valleys on stream terraces" = "terraces",
         "fluvial terraces" = "terraces",
         "^fans on terraces$" = "terraces",
         "river valleys on terraces"= "terraces",
         "alluvial fans on terraces" = "terraces",
         "valleys on terraces" = "terraces",
         "drainageways on terraces" = "terraces",
         "^fan terraces$" = "terraces",
         "terraces--stream or lake" = "terraces",
         "terraces--outwash or marine" = "terraces",
         "dunes on terraces" = "terraces",
         "fan remnants on terraces" = "terraces",
         "drainageways on outwash terraces" = "terraces",
         "drainageways on stream terraces" = "terraces",
         "intermontane basins on stream terraces" = "terraces",
         "plateaus on terraces" = "terraces",
         "river valleys on paleoterraces" = "terraces",
         "stream terraces on river valleys" = "terraces",
         "stream terraces on swales" = "terraces",
         "terraces on backswamps" = "terraces",
         "terraces on intermontane basins" = "terraces",
         "terraces on valleys" = "terraces",
         "terraces on flats" = "terraces"
)
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#flood plains
cor <- c("^flood-plain steps$" = "flood plains",
         "river valleys on flood-plain steps" = "flood plains",
         "river valleys on flood plains" = "flood plains",
         "basin-floor remnants on flood-plain steps" = "flood plains",
         "^alluvial flats$" = "alluvial flats",
         "intermontane basins on alluvial flats" = "alluvial flats",
         "flood plains on alluvial flats" = "alluvial flats",
         "alluvial flats on intermontane basins" = "alluvial flats",
         "^flats$" = "alluvial flats",
         "outwash plains" = "flood plains",
         "basin floor on flood plains" = "flood plains",
         "depressions on flood plains" = "flood plains",
         "valleys on flood plains" = "flood plains",
         "oxbows on flood plains" = "flood plains",
         "^oxbows$" = "flood plains",
         "meander scars on flood plains" = "flood plains",
         "terraces on flood plains" = "flood plains",
         "flood plains on valley sides" = "flood plains",
         "flood-plain steps on oxbows" = "flood plains",
         "intermontane basins on flood plains" = "flood plains",
         "oxbows on flood-plain steps" = "flood plains",
         "alluvial flats on flood plains" = "alluvial flats",
         "strike valleys on flood-plain steps" = "flood plains",
         "^valleys$" = "flood plains")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)

###################
#change all drainageways to drainageways
cor <- c( "alluvial flats on drainageways" = "drainageways",
          "mountains on drainageways" = "drainageways",
          "^swales$" = "drainageways",
          "^draws$" = "drainageways",
          "^deltas$" = "drainageways",
          "^drainhead complexes$" = "drainageways",
          "intermontane basins on basin-floor remnants on drainageways" = "drainageways",
          "intermontane basins on drainageways" = "drainageways",
          "intermontane basins on hogbacks on draws" = "drainageways",
          "flood plains on drainageways" = "drainageways",
          "fan remnants on drainageways" = "drainageways",
          "river valleys on drainageways" = "drainageways",
          "valleys on drainageways" = "drainageways",
          "mesas on drainageways" = "drainageways",
          "drainageways on valleys" = "drainageways",
          "cuestas on scarp slopes on drainageways" = "drainageways",
          "drainageways on alluvial fans" = "drainageways",
          "drainageways on alluvial flats" = "drainageways",
          "drainageways on intermontane basins" = "drainageways",
          "drainageways on mesas" = "drainageways",
          "drainageways on strike valleys" = "drainageways",
          "fan aprons on drainageways" = "drainageways",
          "^fans on drainageways$" = "drainageways")

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#clean up fans
cor <- c("^eroded fan remnants$" = "fan remnants",
         "^fans$" = "alluvial fans",
         "^fan aprons$" = "alluvial fans",
         "fan aprons on fans on terraces on hills on mountain slopes" = "alluvial fans",
         "^fan collars$" = "alluvial fans",
         "^fan piedmonts$" = "alluvial fans",
         "^pediments$" = "hillslopes",
         "eroded fan remnant sideslopes" = "fan remnants",
         "^erosion remnants$" = "fan remnants",
         "intermontane basins on alluvial fans" = "alluvial fans",
         "benches on fan remnants" = "fan remnants",
         "drainageways on alluvial fans" = "drainageways",
         "drainageways on fan remnants" = "drainageways",
         "basin floors on eroded fan remnants" = "basin-floor remnants",
         "flood plains on alluvial fans" = "flood plains",
         "outwash fans" = "alluvial fans",
         "terraces on alluvial fans" = "terraces",
         "^alluvial fans on alluvial flats$" = "alluvial fans",
         "alluvial fans on alluvial flats on stream terraces" = "terraces",
         "alluvial fans on colluvial aprons" = "alluvial fans",
         "alluvial fans on drainageways" = "drainageways",
         "alluvial fans on drainhead complexes" = "alluvial fans",
         "alluvial fans on intermontane basins" = "alluvial fans",
         "alluvial fans on outwash terraces" = "alluvial fans",
         "alluvial fans on stream terraces" = "terraces",
         "alluvial flats on alluvial fans" = "alluvial fans",
         "draws on fans" = "drainageways",
         "fan remnants on fans" = "fan remnants",
         "^inset fans$" = "alluvial fans",
         "fans on scarps" = "alluvial fans",
         "intermontaine basins on alluvial fans" = "alluvial fans",
         "intermontane basins on inset fans" = "alluvial fans",
         "strike valleys on alluvial fans" = "alluvial fans",
         "terraces on fans" = "terraces",
         "alluvial fans on basin floor" = "basin-floor remnants"
)
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#clean up ridges
cor <- c( "^cuestas$" = "cuestas",
          "^cuestas on dip slopes$" = "cuestas",
          "cuestas on dip slopes on saddles" = "cuestas",
          "^cuestas on scarp slopes$" = "cuestas",
          "^dip slopes$" = "cuestas",
          "dip slopes on cuestas" = "cuestas",
          "dip slopes on colluvial aprons" = "cuestas",
          "dip slopes on hogbacks" = "fan remnants",
          "dip slopes on intermontane basins" = "cuestas",
          "hogbacks on intermontane basins" = "hillslopes",
          "hogbacks on slides" = "hillslopes",
          "^hogbacks$" = "hillslopes",
          "swales on hogbacks" = "hillslopes",
          "mountains on ridges" = "fan remnants",
          "knobs on ridges" = "hillslopes",
          "^knobs$" = "hillslopes",
          "mountains on ridges" = "mountain slopes", ##mountain slopes not a thing?
          "saddles on ridges" = "hillslopes",
          "cuestas on homoclinal ridges" = "cuestas",
          "dip slopes on homoclinal ridges" = "cuestas",
          "hogbacks on ridges" = "hillslopes",
          "hogbacks on slumps" = "hillslopes",
          "^hogbacks on scarp slopes$" = "hillslopes",
          "intermontane basins on homoclinal ridges" = "hillslopes",
          "mountainsides on ridges" = "mountain slopes",
          "ridges on benches" = "structural benches", ##just benches?
          "ridges on knobs" = "hillslopes",
          "ridges on saddles" = "hillslopes",
          "upland slopes on ridges" = "hillslopes",
          "valleys on ridges" = "hillslopes")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#clean up benches
cor <- c(  "terraces on benches" = "structural benches",
           "drainageways on pediments on benches" = "drainageways")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#escarpments
cor <- c("^scarp slopes$" = "escarpments",
         "^anticlines on scarp slopes$" = "escarpments",
         "scarp slopes on hogbacks" = "escarpments",
         "scarp slopes on cuestas" = "escarpments",
         "ridges on escarpments" = "escarpments",
         "^scarps$" = "escarpments",
         "drainageways on escarpments" = "drainageways",
         "mountain slopes on escarpments" = "escarpments",
         "basin-floor remnants on escarpments" = "escarpments",
         "escarpments on intermontane basins" = "escarpments",
         "escarpments on mountain slopes" = "escarpments",
         "escarpments on pediments" = "escarpments",
         "plateaus on escarpments" = "escarpments")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#valleys
cor <- c("valley floors" = "flood plains",
         "basin-floor remnants on river valleys" = "basin-floor remnants",
         "^strike valleys$" = "flood plains",
         "terraces on valleys" = "terraces",
         "basin floor on river valleys" = "basin-floor remnants",
         "^mountain valleys$" = "flood plains",
         "cuesta valleys" = "hillslopes",
         "^river valleys$" = "flood plains",
         "valley flats" = "flood plains",
         "valley sides" = "flood plains",
         "river valleys on channels" = "drainageways",
         "river valleys on deltas" = "alluvial fans",
         "river valleys on natural levees" = "flood plains",
         "river valleys on washes" = "flood plains",
         "valleys on slumps" = "flood plains")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#mountain slopes
cor <- c("^mountains$" = "mountain slopes",
         "mountains on mountain slopes" = "mountain slopes",
         "^mountainsides$" = "mountain slopes",
         "mountain slopes on fan remnants" = "mountain slopes",
         "mountains on hills" = "mountain slopes",
         "mountains on hillsides or mountainsides" = "mountain slopes")

pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#moraines
cor <- c("ground moraines" = "hillslopes",
         "lateral moraines" = "hillslopes",
         "recessionial moraines" = "hillslopes",
         "^moraines$" = "hillslopes")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)


#miscellaneous
cor <- c("^bluffs$" = "escarpments",
         "^basin-floor remnants on mesas$" = "basin-floor remnants",
         "^buttes$" = "mesas",
         "^plateaus$" = "mesas",
         "pediments on mesas" = "mesas",
         "plateaus on cuestas" = "cuestas",
         "playa steps" = "alluvial flats",
         "^playa floors$" = "alluvial flats",
         "playas or alluvial flats" = "alluvial flats",
         "^playa rims$" = "alluvial flats",
         "basin-floor remnants on playa rims" = "basin-floor remnants",
         "marshes on playa floors" = "alluvial flats",
         "basin floors on sinkholes" = "basin-floor remnants",
         "^colluvial aprons$" = "hillslopes",
         "colluvial aprons on scarp slopes" = "escarpments",
         "slump blocks" = "hillslopes",
         "landslides" = "hillslopes",
         "^slumps$" = "hillslopes",
         "rotational slides" = "hillslopes",
         "mountains on slides" = "hillslopes",
         "^slides$" = "hillslopes",
         "canyon walls" = "hillslopes",
         "till plains" = "fan remnants",
         "^ridges$" = "fan remnants")
pts$landform <- as.factor(str_replace_all(pts$landform, cor))
levels(pts$landform)



summary(pts$landform)
levels(pts$landform)

#a few remain; classes with less than 3 points are removed in the model

#save

setwd
setwd("~/wy/data")
st_write(pts, "pedonsv5.shp", append=FALSE)

