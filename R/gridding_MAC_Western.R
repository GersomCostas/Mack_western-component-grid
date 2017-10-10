################################################################################
################################################################################
## SCRIPT : ANNUAL EGG PRODUCTION FOR NEA MACKEREL. GRIDDING WESTERN AREA 
## version SEPTEMBER 2017
##
## Gersom Costas (Instituto Espanol de Oceanografia)
## gersom.costas@ieo.es
################################################################################
################################################################################
##DESCRIPTION 
# The  mackerel and western horse mackerel egg surveys (WGMEGG) cover spawning
# areas for NEA mackerel and Western horse mackerel stocks. 
#The north-east Atlantic shelf area is sub-divided  into two spawning areas: 
#western and southern areas.  The southern area for NEA mackerel is regarded as 
#being from 36 degrees N to 44 degrees N in the east and 45 degrees N in the west. 
#The western area for NEA mackerel is from 44 degrees N (45 degrees N in the West) 
#to 63 degrees N.
#The western area for Western horse mackerel includes the Cantabrian Sea and is 
#from 43 degrees N to 63 degrees N with same western boundary as for mackerel(ICES, 2016)
#The basic sampling unit is 0.5 degree longitude * 0.5 degree latitude, half of an ICES rectangle.

#######################################################################################################

##  SPATIAL GRID WESTERN AREA  

### Western Area:

#Latitude: lower or equal 68.5 degrees N   y bigger or equal 44 degrees N  
#Longitude: bigger or equal 0 degrees W    y lower or equal 23 degrees W 
 #######################################################################################################
# clear workspace
rm(list = ls())


##################
# load libraries
##################
if(!require(plyr)) install.packages("plyr") ; require(plyr)
if(!require(sp)) install.packages("sp") ; require(sp)
if(!require(maptools)) install.packages("maptools") ; require(maptools)
if(!require(dplyr)) install.packages("dplyr") ; require(dplyr)
if(!require(maps)) install.packages("maps") ; require(maps)
if(!require(mapdata)) install.packages("mapdata") ; require(mapdata)

#Importing the mackerel Western area rectangles which  have been sampled along survey temporal series. 
#Rectangle dimensions:  0.5? latitud x 0.5? longitude   
 RECT4 <- read.csv("data/SurvRects_4.csv")
 RECT4<-droplevels(RECT4) 
summary(RECT4)

##Importing  file of proportion in rectangles area that correspond sea(removing covered area by land from rectangle area)
## Rectangle area   (cos(RECT$lat*pi/180)*30*1853.2)*30*1853.2 
 
 RECTall <- read.csv("data/rect_searatio_all.csv")
  summary(RECTall)

##Matching covered area by land to western area rectangles. Mackerel Western component 

 RECT_west<-join(RECT4[ ,c("lat","lon","r1","R2","RECT","Area")],RECTall[ ,c("lat","lon","sea_ratio" ,"Area_minus_land" )],by=c("lat","lon")) 
 RECT_west$sea_ratio[is.na(RECT_west$sea_ratio)]<-1   #Problem with NA to be gridded. just in case
 RECT_west$Area_minus_land<-RECT_west$Area*RECT_west$sea_ratio
 RECT_west<-droplevels(RECT_west)
 
 RECT<-RECT_west
 
##Transform to Spatial pixel
 
 gridded(RECT_west) = ~lon+lat
 
##transform to Spatial Polygon
 
 RECTwest_p <- as(RECT_west, "SpatialPolygons")
 
##PLOTING 
 plot(RECTwest_p)
 slotNames(RECTwest_p)# slot names
  
## Original Rectangle names. 
row.names(RECTwest_p) 

 
## Use the spChFIDs() method in maptools to change the  IDs of at least one of your objects to disambiguate them 
 RECTwest_p <- spChFIDs(RECTwest_p, as.character(RECT_west@data[, 3]))
 row.names(RECTwest_p) 
 
## join spatialpoligonos ( step by step)  
 rownames(RECT)<-RECT$RECT
 RECT_p<-(RECTwest_p)

## Projection
proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")
RECT_p<-SpatialPolygonsDataFrame(RECT_p, data=RECT)
 
save.image("AEPM_grid_mack_Western.RData") 

### import land and ploting
par(mfrow=c(1,1))
europe<-map(database = "worldHires",  xlim = c(-27,6), ylim = c(41.5,68.5),fill=T,plot=F)
europe$names
IDs <- sapply(strsplit(europe$names, ":"), function(x) x[1])
europemap <- map2SpatialPolygons(europe,IDs=IDs, proj4string=CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84"))
proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")
plot(RECT_p, border="grey")
plot(europemap,add=T, fill=T,col="yellow", border="grey")



