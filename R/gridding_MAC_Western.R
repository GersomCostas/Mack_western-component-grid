############################################
## SCRIPT : ANNUAL EGG PRODUCTION FOR NEA MACKEREL. GRIDDING WESTERN AREA 
## version August 2019
##
## Gersom Costas 
## gersom.costas@ieo.es
#
############################################
#
##DESCRIPTION
#
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

############################################

##  SPATIAL GRID WESTERN AREA  

### Western Area:

#Latitude: lower or equal 68.5 degrees N   y bigger or equal 44 degrees N  
#Longitude: bigger or equal 0 degrees W    y lower or equal 23 degrees W 

############################################


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
if(!require(tidyr)) install.packages("tidyr") ; require(tidyr)

#Importing the mackerel Western area rectangles which  have been sampled along survey temporal series. 
#Rectangle dimensions:  0.5 degree latitud x 0.5 degree longitude 

RECT4 <- read.csv("data/SurvRects_4.csv")

RECT4<-droplevels(RECT4) 

summary(RECT4)

# Importing  file of proportion in rectangles area that correspond sea(removing covered area by land from rectangle area)
# Rectangle area   (cos(RECT$lat*pi/180)*30*1853.2)*30*1853.2 
 
RECTall <- read.csv("data/rect_searatio_all.csv")

summary(RECTall)


# Matching covered area by land to western area rectangles. Mackerel Western component 

RECT_west<-join(RECT4[ ,c("lat","lon","r1","R2","RECT","Area")],RECTall[ ,c("lat","lon","sea_ratio" ,"Area_minus_land" )],by=c("lat","lon"))
 
RECT_west$sea_ratio[is.na(RECT_west$sea_ratio)]<-1   #Problem with NA to be gridded. just in case
 
RECT_west$Area_minus_land<-RECT_west$Area*RECT_west$sea_ratio
 
RECT_west<-droplevels(RECT_west)
 
RECT_west_df<-RECT_west
 

# standardised name for overlap function

RECT<-RECT_west

# Covert to Spatial pixel
 
gridded(RECT_west) = ~lon+lat
 

# Convert to Spatial Polygon
 
RECT_p <- as(RECT_west, "SpatialPolygons")
 

# PLOTING 

plot(RECT_p)

slotNames(RECT_p)# slot names
 
 
# Original Rectangle names

row.names(RECT_p) 

 
# Use the spChFIDs() method in maptools to change the  IDs of at least one of your objects to disambiguate them 

RECT_p <- spChFIDs(RECT_p, as.character(RECT_west@data[, "RECT"]))

row.names(RECT_p) 
 

## join spatialpoligonos ( step by step)

 rownames(RECT_west_df)<-RECT_west_df$RECT

 
## Projection
 
proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")

RECT_p<-SpatialPolygonsDataFrame(RECT_p, data=RECT_west_df)

#View grid

plot(RECT_p)

# ploting land + grid



png("images/western_survey_grid.png",
    width = 5, height = 7, units = "in", pointsize = 10,
    bg = "white", res = 800,
    type = "cairo-png")

par(mar=c(2,2,2,2) + 0.1)

map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, type="n")

plot(RECT_p, border="grey",  xlim = c(-23,0.5), ylim = c(43,68.5))

degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)

degAxis(1, at = c(seq(-23,1, by=3)), cex.axis = 0.5, las=2)

map(database = "worldHires",  xlim = c(-23,0.5), ylim = c(43,68.5),fill=T, col="darkgreen",add=T)

title("Mackerel western area grid")

box()

dev.off()




rm(RECT4, RECT_west, RECTall)


#save.image("AEPM_grid_mack_Western.RData") 


####################################





####################################



###updating news rectangles in east part of western component areain 2019

###storing files used until 2016 suveys. 

RECT_p.former<-RECT_p
RECT.former<-RECT

#Adding the area front Norway (2019) to traditional  the mackerel Western area ( sampled area along survey temporal series)  
#Rectangle dimensions:  0.5 degree latitud x 0.5 degree longitude 

# Importing  file of proportion in rectangles area that correspond sea(removing covered area by land from rectangle area)
# Rectangle area   (cos(RECT$lat*pi/180)*30*1853.2)*30*1853.2 


RECTall_2019<-read.csv("data/Stat_rectangles_2019_all_rev.csv")


RECT_NOR<-filter(RECTall_2019,lat>=60&lat<=65&lon>=0&lon<=5.5)%>% droplevels() ##part of north sea componennet added: front Norway

RECT_west2019<-bind_rows(select(RECT,lat, lon, RECT),select(RECT_NOR,lat, lon, RECT))%>%
  distinct()%>%
  left_join(RECTall_2019)%>%
  select(-radian,-cos)%>%
  mutate(sea_ratio=replace_na(sea_ratio,1),RECT=as.factor(as.character(RECT)) )%>%
  rowwise()%>%
  mutate( Area_minus_land=Area*sea_ratio, Year=2019)%>%
  droplevels()%>%
  data.frame

summary(RECT_west2019)



###dataframe files

RECT_west2019df<-RECT_west2019

# standardised name for overlap function

RECT<-RECT_west2019

# Covert to Spatial pixel

gridded(RECT_west2019) = ~lon+lat


# Convert to Spatial Polygon

RECT_p <- as(RECT_west2019, "SpatialPolygons")


# PLOTING 

plot(RECT_p)

slotNames(RECT_p)# slot names


# Original Rectangle names

row.names(RECT_p) 


# Use the spChFIDs() method in maptools to change the  IDs of at least one of your objects to disambiguate them 

RECT_p <- spChFIDs(RECT_p, as.character(RECT_west2019@data[, "RECT"]))

row.names(RECT_p) 


## join spatialpoligonos ( step by step)

rownames(RECT_west2019df)<-RECT_west2019df$RECT


## Projection

proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")

RECT_p<-SpatialPolygonsDataFrame(RECT_p, data=RECT_west2019df)

#View grid

plot(RECT_p)



# ploting land + grid

png("images/western_survey_grid_2019.png",
    width = 5, height = 7, units = "in", pointsize = 10,
    bg = "white", res = 800,
    type = "cairo-png")

par(mar=c(2,2,2,2) + 0.1)

map(database = "worldHires",  xlim = c(-23,5), ylim = c(43,68.5),fill=T, type="n")

plot(RECT_p, border="grey",  xlim = c(-23,5), ylim = c(43,68.5))

degAxis(2, at = c(seq(44,69, by=3)),cex.axis = 0.5,las=2)

degAxis(1, at = c(seq(-23,4, by=3)), cex.axis = 0.5, las=2)

map(database = "worldHires",  xlim = c(-23,5), ylim = c(43,68.5),fill=T, col="darkgreen",add=T)

title("Mackerel western area grid")

box()

dev.off()


rm( RECT_west2019)


save.image("AEPM_grid_mack_Western.RData") 
