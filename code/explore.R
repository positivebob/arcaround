##### header #####
# Author: Robert McGuinn, positivebob@gmail.com
# Start Date: 20180405
##### install packages ##### 
#install.packages("pacman")
# library(pacman)
# #pacman::p_load(captioner, bundesligR)
# library(captioner, bundesligR)
# #install.packages("beanplot")
# library(beanplot)
# #install.packages("stringr")
# library(stringr)
# #install.packages("knitr")
library(knitr)
# #install.packages("tidyr")
# library(tidyr)
# #install.packages("sp")
library(sp)
# #install.packages("maptools")
# library(maptools)
# #install.packages("maps")
# library(maps)
# #install.packages("reshape")
# library(reshape)
# #install.packages("reshape2")
# library(reshape2)
# #install.packages("psych")
# library(psych)
# #install.packages("ggplot2")
library(ggplot2)
# #install.packages("data.table")
# library(data.table)
# #install.packages("dplyr")
library(dplyr)
# #install.packages("car")
# library(car)
# #install.packages("gdata")
# library(gdata)
# #install.packages("digest")
# library(digest)
# #install.packages("rgdal")
library(rgdal)
# #install.packages("ggmap")
# library(ggmap)
# #install.packages("rerddap")
# library(rerddap)
# #install.packages("raster")
# library(raster)
# #install.packages("rworldxtra")
# library(rworldxtra)
# #install.packages("ggrepel")
# library(ggrepel)
# #install.packages("xtable")
# library(xtable)
# library(taxize)
# library(rgdal)
# library(dplyr)
# #install.packages("tidyverse")
# library(tidyverse)
# #install.packages("extrafont")
library(leaflet)
library(extrafont)
library(RColorBrewer)
# library(googlesheets)
# library(googledrive)
library(arcgisbinding)

##### load input data #####
#latest working copy of NOAA National Database for Deep Sea Corals and Sponges
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20180327-4.csv", header = T)

##### filter data #####

# filtering out flagged records
filt <- indata %>%
  filter(Flag == "0")

# further filtering to focus on specific AOIs
geo <- filt %>% 
  filter(
    Latitude > 29,
    Latitude < 52,
    Longitude < -110,
    Longitude > -132,
  )

mbari <- geo %>% 
  filter(
    DatasetID == 'MBARI'
  )

no_mbari <- geo %>% 
  filter(
    DatasetID != 'MBARI',
    is.na(SamplingEquipment) == T,
    EndLatitude == '-999'
  )

lines <- geo %>% 
  filter(
     EndLatitude != '-999'
  )

lines$depthdiff <- lines$EndLatitude - lines$StartLatitude
plot(lines$depthdiff)


table(factor(mbari$SamplingEquipment), useNA = 'always')
table(factor(no_mbari$SamplingEquipment), useNA = 'always')
table(factor(no_mbari$DepthMethod), useNA = 'always')
table(factor(no_mbari$RecordType), useNA = 'always')
table(factor(lines$EndLatitude), useNA = 'always')
table(factor(lines$EndLatitude), useNA = 'always')
table(factor(lines$DataProvider), useNA = 'always')
table(factor(lines$RecordType), useNA = 'always')


##### _____ creating spatialpointsdataframe from CSV#####
# defining coordinates
x <- geo
coords <- subset(x, select = c("Longitude", "Latitude"))

# making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

# creating SpatialPointsDataFrame from the subset. 
sdf<-SpatialPointsDataFrame(coords, x, 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                             match.ID = TRUE)

##### _____ transferring sp file to arc object ##### 
library(arcgisbinding)
arc.check_product()

#### turning back into ArcGIS object ##### 
arcsdf <- arc.sp2data(sdf)

##### writing back out to a feature class #####
arc.write("c:\\data\\aptx\\20180405_0_West_Coast_Science_Planning_Maps_RPMcGuinn\\mmmh.gdb\\yo_what", 
          arcsdf)

# C:\data\aptx\20180405_0_West_Coast_Science_Planning_Maps_RPMcGuinn


##### _____ leaflet map #####
# setting up the proper input data
x <- no_mbari

# making a predined color palette based on a certain domain of values

pal <- colorFactor(
  palette = 'Dark2',
  domain = no_mbari$Flag
)

# making a leaflet map and coloring by the predefined palette above 


library(leaflet)
leaflet(no_mbari) %>% 
  addProviderTiles("Esri.OceanBasemap") %>% 
  addCircleMarkers(radius=5, 
                   weight=0, 
                   fillOpacity=1, 
                   color = ~pal(Flag),
                   popup = paste(
                     "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                     "<b><em>","FlagReason:","</b></em>", x$FlagReason, "<br>",
                     "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                     "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                     "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                     "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                     "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                     "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                     "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                     "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                     "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                     "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                     "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                     "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                     "<b><em>","Image:","</b></em>",x$ImageURL)
                   
  )





##### working on mapping lines ##### 
##### make shapefile lines from xy coordinates and mapping with leaflet and points #####
setwd("C:/rworking/digs/indata")

x <- "DSCRTP_NatDB_OceanaSubmission_SouthernCA2016_points"
d2 <- read.csv(paste(x,".csv",sep=''),header = T)

x <- "DSCRTP_NatDB_OceanaSubmission_SouthernCA2016_lines"
d <- read.csv(paste(x,".csv",sep=''),header = T)

begin.coord <- data.frame(lon=d$StartLongitude, lat=d$StartLatitude)
end.coord <- data.frame(lon=d$EndLongitude, lat=d$EndLatitude)

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}
lines<-SpatialLines(l)
class(lines)

##### map lines and points using leaflet #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addPolylines(m, data=lines,
                  fillColor= "green"
                  
)
m <- addCircleMarkers(m, data=d2, 
                      radius=1, 
                      weight=0,
                      lng = d2$LongitudeInDD, 
                      lat = d2$LatitudeInDD,
                      fillColor= "red", 
                      fillOpacity=1,
                      popup = d$ScientificName
)
m

##### getting line segments and calculatng length #####
# Getting just the records with not '-999' in the start and end positions.  
d <- geo %>%
  filter(
    StartLatitude != "-999", 
    StartLongitude != "-999", 
    EndLatitude != "-999", 
    EndLongitude != "-999"
  )

# #checking
# d <- head(d, n = 2000)
# length(d$CatalogNumber)

d2 <- d #%>%
#filter(StartLatitude != "-999")
coordinates(d2) <- c("Longitude", "Latitude")
proj4string(d2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# creating spatial lines
begin.coord <- data.frame(lon=d2$StartLongitude, lat=d2$StartLatitude)
end.coord <- data.frame(lon=d2$EndLongitude, lat=d2$EndLatitude)

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}
lines<-SpatialLines(l)
class(lines)

# this step turns the lines file into a SpatialLinesDataFrame for use with the writeOGR function
lines2 <- SpatialLinesDataFrame(lines, d2@data)
# class(lines2)
# names(lines2)

# adding a length variable
lines2$length <- SpatialLinesLengths(lines, longlat = T)
proj4string(lines2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# lines2
# names(lines2)

setwd("C:/rworking/digs/outdata")
writeOGR(lines2, dsn="what", 
         layer= "what", 
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"), 
         overwrite_layer = T)

# summary(lines2$length)
