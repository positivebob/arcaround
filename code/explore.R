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
# library(sp)
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
# library(rgdal)
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
#from csv
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20180327-4.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")


##### load input data #####
# latest working copy of NOAA National Database for Deep Sea Corals and Sponges
# setwd("C:/rworking/digs/indata")
# indata<-read.csv("DSCRTP_NatDB_20180327-4.csv", header = T)

##### filter data #####

# filtering out flagged records
filt <- indata %>%
  filter(Flag == "0")

# further filtering to focus on AOI 
x <- indata %>% 
  filter(
    Latitude > 29,
    Latitude < 52,
    Longitude < -110,
    Longitude > -132,
    DatasetID != 'MBARI'
    # ScientificName == 'Flabellum sp.'
  )

##### _____ leaflet map #####
# making a predined color palette based on a certain domain of values

pal <- colorFactor(
  palette = 'Dark2',
  domain = x$Flag
)

# making a leaflet map and coloring by the predefined palette above 
library(leaflet)
leaflet(x) %>% 
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


##### _____ creating spatialpointsdataframe #####
# defining coordinates
coords <- subset(x, select = c("Longitude", "Latitude"))

# making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

# creating SpatialPointsDataFrame from the subset. 
sdf<-SpatialPointsDataFrame(coords, x, 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                             match.ID = TRUE)

##### _____ setting up the arc to R bridge ##### 
library(arcgisbinding)
arc.check_product()

#### making it back to an ArcGIS  object ##### 
arcsdf <- arc.sp2data(sdf)

##### writing back out to a shape file #####
arc.write("c:\\data\\baselayers\\arc2Rtest", arcsdf)





