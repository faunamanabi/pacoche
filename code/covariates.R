

# then you can use ggplot2 to plot that object
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(maptools)
library(rasterVis)
library(raster)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal) 
library(dismo)
# library(sdm)
library(spatstat)

machalilla.raw<-read.csv("Data/CT-PNM-2014.csv") # ojo falta la camara 3-12

long<-unique(machalilla.raw$longitude)
lati<-unique(machalilla.raw$latitude)
centercoord<-c(mean(subset(long, long<=1)),mean(unique(subset(lati, lati<=1))))
coordsubset<-subset(machalilla.raw,select = c(camera_trap,longitude,latitude,first_name_set_camera))

# get elevation
elevation<-getData(name = "SRTM",lon=centercoord[1], lat=centercoord[2])


cam.cords<-as.data.frame(distinct(coordsubset))
coordinates(cam.cords) <- ~longitude+latitude #make sppatial data fram
geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #def cord
proj4string(cam.cords)<-geo # set cords

e<-extent (-80.9, -80.5, -1.75,-1.35)
elevation.crop<-crop(elevation, e)

plot(elevation.crop)
plot(cam.cords, add=T)
slope<-terrain(elevation.crop, opt='slope', unit='degrees', neighbors=4)


cam.cords.sp<-SpatialPoints(cam.cords)
proj4string(cam.cords.sp)<-geo 
# etract values
elev.ovr <- extract(elevation.crop, cam.cords, method='bilinear')
slope.ovr <- extract(slope, cam.cords, method='bilinear')

# add to table
cam.cords$elev<-elev.ovr
cam.cords$slope<-slope.ovr

############################
# road
############################



roads <- readShapeSpatial("shp\\machalilla_roads.shp")
proj4string(roads)<-geo #put georef
xrange=c(-80.9, -80.5)
yrange=c(-1.75,-1.35)
window<-owin(xrange, yrange)
roads.ow<-as(roads, "SpatialLines",ext = window) # make SpatialLines


roads.psp<-as.psp(roads.ow)

road.density<-density.psp(roads.psp,edge = T)
road.density.im<-as.im(road.density)

roadpol <- readShapeSpatial("shp\\machalilla_roadsclip.shp")
names(roadpol)<-"dist_rd"
proj4string(roadpol)<-geo
dist_rd<-over(x = cam.cords, y = roadpol)
# add to table
cam.cords$dist_rd<-as.numeric(dist_rd[,1])

# roadpol.ow<-as(as(roadpol, "SpatialPolygons"), "owin") # make owin



#####################################################
## Deforestation
####################################################

defore<-raster("Data\\machalilla_thresholded_50_annual.tif",layer=13)
defore.crop<-crop(defore, e) # cut by the extent
m <- c(0, 2, NA,  4, 6, NA) # matriz de reclasificacion
rclmat <- matrix(m, ncol=3, byrow=TRUE) # put as mat
deforestado <- reclassify(defore.crop, rclmat)
# plot(deforestado, col="red", add=T)
dist.def <- distance(deforestado) # make distance from not raster category 3 to fill NAs
writeRaster(dist.def, filename="dist_def.tif", format="GTiff", overwrite=TRUE)
plot(dist.def)
plot(cam.cords, add=T)
# plot(deforestado, col="red", add=T)
# etract values
dist.def.ovr <- extract(dist.def, cam.cords, method='bilinear')
index<-which(is.na(dist.def.ovr)) # detect NA. Means cam is in deforested
dist.def.ovr [index]<-0 # camera in distance cero to deforested

cam.cords$dist_def<-dist.def.ovr

# falta deforestacion, tipo de habitat (bosque humedo seco)
cam.and.covs<-as.data.frame(cam.cords)

