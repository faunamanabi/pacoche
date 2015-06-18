

source("code/TEAM_code.R")
source("code/calendar.R")
machalilla.raw<-read.csv("Data/CT-PNM-2014.csv") # ojo falta la camara 3-12

# then you can use ggplot2 to plot that object
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(maptools)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal) 


long<-unique(machalilla.raw$longitude)
lati<-unique(machalilla.raw$latitude)
centercoord<-c(mean(subset(long, long<=1)),mean(unique(subset(lati, lati<=1))))
coordsubset<-subset(machalilla.raw,select = c(camera_trap,longitude,latitude,first_name_set_camera))

latpercam<-coordsubset %>%
  group_by(camera_trap) %>%
  summarise(latitude = median(latitude, na.rm=TRUE))

lonpercam<-coordsubset %>%
  group_by(camera_trap) %>%
  summarise(longitude = median(longitude, na.rm=TRUE))

fullcams<-dplyr::full_join(latpercam, lonpercam, by="camera_trap") #Hya solo 59 porque falta la que tomo fotos continuamente
fullcams2<-as.data.frame(fullcams)
coordinates(fullcams2) = c("longitude", "latitude") # convierte a spatial object
# writePointsShape (fullcams2, "shp/machalilla_cams") # escribe en shapefile




map <- get_map(location = centercoord, zoom = 11, 
               source = 'google', maptype = "terrain", color ="bw")
ggmap(map) #, fullpage = TRUE)



aoi.full <- readOGR(dsn = 'shp', 'machalilla') # read shape file
aoi <- spTransform(aoi.full, CRS=CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

PN_Machal<- fortify(aoi, region='name')

ap2<-ggplot(PN_Machal, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = "green") + coord_equal()


### add camera traps
g1<-  ggmap(map, extent = 'device') + geom_point(data = machalilla.raw, 
                                                 aes(x=longitude, y=latitude), 
                                                 colour = "red", size = I(2),  
                                                 alpha = 3/8, na.rm = TRUE) 
                                                      
g2 <- ap2 + geom_point(data = machalilla.raw, 
                     aes(x=longitude, y=latitude), 
                     colour = "red", size = I(2),  
                     alpha = 3/8, na.rm = TRUE) 
  
g3<-g2 + geom_text(data = machalilla.raw, 
          aes(longitude, latitude,label=(camera_trap), size=8),
          angle = 0,
          alpha = 1/2,
          na.rm = TRUE)   
  
overlay <- stat_density2d(
  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
  bins = 10, geom = "polygon",
  data = machalilla.raw) 

# con google
g3<- g1 + overlay + scale_fill_gradient(low = "blue", high = "red") + facet_wrap(~ genus, ncol = 5)
g3a<- g1 + overlay + scale_fill_gradient(low = "blue", high = "red") 

# sin google
g4<- g2 + overlay + scale_fill_gradient(low = "blue", high = "red") + facet_wrap(~ genus, ncol = 6)













############################################
machalilla.raw<-read.csv("Data/CT-PNM-2014.csv")
#
#############################
#### date fix
#############################

# unique(year(machalilla.raw$camera_trap_start_time))
machalilla.raw$photo_date2<-as.Date(as.character(machalilla.raw$photo_date), "%d-%b-%Y")

machalilla.raw$Sampling.Period<-2014
machalilla.raw$binomial<-paste(machalilla.raw$genus, machalilla.raw$specise, sep = " ")

#############################
# translate months
#############################
# 
# meses<-as.data.frame(t(matrix(c("ago","aug","dic","dec","ene","jan","abr","apr"),nrow = 2,ncol = 4)))
# machalilla.raw$camera_trap_start_time2<-NA
# machalilla.raw$camera_trap_end_time2<-NA
# for (i in 1:4){  
# # get month
#   chkmes<-substr(as.character(machalilla.raw$photo_date), start=4, stop=6)
#   chkmes2<-substr(as.character(machalilla.raw$camera_trap_start_time), start=4, stop=6)
#   chkmes3<-substr(as.character(machalilla.raw$camera_trap_end_time), start=4, stop=6)
#   # chane month
#   agoind<-which(chkmes == meses[i,1])
#   agoind2<-which(chkmes2 == meses[i,1])
#   agoind3<-which(chkmes3 == meses[i,1])
#   # machalilla.raw$photo_date[agoind]
#   machalilla.raw$photo_date2[agoind]<-as.Date(gsub(as.character(meses[i,1]), 
#                                                      as.character(meses[i,2]), 
#                                                      machalilla.raw$photo_date[agoind]), "%d-%b-%Y")
#   
#   machalilla.raw$camera_trap_start_time2[agoind2]<-as.Date(gsub(as.character(meses[i,1]), 
#                                                    as.character(meses[i,2]), 
#                                                    machalilla.raw$camera_trap_start_time[agoind2]), "%d-%b-%Y")
#   
#   machalilla.raw$camera_trap_end_time2[agoind3]<-as.Date(gsub(as.character(meses[i,1]), 
#                                                                as.character(meses[i,2]), 
#                                                                machalilla.raw$camera_trap_end_time[agoind3]), "%d-%b-%Y")
# }
# 
# # tochk
# # unique(machalilla.raw$photo_date2)


machalilla.raw$camera_trap_start_date<-as.Date(substr(as.character(machalilla.raw$camera_trap_start_time), start=1, stop=11), "%d-%b-%Y")
machalilla.raw$camera_trap_end_date<-as.Date(substr(as.character(machalilla.raw$camera_trap_end_time), start=1, stop=11), "%d-%b-%Y")

#################################
# Fix 2011 problem
# Two cameras have date wrong
# CT-PNM-1-7  and  CT-PNM-3-10
# Fix start date manually
################################
# identify the problem
# index_problem_2011<-which(machalilla.raw$camera_trap_start_date == "2011-11-11")
# problem_2011<-machalilla.raw[index_problem_2011,]
# unique(problem_2011$camera_trap)



# fix 1-7 just year
index_1_07<-which(machalilla.raw$camera_trap == "CT-PNM-1-7")
cam_1_07<-machalilla.raw[index_1_07, ] # make data frame using the cam
machalilla.raw<-machalilla.raw[-index_1_07 ,] # remove the cam
cam_1_07$camera_trap_start_date<-as.Date("2014-09-23", format="%Y-%m-%d")
cam_1_07<-cam_1_07[-1,] # borra el primero porblematic 2011
cam_1_07$photo_type[1]<-"Start" # fix start
cam_1_07$camera_trap_end_date<-as.Date("2014-11-06", format="%Y-%m-%d")

# fix 1-10  # add 31 days since data setup pickup en data 12 and beyond
index_1_10<-which(machalilla.raw$camera_trap == "CT-PNM-1-10")
cam_1_10<-machalilla.raw[index_1_10, ]
machalilla.raw<-machalilla.raw[-index_1_10 ,]

cam_1_10$camera_trap_start_date<-as.Date("2014-09-23", format="%Y-%m-%d") 
cam_1_10$camera_trap_end_date<-as.Date("2014-11-05", format="%Y-%m-%d")
cam_1_10$photo_date2[c(12:875)]<-cam_1_10$photo_date2[c(12:875)] +31
# delete problematic data
cam_1_10<-cam_1_10[-24,]
cam_1_10<-cam_1_10[-25,]

# fix 3-10  # restar 30 dias
index_3_10<-which(machalilla.raw$camera_trap == "CT-PNM-3-10")
cam_3_10<-machalilla.raw[index_3_10, ]
machalilla.raw<-machalilla.raw[-index_3_10 ,]

cam_3_10$camera_trap_start_date<-as.Date("2015-01-27", format="%Y-%m-%d") 
cam_3_10$camera_trap_end_date<-as.Date("2015-03-15", format="%Y-%m-%d")
# borra los primeros 30 problematicos con fecha 2011
cam_3_10<-cam_3_10[-c(1:30),]
cam_3_10$photo_date2<-cam_3_10$photo_date2  - 30


# fix 3-07 # add difference of 365 days
index_3_07<-which(machalilla.raw$camera_trap == "CT-PNM-3-07")
cam_3_07<-machalilla.raw[index_3_07, ]

machalilla.raw<-machalilla.raw[- index_3_07 ,]
cam_3_07$camera_trap_start_date<-as.Date("2015-01-27", format="%Y-%m-%d")
cam_3_07$camera_trap_end_date<-as.Date("2015-03-11", format="%Y-%m-%d")
cam_3_07$photo_date2<- cam_3_07$photo_date2 + 365

### remove from machalilla.raw
# machalilla.raw<-machalilla.raw[-index_1_07 ,]
# machalilla.raw<-machalilla.raw[-index_3_10 ,]
# machalilla.raw<-machalilla.raw[-index_3_07 ,]

#### Add corrected
machalilla.raw<-rbind(machalilla.raw, cam_1_07, cam_1_10, cam_3_10, cam_3_07)
# machalilla.raw<-rbind(machalilla.raw, cam_1_10)
# machalilla.raw<-rbind(machalilla.raw, cam_3_10)
# machalilla.raw<-rbind(machalilla.raw, cam_3_07)

#  problematic ?
which(machalilla.raw$photo_date2 == "2011-11-11")
# which(machalilla.raw$camera_trap == "CT-PNM-1-10")


########## extract yr and month
machalilla.raw$year<-year(machalilla.raw$photo_date2)
machalilla.raw$month<-month(machalilla.raw$photo_date2)
# problem?
which(machalilla.raw$year == "2011")


####################################
# make photo calendar type
####################################

f.calendar.yr(dataset = machalilla.raw, yr_toplot = 1)
f.calendar.yr(dataset = machalilla.raw, yr_toplot = 2)


####################################
# make mat per species
####################################

mat.per.sp<-f.matrix.creator2(data = machalilla.raw,year = 2014)
sp.names<-names(mat.per.sp) # species names

# counting how many (total) records per species by all days
cont.per.sp<-data.frame(row.names = sp.names)
for (i in 1:length(mat.per.sp)){
  cont.per.sp[i,1]<-sum(apply(as.data.frame(mat.per.sp [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
}
cont.per.sp

########################
### see mat as image
########################

image(t(mat.per.sp[[2]]),col =  topo.colors(5)) # change number to see anoter species
title(main = as.character(sp.names[2]), font.main = 4)


########################
### shrink to 15
########################

library(unmarked)

E_barbara_15<-f.shrink.matrix.to15(matrix = mat.per.sp[[6]])


########################
### make unmarked object 
########################

E_barbara_UMF <- unmarkedFrameOccu(E_barbara_15)
plot(E_barbara_UMF, panels=1)
# add some  covariates
siteCovs(E_barbara_UMF) <- cam.and.covs

#######################
## occu models 
#######################

#  covariates of detection and occupancy in that order.
fm0 <- occu(~ 1 ~ 1, E_barbara_UMF) 
fm1 <- occu(~ 1 ~ elev, E_barbara_UMF)
fm2 <- occu(~ 1 ~ slope, E_barbara_UMF)
fm3 <- occu(~ 1 ~ dist_rd, E_barbara_UMF)
fm4 <- occu(~ elev ~ elev, E_barbara_UMF)
fm5 <- occu(~ elev ~ slope, E_barbara_UMF)
fm6 <- occu(~ elev ~ dist_rd, E_barbara_UMF)

# put the names of each model
models <- fitList(
  'p(.)psi(.)' = fm0,
  'p(.)psi(elev)' = fm1,
  'p(.)psi(slope)' = fm2,
  'p(.)psi(dist_rd)' = fm3,
  'p(elev)psi(elev)' = fm4,
  'p(elev)psi(slope)' = fm5,
  'p(elev)psi(dist_rd)' = fm6)

ms <- modSel(models)
ms


confint(fm0, type='det', method = 'normal')
confint(fm0, type='det', method = 'profile')

# estimate detection effect at obsvars=0.5
(lc <- linearComb(fm0['det'],c(1,0.5)))

# transform this to probability (0 to 1) scale and get confidence limits
(btlc <- backTransform(lc))
confint(btlc, level = 0.9)

# Empirical Bayes estimates of proportion of sites occupied
re <- ranef(fm0)
sum(bup(re, stat="mode"))






##########################
## get just animals
##########################
data<-subset(machalilla.raw, photo_type=="Animal")


data_animal<-tbl_df(filter(machalilla.raw, year==2014))
counts_cam_date <- count(data_animal, camera_trap, photo_date)
colnames(counts_cam_date)<-c("camera_trap", "dates",  "counts")
counts_cam_date$dates<-as.Date(counts_cam_date$dates,format = "%d-%b-%Y")
gg.calendar(counts_cam_date)


# Delete 2011 provisional
index<-which(data$camera_trap_start_date == "2011-11-11")
cuales<-data[index,]
data<-data[-index,]



# to do
yr2014<-f.matrix.creator2(data = machalilla.raw, year = 2014) #### fix names in function

##########################################
##### export camera points
##########################################





cam_point<-unique(machalilla.raw$camera_array)

coordinates (machalilla.raw$longitude, machalilla.raw$latitude)
