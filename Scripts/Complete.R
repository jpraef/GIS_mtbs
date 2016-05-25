#This script will serve to download and convert mtbs rasters into gridded products
#Steps:
#1) Download data from MTBS.gov
#The file names are in a format "<Fire ID>_<prefire image date>_<postfire image date>_dnbr6.tif" or "<Fire ID>_<postfire image date>_nbr6.tif"

#1.5) strip projection data from the metadata.txt file

#2) Project to LDAS projection
#MTBS projections are Albers Equal Area, Datum = NAD83, spheroid GRS80

#3) Intersect with LDAS 1/8° grid
#4) Identify pixels with the majority of each fire (note: most fires will not cover more than one pixel.  If they do, 

#-----------load packages------------------------------------
sapply(c("ggplot2", "raster", "sp","rgdal","reshape2"), ndpkg)


#-----------extract tifs from folders------------------------

setwd("~/Desktop/MTBS_fires/Fires/Data/fires_2008_2014")
dlist <- list.dirs(getwd(), full.names = FALSE)
for (i in dlist){
  rlist <- list.files(i, pattern = "nbr6.tif$", full.names = TRUE)
}

#-----------Change projection to Geographic-----------------
for (j in rlist){
  
  r <- assign(unlist(strsplit(j, "[.]"))[1], raster(j))
  sr <- "+proj=longlat +ellps=WGS84 +no_defs"
  pr <-projectRaster(r, crs = sr, method = "ngb")
  z <- projectRaster(from = pr, to = grdr, method = "ngb")
    
}


#----Making long/lat grid------------------------------------------------------------------
save(grd, file = "~/Desktop/MTBS_fires/Fires/Data/grid.rda")
load("~/Desktop/MTBS_fires/Fires/Data/grid.rda")

coordinates(grd) <-  ~lon+lat                            #set the x/y coordinates to create spatial object
proj4string(grd) <-  CRS("+init=epsg:4326")              #set coordinate reference system
grd <- spTransform(grd, CRS(sr2))                        #convert to geographic projection
gridded(grd) <- TRUE                                     #specify spatial data as gridded

grdr <- raster(grd)                                      #convert to raster
projection(grdr) <- CRS(sr2)                             #set the CRS
plot(grdr)                                               #check it out
                                                         # V save as geoTIFF
writeRaster(grdr,"~/Desktop/MTBS_fires/Fires/Data/grid.tif", overwrite = TRUE)

#------Test stuff------------------------------------------
r<- raster(j)
sr2 <- "+proj=longlat +ellps=WGS84 +no_defs"
# Project Raster, using nearest neighbor
projected_raster <- projectRaster(r, crs = sr2, method = "ngb")
plot(projected_raster)
ras_pr <- rasterToPoints(projected_raster, Spatial = T)

z <- overlay(projected_raster, grdr, fun=function(x,y){return(x+y)})

ras_pr <- data.frame(rasterToPoints(projected_raster))
colnames(ras_pr) <- c("x", "y", "value")
grdr_pr <- data.frame(rasterToPoints(grdr))

head(grdr_pr)
head(ras_pr)

ggplot(ras_pr, aes(x = x, y = y, fill = value)) +geom_raster() 
z <- projectRaster(from = projected_raster, to = grdr, method = "ngb")
z_pr <- data.frame(rasterToPoints(z))
colnames(z_pr) <- c("lon", "lat", "value")

ggplot(z_pr, aes(x = lon, y = lat, fill = value)) +geom_raster() 
