#This script will serve to download and convert mtbs rasters into gridded products
#Steps:
#1) Download data from MTBS.gov
#The file names are in a format "<Fire ID>_<prefire image date>_<postfire image date>_dnbr6.tif" or "<Fire ID>_<postfire image date>_nbr6.tif"

#1.5) strip projection data from the metadata.txt file

#2) Project to LDAS projection
#MTBS projections are Albers Equal Area, Datum = NAD83, spheroid GRS80

#3) Intersect with LDAS 1/8° grid
#4) Identify pixels with the majority of each fire (note: most fires will not cover more than one pixel.  If they do, 



setwd("~/Desktop/MTBS_fires/Fires/Data/fires_2008_2014")
dlist <- list.dirs(getwd(), full.names = FALSE)
for (i in dlist){
  rlist <- list.files(i, pattern = "nbr6.tif$", full.names = TRUE)
  for (j in rlist){
    plot(assign(unlist(strsplit(j, "[.]"))[1], raster(j)))
    
    
}

}

tlist <- list(ls())

r <- raster("/Users/joe/Desktop/MTBS_fires/Fires/Data/fires_2008_2014/OR4474212096020110801/or4474212096020110801_20110723_20110808_dnbr6.tif")
sr <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 

# Project Raster, using nearest neighbor
projected_raster <- projectRaster(r, crs = sr, method = "ngb")
plot(projected_raster)
ras_pr <- rasterToPoints(projected_raster, Spatial = T)
proj4string(ras_pr)

ras_pr@data <- data.frame(ras_pr@data, long=coordinates(ras_pr)[,1],lat=coordinates(ras_pr)[,2])

