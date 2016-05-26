#This script will serve to download and convert mtbs rasters into gridded products
#Steps:
#1) Download data from MTBS.gov
#The file names are in a format "<Fire ID>_<prefire image date>_<postfire image date>_dnbr6.tif" or
#"<Fire ID>_<postfire image date>_nbr6.tif"

#1.5) strip projection data from the metadata.txt file

#2) Project to LDAS projection
#MTBS projections are Albers Equal Area, Datum = NAD83, spheroid GRS80

#3) Intersect with LDAS 1/8° grid
#4) Identify pixels with the majority of each fire (note: most fires will not cover more than one pixel.  If they do, 

#-----------load packages and df------------------------------------
sapply(
  c("ggplot2","raster","sp","rgdal","reshape2","zoo","lubridate"),
  ndpkg
  )

load("~/Desktop/MTBS_fires/Fires/Data/yr_grid.rda")
grdr <- raster("~/Desktop/MTBS_fires/Fires/Data/grid.tif")

#-----------extract tifs from folders------------------------

setwd("~/Desktop/MTBS_fires/Fires/Data/fires_2008_2014")
dlist <- list.dirs(getwd(), full.names = FALSE)


#---------FUNCTION BEGIN FUNCTION BEGIN --------------------

for (i in dlist){
  rlist <- list.files(i, pattern = "nbr6.tif$", full.names = TRUE) #extract severity files



#-----------Change projection to Geographic-----------------
for (j in rlist){
  
  r <- assign(unlist(strsplit(j, "[.]"))[1], raster(j))
  sr <- "+proj=longlat +ellps=WGS84 +no_defs"
  pr <-projectRaster(r, crs = sr, method = "ngb")

#----------Extract date and location------------------------ 
    pr_dt <-as.Date(
    ifelse(strsplit(names(pr), "_")[[1]][3] == "nbr6",   #files are either dnbr6 or nbr6
           strsplit(names(pr), "_")[[1]][2],             #nbr6 files have one date, dnbr6 have two
           strsplit(names(pr), "_")[[1]][3]),            #getting the date from the file name. 
     format = '%Y%m%d'
  )
  mr <- month(pr_dt)
  yr <- year(pr_dt)
  
  pr[pr == 0] <- NA                       #removing "black" area                                        
  pr <- trim(pr)                          #and trimming NA values
  
  cr <- rasterToPoints(crop(grdr, pr))    #crop grdr to the firezone               

#---------Add severity class pixels to date/location-------    
  yr_grd[yr_grd$month %in% mr &
           yr_grd$year %in% yr &
           yr_grd$lat %in% cr[,"y"] &
           yr_grd$lon %in% cr[,"x"],"fire"] <-
           cr[, 3]                               #match the location and date

tr <- as.data.frame(table(values(pr)))
  for (k in tr$Var1){
    yr_grd[yr_grd$month %in% mr &
             yr_grd$year %in% yr &
             yr_grd$lat %in% cr[,"y"] &
             yr_grd$lon %in% cr[,"x"] , paste0("sev",k)] <- table(values(pr))[[k]]
  }
}

}



#--------------FUNCTION END FUNCTION END-------------------------




#----Making long/lat grid------------------------------------------------------------------
save(grd, file = "~/Desktop/MTBS_fires/Fires/Data/grid.rda")
load("~/Desktop/MTBS_fires/Fires/Data/grid.rda")
sr2 <- "+proj=longlat +ellps=WGS84 +no_defs"
coordinates(grd) <-  ~lon+lat                            #set the x/y coordinates to create spatial object
proj4string(grd) <-  CRS("+init=epsg:4326")              #set coordinate reference system
grd <- spTransform(grd, CRS(sr2))                        #convert to geographic projection
gridded(grd) <- TRUE                                     #specify spatial data as gridded

grdr <- raster(grd)                                      #convert to raster
projection(grdr) <- CRS(sr2)                             #set the CRS
plot(grdr)                                               #check it out
                                                         # V save as geoTIFF
writeRaster(grdr,"~/Desktop/MTBS_fires/Fires/Data/grid.tif", overwrite = TRUE)


yr_grd <- data.frame(year = all_var$year, month = all_var$month, lat = all_var$lat, lon = all_var$lon)
yr_grd$fire <- NA
yr_grd$sev1 <- NA
yr_grd$sev2 <- NA
yr_grd$sev3 <- NA
yr_grd$sev4 <- NA
yr_grd$sev5 <- NA
yr_grd$sev6 <- NA

save(yr_grd, file = "~/Desktop/MTBS_fires/Fires/Data/yr_grid.rda")
load("~/Desktop/MTBS_fires/Fires/Data/yr_grid.rda")
#------Test stuff------------------------------------------
r<- raster(j)

# Project Raster, using nearest neighbor
projected_raster <- projectRaster(r, crs = sr2, method = "ngb")
plot(projected_raster)


projected_raster[projected_raster == 0] <- NA
projected_raster <- trim(projected_raster)


table(values(projected_raster))
xmin(projected_raster)

ras_pr <- data.frame(rasterToPoints(projected_raster))
colnames(ras_pr) <- c("x", "y", "value")

grdr_pr <- data.frame(rasterToPoints(grdr))

head(grdr_pr)
head(ras_pr)


ggplot(ras_pr, aes(x = x, y = y, fill = value)) +geom_raster() 
min(ras_pr$x) 
max(ras_pr$x)
min(ras_pr$y) 
max(ras_pr$y)
test <- subset(grdr_pr, y >= 44.95922 & y <= 45.04253)
z <- projectRaster(from = projected_raster, to = grdr, method = "ngb")
z_pr <- data.frame(rasterToPoints(z))
colnames(z_pr) <- c("lon", "lat", "value")

ggplot(z_pr, aes(x = lon, y = lat, fill = value)) +geom_raster() +geom_hline(yintercept = z_pr[,2]) + geom_vline(xintercept = z_pr[,1])



ifelse(strsplit(names(projected_raster), "_")[[1]][3] == "nbr6",
       strsplit(names(projected_raster), "_")[[1]][2],
       strsplit(names(projected_raster),"_")[[1]][3])

#get lat lon to subset raster
latlon <-subset(grdr_pr,
                x >= xmin(projected_raster) - .125 &
                  x <= xmax(projected_raster) + .125 &
                  y >= ymin(projected_raster) - .125 &
                  y <= ymax(projected_raster) + .125)[,c(1:2)]

test <- subset(grdr,
               xmin == min(latlon$x) &
                 xmax == max(latlon$x) &
                 ymin == min(latlon$y) &
                 ymax == max(latlon$y))

rasterToPoints(crop(grdr, projected_raster))

yr_grd <- data.frame(year = all_var$year, month = all_var$month, lat = all_var$lat, lon = all_var$lon)
yr_grd$fire <- NA
yr_grd$sev1 <- NA
yr_grd$sev2 <- NA
yr_grd$sev3 <- NA
yr_grd$sev4 <- NA
yr_grd$sev5 <- NA
yr_grd$sev6 <- NA


table(values(projected_raster))[["6"]]
yr_grd$date < NULL

