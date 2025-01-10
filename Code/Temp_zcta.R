# Extracts temperature data from NCAR's NC4 files, averages values for each ZCTA
# Need census  shapefile for the state
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html


# Load packages
library(stringr)
library(rgdal)
library(ncdf4)
library(proj4)
library(dplyr)
library(sf)
library(ggplot2)
library(writexl)
library(sf)
library(tidyverse)

# Remove all items in the working environment
rm(list = ls())

# Set working directory
setwd("PATH/")


#change start & end dates here : 
start_date <- as.Date("1990/01/01")
end_date <- as.Date("2017/12/31")
date_set <- seq.Date(start_date, end_date, by = "1 day")

#EDIT
ogr_file <- "PATH/NAME.shp" 


# Plot state zipcode shapefile
zip.shp = readOGR (ogr_file)

ggplot()+ geom_polygon(data=zip.shp, aes(long, lat, group = group),
                       colour = alpha("darkred", 1/2), size = 0.7,
                       fill = 'skyblue', alpha = .3)


###############################
### Link GRID to zip code   ###
### (Only need to run once) ###
###############################
  
#Extract lat/lon information for the grid on the first day

#EDIT
filestr <- paste0("Path_to_NC4files/uhi_epi_", format(date_set[1], "%Y%m_%d"), ".nc4")
ncin <- nc_open(filestr)
lat <- ncvar_get(ncin, "lat") # degree_north
lon <- ncvar_get(ncin, "lon") # degree_east

#Create index for all grid cells and convert to sf
n.x = nrow (lat)
n.y = ncol (lat)
gridpoints = data.frame(ID = 1:(n.x*n.y), #add ID variable
                        rowID = rep(1:n.x, n.y),
                        colID = rep(1:n.y, each = n.x),
                        lat=c(lat), lon=c(lon))
gridpoints$lon<- gridpoints$lon-360 # convert to degree_west

#Loop through Zipcodes
Grid2ZIP = vector ("list", nrow (zip.shp))

for (i in 1:nrow(zip.shp)){
    
    zip.i = zip.shp[i,]
    print (paste0("i=",i,", zipcode=",zip.i$ZCTA5CE10))
    
    # Restrict grids by polygon bounding box
    gridpoints.i = subset(gridpoints,
                          lat>zip.i@bbox[2,1]-0.1 & lat<zip.i@bbox[2,2]+0.1 &
                            lon>zip.i@bbox[1,1]-0.1 & lon<zip.i@bbox[1,2]+0.1)
    
    match.point = over(SpatialPoints(cbind (gridpoints.i$lon, gridpoints.i$lat)),
                       SpatialPolygons(zip.i@polygons) )
    zippoints.i = gridpoints.i[!is.na(match.point),]
    
    if (nrow(zippoints.i) ==0){
      # if there is no grid in the zipcode area, find the closest grid cell
      centroid.i = (zip.i@polygons)[[1]]@labpt
      dist.i = sweep(cbind(gridpoints.i$lon, gridpoints.i$lat), 2,centroid.i, "-")
      zippoints.i = gridpoints.i[which.min (apply(dist.i^2, 1, sum)),]
    }
    
    Grid2ZIP[[i]] = data.frame (zipcode = zip.shp@data[i,"ZCTA5CE10"], 
                                 zippoints.i)
}#End of Zipcodes

#Combine into an index data set
Grid2ZIP = do.call ("rbind", Grid2ZIP)

plot (zip.shp)
lines (Grid2ZIP$lon, Grid2ZIP$lat, pch = ".", col = 4, type = "p")
write.csv (Grid2ZIP, file = "temp_files/Grid2ZIP_state.csv", row.names = FALSE)


######################
# Now go through days #
######################
rm (list = ls())
start_date <- as.Date("1990/1/1") 
end_date <- as.Date("2017/12/31") 
date_set <- seq.Date(start_date, end_date, by = "1 day")

ogr_file <- "PATH/NAME.shp"


zip.shp = readOGR (ogr_file)
shp <- read_sf(ogr_file)

Grid2ZIP = read.csv ("temp_files/Grid2ZIP_state.csv")


ZIP_data_Tmax = vector ("list",length (date_set) )
ZIP_data_Tmax_sd = vector ("list",length (date_set) )
  
for (i in seq_along(date_set)) {
  
  date1 <- date_set[i]
  #EDIT
  filestr <- paste0("PATH/uhi_epi_", format(date1, "%Y%m_%d"),".nc4")
  print(filestr)

  ## Limits grids within each zipcode area, extracts var for each day for each gridpoint.
  
  ncin <- nc_open(filestr)

#####
#####
#####
#Extract variables by day
  
  ### Tmax (t2_max_cor)

  Tmax.array  <- ncvar_get(ncin,"t2_max_cor")
    
  dim(Tmax.array)
  
  #convert data into a vector, merge with ZIPtoGrid dataset, compute mean by Zip
  Tmax.vector = c(Tmax.array)
  Grid2ZIP$value = Tmax.vector[Grid2ZIP$ID]
  Grid2ZIP$value = (Grid2ZIP$value-273.15)*1.8+32 #Convert K to F
  ZIP_avg = tapply (Grid2ZIP$value, Grid2ZIP$zipcode, mean, na.rm = TRUE)
  ZIP_sd = tapply (Grid2ZIP$value, Grid2ZIP$zipcode, sd, na.rm = TRUE)
  
  Tmax_zip_avg = data.frame (ZIP = names (ZIP_avg), Date = date1, Tmax = ZIP_avg)
  ZIP_data_Tmax[[i]] = Tmax_zip_avg
  
  Tmax_zip_sd = data.frame (ZIP = names (ZIP_sd), Date = date1, Tmax_sd = ZIP_sd)
  ZIP_data_Tmax_sd[[i]] = Tmax_zip_sd  
}
 

  ### Tmin (t2_min_cor)
  ZIP_data_Tmin = vector ("list",length (date_set) )
  ZIP_data_Tmin_sd = vector ("list",length (date_set) )
  
  for (i in seq_along(date_set)) {
    
    date1 <- date_set[i]
    #EDIT
    filestr <- paste0("PATH/uhi_epi_", format(date1, "%Y%m_%d"), ".nc4")
    print(filestr)
    ncin <- nc_open(filestr)
    Tmin.array  <- ncvar_get(ncin,"t2_min_cor")
    dim(Tmin.array)
    
    #convert data into a vector, merge with ZIPtoGrid dataset, compute mean by Zip
    Tmin.vector = c(Tmin.array)
    Grid2ZIP$value = Tmin.vector[Grid2ZIP$ID]
    Grid2ZIP$value = (Grid2ZIP$value-273.15)*1.8+32 #Convert K to F
    ZIP_avg = tapply (Grid2ZIP$value, Grid2ZIP$zipcode, mean, na.rm = TRUE)
    ZIP_sd = tapply (Grid2ZIP$value, Grid2ZIP$zipcode, sd, na.rm = TRUE)
    
    Tmin_zip_avg = data.frame (ZIP = names (ZIP_avg), Date = date1, Tmin = ZIP_avg)
    ZIP_data_Tmin[[i]] = Tmin_zip_avg  
    
    Tmin_zip_sd = data.frame (ZIP = names (ZIP_sd), Date = date1, Tmin_sd = ZIP_sd)
    ZIP_data_Tmin_sd[[i]] = Tmin_zip_sd  
  }
  
####Binds rows together - 
  ZIP_data_Tmin = do.call ("rbind", ZIP_data_Tmin)
  ZIP_data_Tmax = do.call ("rbind", ZIP_data_Tmax)
   ZIP_data_Tmin_sd = do.call ("rbind", ZIP_data_Tmin_sd)
  ZIP_data_Tmax_sd = do.call ("rbind", ZIP_data_Tmax_sd)
  
####Joins columns & writes final csv file

  ZIP_allvar = list(ZIP_data_Tmin, ZIP_data_Tmax,) 
  ZIP_allvar %>% reduce(full_join, by=c("ZIP", "Date"))
  
  write.csv (ZIP_allvar, file = "temp_files/Extract_state.csv", row.names = FALSE)
  
#########################
   #Calculate Tmean
  Final <- ZIP_allvar %>% mutate(Tmean = rowMeans(across(Tmin:Tmax), na.rm = FALSE))

  #Write FINAL dataset     
  save(Final, file = "final_data/Nevada_1990_2017.Rdata")
  