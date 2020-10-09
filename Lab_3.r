library(rgdal)
library(sp)
library(raster)
library(gdalUtils)
library(foreach)
library(doSNOW)

setwd("/home/ubuntu/labs/Lab_3_Processing_Spatial_Data")

world <- readOGR("shape",layer = "ne_110m_admin_0_countries")
world_Guinea_Bissau <- subset(world, sovereignt=="Guinea Bissau")

world_Guinea_Bissau@data[,"sovereignt"]

#### Find the raster that overlays Guinea-Bissau
# Read year of green loss satellite file (tiff file)
lossyear_gb <- raster("shape/lossyear/Hansen_GFC2015_lossyear_20N_020W.tif")

#print(gdalinfo(lossyear_gb@file@name))

# We start with the raster file, which will set the extent of the map
plot(lossyear_gb, add=FALSE)
plot(world, border="red", lwd = 2, add=TRUE)
plot(world_Guinea_Bissau, border="black", lwd = 4, add=TRUE)

time1 <- proc.time() #start timer
startTimeGB <- time1
clip1_gb <- crop(lossyear_gb, extent(world_Guinea_Bissau)) #crop to extent of polygon
cat("crop 1:","\n"); proc.time() - time1

time1 <- proc.time() #start timer
clip2_gb <- rasterize(world_Guinea_Bissau, clip1_gb, mask=TRUE) #crops to polygon edge & converts to raster
cat("crop 2:","\n"); proc.time() - time1

time1 <- proc.time() #start timer
ext_gb <- getValues(clip2_gb) #much faster than extract
cat("getValues:","\n"); proc.time() - time1

time1 <- proc.time() #start timer
tab_gb <-table(ext_gb) #tabulates the values of the raster in the polygon
cat("tabulate:","\n"); proc.time() - time1

time1 <- proc.time() #start timer
mat_gb <- as.data.frame(tab_gb)
cat("as.data.frame(tab):","\n"); proc.time() - time1

timeGB <- proc.time() - startTimeGB
cat("Total time to extract raster cell values in Guinea Bissau:\n")
print(summary(timeGB))

mat_gb

#Using values from position 2 on will exclude the first row - the 0 code for no loss.
# The 0 code is much more common than any other, and would make the plot hard to read.
world_Indonesia <- subset(world, sovereignt=="Indonesia")

GBlossyears <- mat_gb$Freq[2:length(mat_gb$Freq)]
GBlosscodes <- mat_gb$ext_gb[2:length(mat_gb$ext_gb)]
#Label setting
GBlosscodes <- factor(GBlosscodes,
                      levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14"),
                      labels = c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014"))

barplot(height = GBlossyears, names.arg = GBlosscodes, xlab = "Code for Year of Green Loss", beside=TRUE)

#Create a vector of file names for the Indonesia area raster files
ind_lossyear <- c("shape/lossyear/Hansen_GFC2015_lossyear_00N_090E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_00N_100E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_00N_110E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_00N_120E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_10N_090E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_10N_100E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_10N_110E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_10N_120E.tif", 
                 "shape/lossyear/Hansen_GFC2015_lossyear_10S_120E.tif")

# create a VRT out of the input files, store in your directory
vrtpath <- "indonesia.vrt"
gdalbuildvrt(ind_lossyear, vrtpath)

IndRasters <- raster(vrtpath)

# Two ways of getting information on the rasters we've loaded:
#print(IndRasters) #print the default information for the raster object
#print(gdalinfo(IndRasters@file@name)) #print the results of gdalinfo on the filename

time1 <- proc.time() #start timer

plot(IndRasters, add=FALSE)
plot(world, border="red", lwd = 1, add=TRUE)
plot(world_Indonesia, border="black", lwd = 2, add=TRUE)

time2 <- proc.time() #end timer
cat("Plotting time for Indonesia:","\n")
summary(time2 - time1)

time1 <- proc.time() #start timer

# Get extent for country
ind_yext <- world_Indonesia@bbox["y","max"] - world_Indonesia@bbox["y","min"]
ind_xext <- world_Indonesia@bbox["x","max"] - world_Indonesia@bbox["x","min"]
cat("Indonesia longitude extent: ", ind_xext, "; latitude extent: ", ind_yext)

# Divide lat/long extents by U/V increments to create new polygons
# Here, we've manually chosen divisions, but you can imagine setting a maximum width for these
udiv = 45; vdiv = 15;

# Extent in map units for each rectangle
ind_yint <- ind_yext / vdiv
ind_xint <- ind_xext / udiv

# What is the size of each rectangle?
cat("\nSubdivision rectangle width:  ", ind_xint)
cat("\nSubdivision rectangle height: ", ind_yint)

#initialize an empty list with the length to hold all the grid cells we will create
polys <- vector("list", udiv*vdiv) #make a vector of type list, with a length of udiv * vdiv

#loop
for (u in 1:udiv){
  for (v in 1:vdiv){     
    #store the calculated extents as polygons in an indexed array
    polys[[(u-1)*vdiv+v]] <- as(extent(world_Indonesia@bbox["x","min"] + ind_xint * (u-1),
                                       world_Indonesia@bbox["x","min"] + ind_xint * u, 
                                       world_Indonesia@bbox["y","min"] + ind_yint * (v-1),
                                       world_Indonesia@bbox["y","min"] + ind_yint * v),
                                'SpatialPolygons')
  }
}

#merge polygons - this calls the bind function on each polygon to make them a single object
merged_polys <- do.call(bind, polys) 

#set the projection of these new shapes to the same as is used for our Indonesia shape
projection(merged_polys) <- projection(world_Indonesia)

cat("\nSubdivision rectangle creation time","\n"); proc.time() - time1

plot(merged_polys, col='red', axes=TRUE)
plot(world_Indonesia, col=rgb(1,1,1,0.5), add=TRUE)

#Intersected chunks with country shape
chunks_Ind <- intersect(world_Indonesia, merged_polys)
plot(chunks_Ind)

length(chunks_Ind)

shapecount <- length(chunks_Ind) # total number of chunks

endcount <- 10 # choose a number less than shapecount for testing
mat <- vector("list", endcount)

extractor <- function(p){
  clip1_Ind <- crop(IndRasters, extent(chunks_Ind[p,])) #crop to extent of polygon
  clip2_Ind <- rasterize(chunks_Ind[p,], clip1_Ind, mask=TRUE)
  ext <- getValues(clip2_Ind) #much faster than raster::extract
  #print(ext)
  tab<-table(ext) #tabulates the values of the raster in the polygon
  #print(tab)
}

starttimeInd <- proc.time() #begin time

#apply (run) the extractor function for each chunk of Indonesia, up to the endcount limit
tablesInd <- lapply(1:endcount, function(p) extractor(p)) 

dfInd <- do.call(cbind, tablesInd) #call the cbind (combine by columns) function on each table in the `tablesInd` list
endtimeInd <- proc.time() #end time

cat("Time to extract values from ", endcount, " polygons:\n")
print(summary(endtimeInd - starttimeInd))

cat("\nEstimated total time for serial extraction of all Indonesia values:\n")
cat((endtimeInd - starttimeInd)[[3]] * length(chunks_Ind) / endcount / 60, " minutes")
                    
Ts <- (endtimeInd - starttimeInd)[[3]] * length(chunks_Ind) / endcount / 60                    

#dfInd is a data frame with a column for every table from the tablesInd list.
# Sum across each row to get the frequency of each raster value
sumInd <- as.data.frame(rowSums(dfInd)) 
colnames(sumInd) <- "Freq" #assign the name Freq to the column in the data frame to be consistent with Guinea Bissau method
sumInd

shapecount <- length(chunks_Ind) # total number of chunks

endcount <- 10 # use the same number as the above serial version to measure speedup
# SET TO SHAPECOUNT to extract data for all of Indonesia
extractor <- function(p){
  clip1_Ind <- crop(IndRasters, extent(chunks_Ind[p,])) #crop to extent of polygon
  clip2_Ind <- rasterize(chunks_Ind[p,], clip1_Ind, mask=TRUE)
  ext <- getValues(clip2_Ind) #much faster than raster::extract
  tab<-table(ext) #tabulates the values of the raster in the polygon
}

cluster = makeCluster(10, type = "SOCK")
registerDoSNOW(cluster)
clusterExport(cluster, c("IndRasters","chunks_Ind", "extractor"))
starttimeInd <- proc.time() #begin time   
results = foreach(n = 1:endcount, .combine = cbind) %dopar% {
  library(raster); 
  #Define function
  extractor(n)
}
#results
endtimeInd <- proc.time() #end time
stopCluster(cluster)

cat("Time to extract values from ", endcount, " polygons:\n")
print(summary(endtimeInd - starttimeInd))
Tp <- (endtimeInd - starttimeInd)[[3]] * length(chunks_Ind) / endcount / 60   

sumInd <- as.data.frame(rowSums(results))
colnames(sumInd) <- "Freq"
sumInd

cat("Speed up improvement (%): ", round((Ts/Tp)*100, digits = 2))