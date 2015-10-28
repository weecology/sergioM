#this little script may be used every time it is useful to collect MODIS data for a wide 
#amount of pixels from the Land Global Subset. The different products are:

# #"MCD12Q1"    LAnd Cover type product >>500m
# "MCD12Q2"     Land Cover Dynamics >> 500m
# "MCD43A1"     Modis Terra Aqua Albedo  
# "MCD43A2"     BRDF-Albedo Quality 16-Day L3 Global 500m
# "MCD43A4"     Adjusted reflectance 16 days
# "MOD09A1"     Surface Reflectance
# "MOD11A2"     Land Surface Temperature & Emissivity
# "MOD13Q1"     NDVI, EVI
# "MOD15A2"     LAI and FPAR
# "MOD15A2GFS"  Smoothed LAI (Gapfilled)
# "MOD16A2"     ET
# "MOD17A2_51"  GPP
# "MOD17A3"     NPP
# "MYD09A1"     Surface Reflectance
# "MYD11A2"     LST/Emissivity
# "MYD13Q1"     NDVI/EVI
# "MYD15A2"     LAI and FPAR

library(MODISTools)
library(dplyr)

#this case is for the Tapajos basin. The csv file was extracted from a polygon, 0.18 degrees to raster (~1590 pixels). 
#Pixels coordinates were sampled from the produced raster. 
Export_1590t <- read.csv("~/Documents/Ecohydrology/Export_1590t.txt", header=FALSE)
#View(Export_1590t)
modis.data <-Export_1590t[,c(3,4)];
#Since long and lat were inverted in my poligon to coordinates extraction, I had to invert the order of the columns
modis.data <-modis.data[, c("V4","V3")]
colnames(modis.data) <- c('lat','long')
modis.sDate <- 2000
modis.eDate <- 2000
modis.coordinates <-"dec"
modis.product <-"MOD16A2"
modis.bands <- c("ET_1km")
#modis.subset <-modis.subset[1,]
modis.data$start.date <- rep(modis.sDate, nrow(modis.data))
modis.data$end.date <- rep(modis.eDate, nrow(modis.data))
#the basin counts for ~480,000 km2. ModisTool works faster collecting less input pixels with wide around, than the opposite
# the size is a 9x9, which means a 19x19 matrix will be extracted around the selected point, for each point.
MODISSubsets(LoadDat = modis.data, Products = modis.product, Bands = modis.bands, Size = c(9,9), StartDate = TRUE)
