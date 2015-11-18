#this little script may be used every time it is useful to collect MODIS data for a wide 
#amount of pixels from the Land Global Subset. The different products are:

install.packages(c("sp"))
install.packages(c("raster"))
install.packages(c("MODISTools"))
install.packages(c("geosphere"))

#to be able to use rgdal you'll need libgdal-dev (brew install gdal && proj on OSX, just sudo apt-get install on linux) 
install.packages(c("rgdal"))
install.packages(c("rgeos"))

library(rgdal) # Loads SP package by default
library(rgeos)
library("raster")
library(MODISTools)
library(dplyr)
library(geosphere)

# -------- legend of Modis products -------#

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

#--------- main arguments (tunable for different purposes) -----------#
arg.XY <- 32 #a matrix 400 pixel per product
arg.sDate <- 2000
arg.eDate <- 2000
arg.product <-"MOD16A2"
#arg.product <-"MCD12Q1"

arg.shp <- readOGR('/Users/sergiomarconi/Documents/Ecohydrology/Project/SUB_oct2/', 'sub_tapajos')
arg.mask <- raster(nrows = arg.XY, ncol = arg.XY)
#----debug --- check is the area after converision is reasonable 
#arg.mask
#arg.area <- gArea(SpatialPolygons(arg.shp@polygons)) #apparently 382680 10^6 in UTM

#---------- Extracting data form the shp, and creating the lat/long table used by modisTool to extract sub grids#
extent(arg.mask) <- extent(arg.shp)
modis.raster = rasterize(arg.shp, arg.mask, 'ID')  # pop is a real class name
modis.raster
plot(modis.raster)
lines(arg.shp)
#calculating the average subset x and y dimention in km
modis.X <- distGeo(c(xmin(modis.raster), ymin(modis.raster)), c(xmax(modis.raster), ymin(modis.raster)))/(1000*modis.raster@nrows)
modis.Y <- distGeo(c(xmin(modis.raster), ymin(modis.raster)), c(xmin(modis.raster), ymax(modis.raster)))/(1000*modis.raster@ncols)

#creating the table
modis.data <- rasterToPoints(modis.raster)
modis.data <- modis.data[,-3]
colnames(modis.data) <- c('long','lat')
modis.data <-modis.data[, c("lat","long")]
modis.data <- data.frame(lat = modis.data[ ,1], long = modis.data[ ,2])
length(modis.data[,1])
modis.data

if (arg.product == "MOD16A2")
{arg.bands <- c("ET_1km", "ET_QC_1km")
#since we will have pixels 1km2 wide, but raster returns non squared pixels, use the reprojected raster to define the size of a pixel
#remember taht MODIStool asks for n pixels east and m north 
modis.X <- as.integer((modis.X-1)/2)
modis.Y <- as.integer((modis.Y-1)/2)

}else if(arg.product == "MCD12Q1")
{arg.bands <- c("Land_Cover_Type_1", "LC_Property_1", "Land_Cover_Type_1_Secondary")
#since we will have pixels 0.25km2 wide, but raster returns non squared pixels, use the reprojected raster to define the size of a pixel
#remember taht MODIStool asks for n pixels east and m north: since every  
modis.X <- as.integer(modis.X-1)
modis.Y <- as.integer(modis.Y-1)

}else
  
  #for the study only MOD16A2 and MCD12Q1 accepted
{print(exit <-'Exit = 1')
}


#------------------------ Data extraction ------------------------#
modis.data$start.date <- rep(arg.sDate, nrow(modis.data))
modis.data$end.date <- rep(arg.eDate, nrow(modis.data))


#remember that size is the amount of pixels you are taking surrounding each location
MODISSubsets(LoadDat = modis.data, Products = arg.product, Bands = arg.bands, Size = c(modis.X, modis.Y), StartDate = TRUE)

MODISSummaries(LoadDat = modis.data, Product = arg.product, Bands = arg.bands[1], 
               ValidRange = c(-32767,32700), NoDataFill = -32767, ScaleFactor = 0.1, 
               StartDate = TRUE, QualityScreen = FALSE, QualityThreshold = 0,
               QualityBand = "ET_QC_1km")

#formatting evapotraspiration data into annual data matrices
#the table has float points, data only 5 digits after the comma
#prova <-paste("LAT", format(round(modis.data[2,1], 5), nsmall = 5),"LON", format(round(modis.data[2,2], 5), nsmall = 5), "Start",modis.data[2,3], "-01-01End", modis.data[2,4], "-12-31___", arg.product, ".asc", sep = "")

#allocate matrix check how can automate 745
aveET <- matrix(data = NA, nrow = length(modis.data[,1]), ncol = 745)
#percentage of missing info regarding the specific date Check with Trey
alpha <- 8


for (i in 1:length(modis.data[,1])) {
  
  aveET[i,1] <- arg.sDate
  #lat
  aveET[i,2] <- format(round(modis.data[i,1], 5), nsmall = 5)
  #long
  aveET[i,3] <- format(round(modis.data[i,2], 5), nsmall = 5)
  
  tempET <-read.csv(paste(getwd(), "/","LAT", format(round(modis.data[i,1], 5), nsmall = 5),"LON", 
                          format(round(modis.data[i,2], 5), nsmall = 5), "Start",modis.data[i,3], 
                          "-01-01End", modis.data[i,4], "-12-31___", arg.product, ".asc", sep = ""), header=FALSE)
  tempQC <-tempET[(1 + length(tempET[,1])/2):(length(tempET[,1])),]
  tempET <- tempET[1:(length(tempET[,1])/2),]
  for (j in 11: length(tempET[1,])){
    aveET[i,(j-7)] <- mean(tempET[,j])
  }
}
aveET <- aveET[,1:738]

tabET <- matrix(data = arg.sDate, nrow = (511 *735), ncol = 4)
#aveET <-data.matrix(aveET)
token <-1
for(i in 1:511)
{
  pX <- - modis.X
  pY <- modis.Y
  for (j in 4:738){
    tabET[token, 2] <- as.numeric(aveET[i,2]) + pX -.500
    tabET[token, 3] <- as.numeric(aveET[i,3]) + pY +.500
    tabET[token, 4] <- as.numeric(aveET[i,j])
    token <-token +1
    pX <- pX +1
    if (pX > modis.X){
      pY <- pY -1
      pX <- - modis.X
    }
    if (pY < - modis.Y)
    {
      if(pX > modis.X)
      {
        print("Warning! for out of loop")
      }
    }
  }
}

