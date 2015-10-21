#project idea is to use remote sensing data to validate a simple hydrological scheme on the amazon basin. We have ET and LULC data for 15 years. The idea is to evaluate if there is some kind of unbalance in the whole water mass balance.
# first step: take land use data from modis using modis tool
library(MODISTools)
library(dplyr)
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

## make it a function so that you can run it automatically with no issues. Before that just write the arguments here for debugging
modis.sDate <- 2000
modis.eDate <- 2014
modis.coordinates <-"dec"
modis.product <-"MOD16A2"
modis.bands <- c("ET_1km", "LE_1km")

#how to format SubsetExample
# The data should be organised accordingly: "lat" and "long" columns showing WGS-1984 decimal degrees latitudes and longitudes respectively; 
# "end.date" for study end date (and optionally "start.date"), that can be in years or in POSIXlt date format; 
# "ID" containing a unique ID for each unique time-series (in time or space). Column names should be exactly as shown.

#coordinated are WGS-84, but modis likes Lat Long in decimal degrees. ConvertToDD makes the trick 
if(modis.coordinates == 'deg')
{
  modis.subset <- ConvertToDD(XY = ConvertExample, LatColName = "lat", LongColName = "long")
  modis.subset <- data.frame(lat =modis.subset[,1], long = modis.subset[,2])
} else if (modis.coordinates == 'dec')
{
  #this is what we want:
  #   lat       long
  #   51.40177 -0.6336333
  #   51.41537 -0.6462000
  #   51.40177 -0.6444000
  #   51.41287 -0.6340500
  #   51.41420 -0.6488939
  #   51.41053 -0.6421589
  #   51.41170 -0.6382400
  #   51.41327 -0.6450119
  mSubset <-list(c(51.41420,51.41053,-0.6488939,-0.6421589), ncol =2);
  colnames(mSubset, do.NULL = FALSE)
  colnames(mSubset) <- c('lat','long')
  mSubset
}
#defining iniatial and end dates of the subset extraction
#While the above techniques will work for data frames as well as matrices, there is a simpler way to refer to variables by name in a data frame, 
#namely separating the data frame’s name from the name of the variable with a dollar sign ($)
modis.subset$start.date <- rep(modis.sDate, nrow(modis.subset))
modis.subset$end.date <- rep(modis.eDate, nrow(modis.subset))


#remember that size is the amount of pixels you are taking surrounding each location
MODISSubsets(LoadDat = modis.subset, Products = modis.product, Bands = modis.bands, Size = c(0,0), StartDate = TRUE)

MODISSummaries(LoadDat = modis.subset, Product = modis.product, Bands = modis.bands(1,1), 
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001, 
               StartDate = TRUE, QualityScreen = TRUE, QualityThreshold = 0,
               QualityBand = "ET_QC_1km")

