#this little script may be used every time it is useful to collect MODIS data for a wide 
#amount of pixels from the Land Global Subset. The different products are:

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


#modis.coordinates <-"dec"
#modis.product <-"MOD16A2"
modis.product <-"MCD12Q1"

Export_Output <- read.csv("~/Documents/Ecohydrology/Project/AmazonConceptualBalance/41.txt", header=FALSE)
#View(Export_1590t)
modis.data <-Export_Output[,c(3,4)];
modis.data <-modis.data[, c("V4","V3")]
colnames(modis.data) <- c('lat','long')
#

if (modis.product == "MOD16A2")
  {modis.bands <- c("ET_1km", "ET_QC_1km")
  #basin ~460,000 km2. MOD16A2 gives back 1km2 data: hence each subset is going to be (110)^2 cells
  bSize <-54
}else if(modis.product == "MCD12Q1")
  {modis.bands <- c("Land_Cover_Type_1", "LC_Property_1", "Land_Cover_Type_1_Secondary")
  # TODO: MCD12Q1 gives back annual data 500m2 resolution. it should be bSize <-108; max accepted is 100.
  bSize <-100
}else
#for the study only MOD16A2 and MCD12Q1 accepted
  {print(exit <-'Exit = 1')
}

dd <- modis.tempSeries <-GetDates(Product = modis.product, Lat = modis.data$lat[1], Long = modis.data$long[1])
#by now handmade; divide the AYYYYDDD into real dates. Remember: first and last of the vector dd
modis.sDate <- 2001
modis.eDate <- 2009

modis.data$start.date <- rep(modis.sDate, nrow(modis.data))
modis.data$end.date <- rep(modis.eDate, nrow(modis.data))

MODISSubsets(LoadDat = modis.data, Products = modis.product, Bands = modis.bands, 
             Size = c(bSize,bSize), StartDate = TRUE)
