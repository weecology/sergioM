library(maptools)
#worldclim maps and "maybe" meteo maps
library(dismo)

data(wrld_simpl)
plot(wrld_simpl, xlim=c(-130,-60), ylim=c(25,50), axes=TRUE,col="light yellow")
plot(wrld_simpl, xlim=c(-130,60), ylim=c(-25,50), axes=TRUE,col="light yellow")

box()

#pick data we have traits of
points(plyr1$LONGITUDE, plyr1$LATITUDE, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(plyr1$LONGITUDE, plyr1$LATITUDE, col='red', cex=0.75)

"we want to use the randomForest algorithm to spatially interpolate 
the occurrence of the different traits: let's start with MAP and MAT"
w = getData('worldclim', var='tmean', res=0.5, lon=c(-90), lat=c(35)) #lon=5, lat=45)
plot(w$tmin1_13)
climate <- getData('worldclim', var='bio', res=2.5)
plot(climate$bio1)
newext <- c(-130, -60, 25, 50)
tmin1.c <- crop(climate$bio1, newext)
plot(tmin1.c)

altim = getData('alt', country='USA')
plot(w$`/Users/sergiomarconi/Documents/Data/NACP_TERRA-PNW_1292/USA1_msk_alt.grd`)
?getData
